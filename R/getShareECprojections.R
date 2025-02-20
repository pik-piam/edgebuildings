#' Projected final energy share of carriers for each end use
#'
#' Project the final energy shares of carriers (EC) for each end use (EU) based
#' on assumed final shares.There are tuples of (EU, EC) that are phased out once
#' a particular threshold of GDP per capita is reached (energy ladder). The
#' remaining share is then split between the other carriers with their relative
#' shares approaching assumed shares. The speed of approaching final values is
#' determined by the maximum of a temporal and a GDP-driven convergence. The
#' temporal convergences is reached in 2150. The GDP-driven convergence is
#' reached, once GDP per capita is 40 times higher than it was at the end of
#' history (EOH).
#'
#' @param config scenario-wise parameter configuration
#' @param fe data.frame final energy data
#' @param hddcdd historical and future HDD's/CDD's
#' @param gdp data.frame gdp data
#' @param gdppop data.frame gdp per capita
#' @param scenAssumpFEShares data.frame scenario-specific FE sare assumptions
#' @param regionalmap data.frame regional mapping
#'
#' @returns data.frame
#'
#'
#' @note for the variables without projections based on gdppop, use the
#' scenario assumptions from scenAssumpFEShares. Here we assume that the
#' share fully converge to the scenario assumption when the gdp reaches 10 times
#' the getOption("end_of_history") amount. This is an ad hoc assumption. The
#' shares depend on the development of the size of the economy. Further, a time
#' component has been introduced to allow rich countries to change their shares
#' as well. Time convergence by 2150.
#'
#' @note Compute the projected rest: The gdppop based projections determined
#' what is not fueled with the projections (a - sum(projectShares)).
#' if biotrad cooking = 0.75, coal cooking = 0.02 and petrol cooking = 0.05,
#' the rest is 0.18
#' if the shares projected in projections_non.gdppop give natgas cooking = elec
#' cooking = 0.5, we will have natgas cooking = elec cooking = 0.5 * 0.18 = 0.09
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom dplyr mutate filter across all_of rename summarise group_by
#'   ungroup left_join right_join matches %>% .data
#' @importFrom tidyr spread gather unite replace_na pivot_longer
#' @importFrom quitte interpolate_missing_periods getRegs
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point ggtitle facet_wrap
#'   expand_limits

getShareECprojections <- function(config,
                                  fe,
                                  hddcdd,
                                  gdp,
                                  gdppop,
                                  scenAssumpFEShares,
                                  regionalmap) {
  # FUNCTIONS ------------------------------------------------------------------

  # normalise at carrier level
  normalise <- function(df) {
    colGroup <- setdiff(colnames(df), c("carrier", "value"))
    df %>%
      group_by(across(all_of(colGroup))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(value = replace_na(.data[["value"]], 0))
    return(df)
  }


  # PARAMETERS -----------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>% unique()

  # FE share convergence target year
  feShareSpeed <- config[scen, "speed_feShare"] %>%
    as.numeric()

  # upper temporal threshold of historic data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()

  # temporal projection range
  years <- c(1960:endOfHistory, seq(endOfHistory + 5, 2100, 5))

  refScen <- "SSP2"

  # tuples of carrier and end use that are phased out with gdppop
  useCarrierPhaseOut <- c("space_heating.biotrad",
                          "cooking.biotrad",
                          "water_heating.biotrad",
                          "appliances_light.petrol",
                          "water_heating.coal",
                          "space_heating.coal",
                          "cooking.coal")

  # tuples of carrier and end use that are inert:
  # Their shares cannot decrease and growth is constraint in case of very low historic values
  # TODO: make this selection dynamic #nolint
  useCarrierInert <- c(
    "space_heating.heat",
    "water_heating.heat",
    "space_cooling.heat"
  )

  # tuples of carrier and end use that involve district heat
  # Their target shares is kept constant in regions with low HDD
  useCarrierDh <- c(
    "space_heating.heat",
    "water_heating.heat"
  )

  # Threshold until which inert tuples are considered to have very low historic values
  thresInert <- config[[scen, "thresInert"]]

  # Threshold of hdd until which district heating does not grow in share
  thresDh <- config[[scen, "thresDh"]]

  # multiple of gdppop needed to reach for gdp-driven convergence (w.r.t. EOH)
  fullConvergenceLevel <- 40



  # PRE-PROCESS DATA ---------------------------------------------------------------

  # final/useful energy data
  fe <- fe %>%
    select(-"variable") %>%
    rename(variable = "enduse") %>%
    select("region", "period", "scenario", "unit", "carrier", "variable", "value")

  # Compute last time period with historic data
  # Only works if temporal scope is the same for all carrier-variable combinations
  endOfData <- max(fe[["period"]][!is.na(fe[["value"]])])
  # TODO: This is required for the start of the phase-out - maybe do this explicitly

  # temporal convergence of shares
  lambda <- calcLambda(endOfData, feShareSpeed, 1, type = "linear", step = 1)

  #--- Data is made compliant with config file

  # gdppop
  gdppop <- gdppop %>%
    filter(.data[["scenario"]] == config[, "gdppopScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    filter(.data[["period"]] %in% years) %>%
    sepHistScen(endOfHistory = endOfHistory) %>%
    select("region", "period", "scenario", "unit", "variable", "value")


  # gdp
  gdp <- gdp %>%
    filter(.data[["scenario"]] == config[, "gdpScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    filter(.data[["period"]] %in% years) %>%
    sepHistScen(endOfHistory = endOfHistory) %>%
    mutate(unit = NA)

  # hdd
  hdd <- hddcdd %>%
    filter(.data[["variable"]] == "HDD", .data[["scenario"]] == config[, "hddcddScen"],
           .data[["period"]] == endOfHistory) %>%
    select(-"variable", -"period", -"scenario") %>%
    rename(hdd = "value")


  # gdppop threshold for EC share convergence
  incomeThresholdEC <- config[scen, "incomeThresholdEC"] %>%
    as.numeric() %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionalmap,
                   valueOnly = TRUE)


  # scenario parameter assumptions for EC share convergence
  refAssumpFEShares <- scenAssumpFEShares %>%
    filter(.data[["scenario"]] == refScen) %>%
    mutate(obj_shareRef = .data[["obj_share"]]) %>%
    select(-"scenario") %>%
    tidyr::crossing(scenario = c("history", refScen))

  scenAssumpFEShares <- scenAssumpFEShares %>%
    filter(.data[["scenario"]] == scen) %>%
    select(-"scenario") %>%
    tidyr::crossing(scenario = c("history", scen))



  # PROCESS DATA ---------------------------------------------------------------

  fillYears <- years[which(years %in% min(fe$period):endOfHistory)]

  # extrapolate historic values, replace NAs in RUS and AFR
  fe <- fe %>%
    interpolate_missing_periods(fillYears, expand.values = TRUE) %>%
    group_by(across(all_of(c("period", "variable", "region")))) %>%
    mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE),
           unit = NA) %>%
    ungroup() %>%
    mutate(value = ifelse(.data[["variable"]] == "space_cooling" & is.na(.data[["value"]]),
                          ifelse(.data[["carrier"]] == "elec",
                                 1,
                                 0),
                          .data[["value"]])) %>%
    unite(col = "variable", c("variable", "carrier"), sep = ".") %>%
    rbind(gdppop, gdp)


  ## tuples (EU, EC) to phase out ====

  # project phase out with increasing gdppop
  fePhaseOut <- filter(fe, .data[["period"]] <= endOfData | .data[["variable"]] %in% c("gdp", "gdppop"))
  for (useCarrier in useCarrierPhaseOut) {
    fePhaseOut <- projectShares(df = fePhaseOut,
                                var = useCarrier,
                                xTar = incomeThresholdEC,
                                yTar = 0.01,
                                phaseOutMaxEnd = feShareSpeed,
                                phaseOutStart = endOfData)
  }


  # save the projectShares and split the variable columns
  fePhaseOut <- fePhaseOut %>%
    filter(.data[["variable"]] %in% useCarrierPhaseOut) %>%
    separate(col = "variable", into = c("enduse", "carrier"), sep = "\\.")


  ## tuples converging final shares ====

  # shares of (end-use, carrier) tuples that converge towards assumed values
  feConverge <- fe %>%
    filter(!(.data[["variable"]] %in% c("gdp", "gdppop")),
           !.data[["variable"]] %in% useCarrierPhaseOut) %>%
    separate(col = "variable", into = c("enduse", "carrier"), sep = "\\.") %>%
    normalise() %>%
    unite(col = "variable", c("enduse", "carrier"), sep = ".") %>%
    rbind(filter(fe, .data[["variable"]] %in% c("gdp", "gdppop"))) %>%
    spread("variable", "value") %>%
    gather("variable", "value", matches("[_.]")) %>%
    left_join(hdd, by = "region")

  refConverge <- convergeFEShares(feConverge, refAssumpFEShares, lambda, endOfData, endOfHistory, fullConvergenceLevel,
                                  useCarrierInert, thresInert, useCarrierDh, thresDh)

  feConverge <- convergeFEShares(refConverge, scenAssumpFEShares, lambda, endOfHistory, feShareSpeed,
                                 fullConvergenceLevel, useCarrierInert, thresInert, useCarrierDh, thresDh) %>%
    select("region", "period", "scenario", "unit", "variable", "value") %>%
    rbind(feConverge %>%
            filter(.data[["variable"]] == "gdp") %>%
            select("region", "period", "scenario", "unit", "variable", "value"))


  # variables for which there are already projections
  varsProjected <- feConverge %>%
    filter(.data[["period"]] == 2100,
           .data[["variable"]] != "gdppop") %>%
    getElement("variable") %>%
    unique()

  # set variables without projections (gdppop-driven or temporal) to zero
  feConverge <- feConverge %>%
    filter(!(.data[["variable"]] %in% varsProjected)) %>%
    spread("variable", "value") %>%
    gather("variable", "value", matches("[_.]")) %>%
    mutate(value = ifelse(.data[["scenario"]] != "history",
                          0, .data[["value"]])) %>%
    rbind(filter(feConverge, .data[["variable"]] %in% c("gdppop", varsProjected))) %>%
    separate(col = "variable", into = c("enduse", "carrier"), sep = "\\.")

  # re-normalize converged shares
  feConverge <- feConverge %>%
    group_by(across(-all_of(c("carrier", "value")))) %>%
    mutate(value = proportions(.data[["value"]]),
           value = replace_na(.data[["value"]], 0)) %>%
    ungroup()


  ## remaining tuples ====

  # rest as sum of what remains after phase out
  feRest <- fePhaseOut %>%
    group_by(across(-all_of(c("carrier", "value")))) %>%
    reframe(rest = 1 - sum(.data[["value"]]))

  # multiplied the shares computed with the assumptions by the rest
  feConverge <- feConverge %>%
    left_join(feRest, by = c("region", "period", "scenario", "unit", "enduse")) %>%
    mutate(value = ifelse(!is.na(.data[["rest"]]),
                          .data[["value"]] * .data[["rest"]],
                          .data[["value"]])) %>%
    select(-"rest")

  fe <- rbind(fePhaseOut, feConverge)


  # OUTPUT----------------------------------------------------------------------

  data <- fe %>%
    mutate(scenario = scen) %>%
    select(-"unit")

  return(data)
}



# INTERNAL FUNCTIONS -----------------------------------------------------------


#' Project energy carrier (EC) shares to pre-set target value
#'
#' Project the share of specific demand w.r.t. absolute FE for individual energy
#' carrier/enduse combinations to a pre-defined value target in a pre-defined
#' temporal period.
#'
#' @param df data.frame with historic EC shares and gdp per capita
#' @param var carrier/enduse combination to be phased out
#' @param xTar region-wise target values of carrier FE share
#' @param yTar year of full convergence
#' @param phaseOutMaxEnd upper temporal boundary of phaseout time period
#' @param phaseOutStart starting point of phaseOut
#'
#' @return projected EC shares
#'
#' @importFrom utils tail
#' @importFrom dplyr %>% .data filter rename select group_by arrange mutate
#' @importFrom tidyr pivot_wider gather
#' @importFrom quitte getRegs

# income elasticity to reach (xTar, yTar) starting from 2010 (gdppop, share)

projectShares <- function(df, var, xTar, yTar, phaseOutMaxEnd, phaseOutStart) {

  if (!(var %in% unique(df$variable))) {
    simpleError(paste("Variable ", var, " not included in passed dataset."))
  }

  # filter data
  data <- df %>%
    filter(.data[["variable"]] %in% c(var, "gdppop"))

  # extract regions with last historic value above the target value
  regions <- data %>%
    filter(.data[["variable"]] == var,
           .data[["value"]] > yTar,
           .data[["period"]] == phaseOutStart) %>%
    getRegs()

  # spread variables
  data <- data %>%
    pivot_wider(names_from = "variable", values_from = "value")

  # Prepare the data
  outliers <- setdiff(getRegs(df), regions)
  data <- data %>%
    rename("pred" = var)
  dataOuter <- data %>%
    filter(.data[["region"]] %in% outliers)
  data <- data %>%
    filter(.data[["region"]] %in% regions)

  # project phaseouts
  data <- do.call(
    "rbind",
    lapply(getRegs(data), function(reg) {
      # last historical variable value
      predEndHist <- data %>%
        filter(.data[["region"]] == reg,
               .data[["period"]] == phaseOutStart) %>%
        select("pred") %>%
        as.numeric()

      # last historical gdppop value
      gdppopEndHist <- data %>%
        filter(.data[["region"]] == reg,
               .data[["period"]] == phaseOutStart) %>%
        select("gdppop") %>%
        as.numeric()

      # data of interest
      dataRegScen <- data %>%
        filter(.data[["region"]] == reg,
               .data[["period"]] > phaseOutStart)

      # weight (1 is the linear case)
      power <- 1

      # nolint start
      # reach yTar once gdppop reaches threshold
      thresholdPhaseOut <- pmax(
        yTar,
        predEndHist + (yTar - predEndHist) *
          (pmax(0, dataRegScen$gdppop - gdppopEndHist) /
             (xTar[[reg, "value"]] - gdppopEndHist))^power)
      # nolint end

      # max phase out: reaches 0 when gdppop is 1.5 gdppop(eoh)
      maxPhaseOut <- predEndHist *
        pmin(1, pmax(0, 3 - 2 * dataRegScen$gdppop / gdppopEndHist))

      # forced phase out until target year
      forcedPhaseOut <- predEndHist *
        pmin(1, pmax(0, 1 - (dataRegScen$period - phaseOutStart) / (phaseOutMaxEnd - phaseOutStart)))


      if (nrow(dataRegScen) > 0) {
        if (gdppopEndHist < xTar[[reg, "value"]] & predEndHist > yTar) {
          # hist share higher than target
          dataRegScen$pred <- pmax(maxPhaseOut, thresholdPhaseOut)
        }
        if (gdppopEndHist < xTar[[reg, "value"]] & predEndHist <= yTar) {
          # hist share already below target -> cap share to yTar
          dataRegScen$pred <- pmin(yTar, thresholdPhaseOut)
        }
        if (gdppopEndHist >= xTar[[reg, "value"]]) {
          # hist gdppop already above threshold -> max phase out
          dataRegScen$pred <- maxPhaseOut
        }
        if (!is.null(phaseOutMaxEnd)) {
          # consider forced phaseout
          dataRegScen$pred <- pmin(forcedPhaseOut, dataRegScen$pred)
        }
      }
      return(dataRegScen)
    }
    )
  )

  # fill outliers with last historical value
  dataOuter <- dataOuter %>%
    group_by(across(all_of("region"))) %>%
    arrange(.data[["period"]]) %>%
    mutate(pred = ifelse(.data[["period"]] > phaseOutStart,
                         tail(.data[["pred"]][!is.na(.data[["pred"]])], 1),
                         .data[["pred"]]))  %>%
    filter(.data[["period"]] > phaseOutStart) %>%
    as.data.frame()

  # bind data and filter for non-historical values
  df <- rbind(data, dataOuter) %>%
    rename(value = "pred") %>%
    select(any_of(colnames(df))) %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(variable = var) %>%
    anti_join(df, by = c("region", "period", "scenario", "unit", "variable")) %>%
    rbind(df)

  return(df)
}

#' Compute convergence towards target shares within a given time interval
#'
#' @param feConverge data frame with historic shares, gdp and hdd
#' @param assumpFEShares data frame with FE target shares
#' @param lambda numeric, transition path for convergence
#' @param thist numeric, last historic year, i.e. the year from which convergence is computed
#' @param tmax numeric, last time period for which shares are to be projected
#' @param fullConvergenceLevel numeric, ratio of GDP to GDP in \code{thist}
#'   at which full convergence should be reached
#' @param useCarrierInert character, end use-carrier combinations for which target shares
#'   are halved if historic shares are low, i.e. below \code{thresInert}
#' @param thresInert numeric, threshold for historic shares below which target shares
#'   of \code{useCarrierInert} are halved
#' @param useCarrierDh character, end use-carrier combinations for which the share remains constant
#'   if heating degree days \code{hdd} are low, i.e. below \code{thresDh}
#' @param thresDh numeric, threshold for hdd below which target shares of \code{useCarrierDh}
#'   are kept at historic levels
#'
#' @importFrom dplyr %>% .data across all_of group_by left_join mutate select ungroup
#'
convergeFEShares <- function(feConverge, assumpFEShares, lambda, thist, tmax, fullConvergenceLevel,
                             useCarrierInert, thresInert, useCarrierDh, thresDh) {

  useCarrierRescale <- union(useCarrierInert, useCarrierDh)

  # transition towards assumed shares with max of temporal and GDP-driven speed
  feConverge <- feConverge %>%
    group_by(across(all_of("region"))) %>%
    mutate(gdpRatio = .data[["gdp"]] /
             .data[["gdp"]][.data[["period"]] == thist]) %>%
    group_by(across(all_of(c("region", "variable")))) %>%
    mutate(shareEndHist = .data[["value"]][.data[["period"]] == thist]) %>%
    ungroup()


  # add target shares from scenario assumptions, make adjustments
  feConvergeTmp <- feConverge %>%
    left_join(assumpFEShares, by = c("region", "scenario", "variable"))
  feConvergeTmp <- feConvergeTmp %>%
    mutate(enduse = gsub(".[a-z]*$", "", .data[["variable"]]),
           obj_share = ifelse(
             .data[["variable"]] %in% useCarrierInert,
             pmax(.data[["obj_share"]] * ifelse(.data[["shareEndHist"]] <= thresInert, 0.5, 1),
                  .data[["shareEndHist"]]),
             .data[["obj_share"]]
           )) %>%
    mutate(obj_share = ifelse(
      .data[["variable"]] %in% useCarrierDh & .data[["hdd"]] < thresDh,
      .data[["shareEndHist"]],
      .data[["obj_share"]]
    )) %>%
    group_by(across(all_of(c("scenario", "period", "region", "enduse")))) %>%
    mutate(across(all_of(c("obj_share", "shareEndHist")), ~ .x *
                    ifelse(.data[["variable"]] %in% useCarrierRescale
                           | sum(.x[!.data[["variable"]] %in% useCarrierRescale]) == 0,
                           1,
                           (1 - sum(.x[.data[["variable"]] %in% useCarrierRescale])) /
                             sum(.x[!.data[["variable"]] %in% useCarrierRescale])))) %>%

    # Compute projections of carrier shares
    mutate(lambdaEff = pmin(1, pmax(lambda[as.character(.data[["period"]])],
                                    (.data[["gdpRatio"]] - 1) /
                                      (fullConvergenceLevel - 1))),
           value = ifelse(
             .data[["period"]] <= tmax & .data[["period"]] > thist,
             .data[["lambdaEff"]] * .data[["obj_share"]] +
               (1 - .data[["lambdaEff"]]) * .data[["shareEndHist"]],
             .data[["value"]])
           ) %>%
    ungroup() %>%
    select("region", "period", "scenario", "unit", "variable", "gdp", "hdd", "value")
}
