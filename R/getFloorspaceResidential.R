#' Residential floorspace demand projections
#'
#' Takes historic floorspace demand, income projections, population density
#' projections and population projections and projects residential floorspace
#' demand at country level.
#'
#' @param config scenario-wise parameter configuration
#' @param floorspacePast data.frame historic floorspace demand
#' @param gdppop data.frame gdp per capita
#' @param density data.frame population density
#' @param pop data.frame population
#' @param surface data.frame with region surface area
#' @param regionmap regional mapping
#' @param scenAssump data.frame scenario-specific assumptions
#' @param scenAssumpSpeed data.frame scenario-specific temporal upper boundary
#'
#' @returns data.frame historic and scenario-specific projected residential
#' floorspace in million m2 and floorspace per capita in m2/cap on regional level
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte character.data.frame replace_column overwrite getScenarios
#' getPeriods removeColNa
#' @importFrom dplyr anti_join select select_ filter mutate one_of left_join
#'   group_by min_rank desc arrange lag ungroup bind_rows across all_of %>%
#'   .data
#' @importFrom tidyr spread gather
#' @importFrom dplyr left_join group_by mutate select ungroup rename filter %>%
#'   .data bind_rows
#' @importFrom purrr is_null
#' @importFrom stats coef lm

getFloorspaceResidential <- function(config,
                                     floorspacePast,
                                     gdppop,
                                     density,
                                     pop,
                                     surface,
                                     regionmap,
                                     scenAssump,
                                     scenAssumpSpeed) {
  # FUNCTIONS-------------------------------------------------------------------

  # calculate global population density
  globDensity <- function(surface, density) {
    left_join(density %>% spread("variable", "value"),
              surface %>% spread("variable", "value"),
              by = "region") %>%
      gather(key = "variable", value = "value", one_of("density", "surface")) %>%
      calcGlob("density", weights = "surface")
  }


  # calculate difference between expected and calculated mscap values
  addDelta <- function(df) {
    df <- df %>%
      group_by(across(all_of(c("region")))) %>%
      mutate(ord   = min_rank(desc(.data[["period"]])),
             delta = log(.data[["m2cap"]][.data[["ord"]] == 1]) - log(.data[["m2caphat"]][.data[["ord"]] == 1]),
             gdppopDelta = .data$gdppop[.data[["ord"]] == 1]) %>%
      select(-"ord") %>%
      ungroup()
    return(df)
  }


  # PARAMETERS------------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>% unique()


  # upper floorspace limit
  scenAssumpCap <- config[scen, "floorspaceCap"]

  # upper temporal threshold of historic data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()



  # READ-IN DATA----------------------------------------------------------------

  if (scenAssumpCap != "NULL") {
    scenAssumpCap <- scenAssumpCap %>%
      buildScenInput(subtype = "mapping",
                     regionmap = regionmap) %>%
      rename(capFloor = "value")
  }


  # PRE-PROCESS DATA------------------------------------------------------------

  #--- Data is made compliant with config file

  # gdppop
  gdppop <- gdppop %>%
    filter(.data[["scenario"]] == config[scen, "gdppopScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    missingToNA()

  # pop
  pop <- pop %>%
    filter(.data[["scenario"]] == config[scen, "popScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    missingToNA()

  # density
  density <- density %>%
    filter(.data[["scenario"]] == config[scen, "densityScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    missingToNA()


  # scenario parameter assumptions
  scenAssump <- scenAssump %>%
    filter(.data[["scenario"]] == scen) %>%
    select("region", "scenario", "floorspace")


  # scenario convergence speed assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)


  # historical floorspace
  floorspacePast <- floorspacePast %>%
    mutate(scenario = scen)


  # create the lambda vector
  lambda <- compLambdaScen(scenAssumpSpeed, startYearVector = 1960)



  # PROCESS DATA----------------------------------------------------------------

  # global population density
  densityGLO <- globDensity(surface, density)


  # introduce all the necessary information in the hh data.frame for the regression and spread
  hh <- bind_rows(floorspacePast, gdppop, density, pop) %>%
    spread("variable", "value") %>%
    filter(!is.na(.data[["gdppop"]]))


  # TODO: transition the lower part towards a regional regression #nolint

  # for the estimation, only take the rows with data for floorspace per capita
  hhEstimate <- filter(hh, !is.na(.data[["m2cap"]]))

  # create the estimate object from the regression
  estimate <- lm("log(m2cap) ~  log(gdppop) + log(density)", data = hhEstimate)  # 0.42 for gdppop

  # withdraw the coeffient informations. We do not use predict for the out-of-sample data,
  # because of the way of dealing with discrepancies between estimation and actual data
  intercept      <- coef(summary(estimate))["(Intercept)", "Estimate"]
  coefLogGdp     <- coef(summary(estimate))["log(gdppop)", "Estimate"]
  coefLogDensity <- coef(summary(estimate))["log(density)", "Estimate"]

  # make the estimation for m2cap
  hhEstimate$m2caphat <- exp(predict(estimate, newdata = hhEstimate))

  # compute the country-specfic delta from the estimate, which will give the change in the constant
  # delta is computed for the last period of each region
  hhEstimate <- addDelta(hhEstimate)

  # take the data set with all countries and overwrite with the values in hhEstimate if existing
  hhFullReg <- completeHH(hhEstimate, hh, endOfHistory)

  # make the prediction with the convergence of lambda towards 0 in 2200 in a logit fashion
  hhFull <- predictFullDelta(hhFullReg, coefLogGdp, coefLogDensity, intercept,
                             scenAssump, lambda, densityGLO, endOfHistory)

  # cap floorspace projections if required by scenario assumptions
  if (!is_null(scenAssumpCap) && is.data.frame(scenAssumpCap)) {
    hhFull <- capFloorProjections(hhFull, scenAssumpCap, lambda, endOfHistory)
  }

  # calculate full aggregated floorspace
  hhFull <- hhFull %>%
    mutate(m2 = .data[["m2hatConv"]] * .data[["pop"]]) %>%
    select("model", "scenario", "region", "unit", "period", "m2hatConv", "m2") %>%
    rename("m2cap" = "m2hatConv") %>%
    gather("variable", "value", "m2cap":"m2") %>%
    as.quitte() %>%
    missingToNA()

  return(hhFull)
}


# INTERNAL FUNCTIONS -----------------------------------------------------------


#' Complete historical data with countries not present in \code{dfPartial}
#'
#' Fill data points and set future data/projection-deviation, \code{delta}, to
#' the last historical value.
#'
#' @param dfPartial historic data.frame on floorspace, population, floorspace per capita and population density
#' @param dfComplete data.frame with projected values
#' @param endOfHistory upper temporal boundary of historical data
#'
#' @return complete data.frame
#'
#' @importFrom quitte getPeriods
#' @importFrom dplyr group_by mutate select ungroup across all_of

completeHH <- function(dfPartial, dfComplete, endOfHistory) {
  dfScen <- dfComplete %>%
    filter(!(.data[["period"]] %in% getPeriods(dfPartial)))

  # complete the hist df with countries not present in dfPartial
  # add the prediction for the countries outside the dfPartial
  # bind with the scenario df
  # in the scenario part of the df, set delta to the last historical value
  tmp <- dfPartial %>%
    group_by(across(all_of(c("region")))) %>%
    mutate(delta = ifelse(length(.data[["delta"]][!is.na(.data[["delta"]])]) > 0,
                          unique(.data[["delta"]]),
                          0),
           gdppopDelta = ifelse(length(.data[["delta"]][!is.na(.data[["delta"]])]) > 0,
                                unique(.data[["gdppopDelta"]]),
                                NA),
           delta = ifelse(.data[["period"]] <= max(.data[["period"]][!is.na(.data$m2cap)]),
                          .data[["delta"]] * .data[["gdppop"]] / .data[["gdppopDelta"]],
                          .data[["delta"]])) %>%
    ungroup() %>%
    select(-"gdppopDelta") %>%
    rbind(dfScen %>%
            mutate(m2caphat = NA, delta = NA)) %>%
    group_by(across(all_of("region"))) %>%
    mutate(delta = ifelse(.data[["period"]] > endOfHistory,
                          .data[["delta"]][.data[["period"]] == endOfHistory],
                          .data[["delta"]]))
  return(tmp)
}


#' Calculate full residential floorspace prediction
#'
#' First, missing historic data is filled in with delta-corrected values derived
#' from regression parameters. Partly, these are replaced with correction/target
#' values from \code{scenAssump} and the converged w.r.t. to a target period.
#'
#' As the income elasticity will evolve over time according to scenario assumptions,
#' the elasticity at a certain point in time will only influence the incremental
#' floor space demand. Therefore a stepwise calculation of future floor space demand
#' is used which is based on the value in the previous time step.
#'
#' @param df full dataset containing predicted floorspace values
#' @param coefLogGdpEst logarithmic income elasticity parameter
#' @param coefLogDensityEst logarithmic pop density elasticity parameter
#' @param interceptEst logarithmic intercept parameter
#' @param scenAssump parameter scenario assumptions
#' @param lambda temporal convergence factors
#' @param densityGlobal global population density
#' @param endOfHistory upper temporal boundary of historical data
#'
#' @importFrom dplyr filter left_join mutate select arrange group_by ungroup across all_of
#' @importFrom quitte getPeriods overwrite as.quitte removeColNa
#' @importFrom tidyr gather
#'
predictFullDelta <- function(df,
                             coefLogGdpEst,
                             coefLogDensityEst,
                             interceptEst,
                             scenAssump,
                             lambda,
                             densityGlobal,
                             endOfHistory) {
  # convergence share types
  lambdaCols <- colnames(lambda[!(colnames(lambda) %in% c("region", "period", "scenario"))])

  # match periods of df and lambda and append scenario assumptions
  df <- df %>%
    filter(.data[["period"]] %in% getPeriods(lambda)) %>%
    left_join(scenAssump, by = c("region", "scenario"))

  # The floorspace column in scenAssump gives the variation in the elasticity
  # of floorspace per capita, depending upon the scenario. The parameter is corrected
  # and temporally converged to the target value.

  coefGdpCorrect <- rbind(df %>%
                            filter(.data[["period"]] <= endOfHistory) %>%
                            mutate(coefLogGdp = coefLogGdpEst,
                                   coefLogDensity = coefLogDensityEst,
                                   intercept = interceptEst),
                          df %>%
                            filter(.data[["period"]] > endOfHistory) %>%
                            left_join(lambda, by = c("region", "period", "scenario")) %>%
                            mutate(coefLogGdp = .data[["fullconv"]] * (coefLogGdpEst * .data[["floorspace"]]) +
                                     (1 - .data[["fullconv"]]) * coefLogGdpEst,
                                   coefLogDensity = coefLogDensityEst,
                                   intercept = interceptEst)) %>%
    select(-"floorspace", -lambdaCols)

  # replace non-existing historic values (should not be necessary)
  histReplaced <- coefGdpCorrect %>%
    mutate(m2hat = ifelse(.data[["period"]] <= endOfHistory,
                          ifelse(is.na(.data[["m2cap"]]),
                                 exp(log(.data[["gdppop"]]) * .data[["coefLogGdp"]] +
                                       log(.data[["density"]]) * .data[["coefLogDensity"]] +
                                       .data[["intercept"]]) + .data[["delta"]],
                                 .data[["m2cap"]]),
                          NA)) %>%
    arrange(.data[["period"]])

  projectionData <- histReplaced %>%
    group_by(across(all_of("region"))) %>%
    filter(.data[["period"]] >= endOfHistory) %>% # Need last historic time step and all after that
    mutate(coefLogGdpDiff = c(-diff(.data[["coefLogGdp"]]), NA),
           tempCorr = lag(cumsum(.data[["coefLogGdpDiff"]] * log(.data[["gdppop"]]))),
           m2hat = exp(.data[["tempCorr"]] + .data[["delta"]]
                       + .data[["intercept"]] + log(.data[["gdppop"]]) * .data[["coefLogGdp"]]
                       + log(.data[["density"]]) * .data[["coefLogDensity"]])) %>%
    ungroup() %>%
    filter(.data[["period"]] > endOfHistory)

  fullData <- rbind(
    histReplaced %>%
      filter(.data[["period"]] <= endOfHistory),
    projectionData
  )

  # calculate global values
  m2hatGLO <- function(df) {
    df <- df %>%
      select("scenario", "region", "period", "gdppop", "m2hat", "pop", "coefLogGdp", "coefLogDensity") %>%
      gather("variable", "value", one_of("gdppop", "m2hat", "pop", "coefLogGdp", "coefLogDensity")) %>%
      as.quitte() %>%
      missingToNA() %>%
      calcGlob(c("gdppop", "m2hat", "coefLogGdp", "coefLogDensity"), weights = "pop") %>%
      rbind(densityGlobal) %>%
      spread("variable", "value") %>%
      mutate(alpha = log(.data[["m2hat"]]) - .data[["coefLogDensity"]] * log(.data[["density"]]) -
               .data[["coefLogGdp"]] * log(.data[["gdppop"]])) %>%
      select("period", "scenario", "alpha") %>%
      removeColNa()
    return(df)
  }

  tmpGlob <- m2hatGLO(fullData)

  # the regional delta decreases (in absolute value) with time
  tmp <- fullData %>%
    left_join(tmpGlob, by = c("period", "scenario")) %>%
    left_join(lambda,  by = c("region", "period", "scenario")) %>%
    mutate(lambda    = ifelse(.data[["period"]] > endOfHistory,
                              .data[["lambda"]],
                              .data[["fullconv"]]),
           m2hatGlo  = exp(.data[["alpha"]] + .data[["coefLogDensity"]] * log(.data[["density"]]) +
                             .data[["coefLogGdp"]] * log(.data[["gdppop"]])),
           deltaGlo  = .data[["m2hat"]] - .data[["m2hatGlo"]],
           deltaGlo  = .data[["deltaGlo"]] * (1 - .data[["lambda"]]),
           m2hatConv = .data[["m2hatGlo"]] + .data[["deltaGlo"]]) %>%
    select("model", "scenario", "region", "unit", "period", "pop", "gdppop", "m2hatConv")

  return(tmp)
}


#' Correct floorspace projections with upper floorspace limit
#'
#' @param df data.frame containing projected floorspace data
#' @param capFloor country-wise upper floorspace boundary
#' @param lambda temporal convergence shares
#' @param endOfHistory upper temporal boundary of historical data
#'
#' @importFrom dplyr filter left_join group_by mutate select across all_of

capFloorProjections <- function(df, capFloor, lambda, endOfHistory) {
  # historical data
  tmpHist <-  df %>%
    filter(.data[["period"]] <= endOfHistory)

  # scenario / projected data
  tmpScen <- df %>%
    left_join(capFloor, by = "region") %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    group_by(across(all_of("region"))) %>%

    # choose future delta values as last delta of historical data
    mutate(deltaHist = max(0,
                           .data[["m2hatConv"]][as.character(.data[["period"]]) == as.character(endOfHistory)]
                           - .data[["capFloor"]])) %>%
    filter(.data[["period"]] > endOfHistory) %>%

    # project delta and capped floorspace values
    mutate(deltaFin =  .data[["deltaHist"]] *
             (1 - .data[["lambda"]]),
           m2hatConv = ifelse(.data[["deltaHist"]] == 0,
                              pmin(.data[["capFloor"]], .data[["m2hatConv"]]),
                              .data[["capFloor"]] + .data[["deltaFin"]])) %>%
    select(-"deltaHist", -"deltaFin", -"capFloor", -"fullconv", -"lambda") %>%
    ungroup()

  tmp <- rbind(tmpHist, tmpScen)
  return(tmp)
}
