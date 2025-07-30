#' Projection of Final to Useful Energy Efficiencies
#'
#' This function calculates the final (FE) to useful energy (UE) efficiencies for all
#' combinations of energy carriers and end-uses. The projections are based on a model
#' by De Stercke et al., primarily driven by GDP per capita, which follows an S-shaped
#' curve approaching assumed efficiency levels. The curve parameters are derived from a
#' regression using IEA data and are modified according to scenario assumptions to reflect
#' different narratives. The final projection is obtained via a linear interpolation
#' between the unmodified and modified projections.
#'
#' @note Given that the regional resolution from the initial dataset (used to derive the
#' historical efficiencies) differs, we use the regression parameters obtained from
#' said regional resolution (\code{regPars}) in \code{makeProjections}.
#'
#' @param config \code{data.frame} Configuration dataframe specifying scenario settings
#' @param histEfficiencies \code{data.frame} Historical efficiency data
#' @param gdppop \code{data.frame} GDP per capita projections
#' @param scenAssump \code{data.frame} Scenario assumptions for projections
#' @param scenAssumpSpeed \code{data.frame} Convergence speed assumptions for scenarios
#' @param regPars \code{data.frame} Pre-calculated regression parameters (optional)
#' @param gasBioEquality \code{logical} Indicates whether gas and bioenergy should be treated equally
#'
#' @return \code{data.frame} Projected efficiencies for all carrier-enduse combinations
#'
#' @author Hagen Tockhorn
#'
#' @references
#' De Stercke, S. (2014). Dynamics of Energy Systems: A Useful Perspective
#' (Interim Report, p. 68). IIASA. http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @importFrom dplyr filter mutate select bind_rows ungroup cross_join
#' @importFrom tidyr pivot_wider unite separate separate unite
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom rlang .data
#' @importFrom quitte getPeriods

getEfficiencies <- function(config,
                            histEfficiencies,
                            gdppop,
                            scenAssump,
                            scenAssumpSpeed,
                            regPars = NULL,
                            gasBioEquality = TRUE) {

  # PARAMETERS -----------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>%
    unique()

  if (length(scen) > 1) {
    stop("Error in getEfficiencies. Please provide single scenario data.")
  }

  # lower temporal boundary
  periodBegin <- config[scen, "periodBegin"] %>%
    unlist()

  # upper temporal boundary of historical data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()

  # Weight given to log(gdppop) for space_cooling.elec input variable
  #   -> x = log(gdppop) * weight + period * (1 - weight)
  gdppopWeight <- config[scen, "gdppopWeight"] %>%
    unlist()



  # PRE-PROCESS DATA------------------------------------------------------------

  #--- Data is made compliant with config file

  # gdppop
  gdppop <- gdppop  %>%
    filter(.data[["scenario"]] == config[, "gdppopScen"]) %>%
    unique() %>%
    sepVarScen() %>%
    mutate(scenario = scen)


  # scenario parameter assumptions
  scenAssump <- scenAssump %>%
    filter(.data[["scenario"]] == scen)


  # scenario convergence speed assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)


  # calculate temporal convergence shares
  lambda <- compLambdaScen(scenAssumpSpeed,
                           startYearVector = periodBegin,
                           lastIdenticalYear = endOfHistory)

  # calculate temporal convergence shares for deviations (delta)
  lambdaDelta <- compLambdaScen(scenAssumpSpeed,
                                startYearVector = periodBegin,
                                lastIdenticalYear = endOfHistory)


  # electric space cooling COP boundaries
  coolingBounds <- toolGetMapping("coolingEfficiencyBoundaries.csv",
                                  type = "sectoral",
                                  where = "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------

  coolingBounds <- coolingBounds %>%
    select("variable", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value")

  # bring regression parameters into correct form
  regPars <- regPars %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    unite(col = "variable", c("enduse", "carrier"), sep = ".")


  # pre-process gdppop
  gdppop <- gdppop %>%
    mutate(scenario = ifelse(.data[["period"]] <= endOfHistory,
                             "history",
                             scen)) %>%
    filter(.data[["period"]] %in% getPeriods(lambda),
           .data[["period"]] >= periodBegin) %>%
    select("region", "period", "scenario", "variable", "value")


  # pre-process efficiencies
  histEfficiencies <- histEfficiencies %>%
    select("region", "period", "enduse", "carrier", "value") %>%
    unite("variable", c("enduse", "carrier"), sep = ".") %>%
    mutate(scenario = "history")


  # get enduse.carrier combinations
  euecCombinations <- unique(histEfficiencies$variable)


  # bind necessary data frames
  data <- histEfficiencies %>%
    rbind(gdppop)


  # fill missing historical values and make scenario projections
  efficiencyProjections <- do.call("rbind", lapply(euecCombinations, function(euec) {

    # define projection formula
    if (euec == "space_cooling.elec") {

      inputData <- data %>%
        filter(.data$variable == "gdppop") %>%
        rename("gdppop" = "value") %>%
        mutate(value = log(.data$gdppop) * gdppopWeight + .data$period * (1 - gdppopWeight),
               variable = "x") %>%
        select(colnames(data))

      formul <- paste0(euec, " ~ min + (max - min) / (1 + exp(-k * (x + x0)))") %>%
        as.formula()

      data <- data %>%
        rbind(inputData) %>%
        cross_join(coolingBounds)
    } else {
      formul <- paste0(euec, " ~ SSasymp(gdppop, Asym, R0, lrc)") %>%
        as.formula()
    }

    # make projections
    euecProjection <- data %>%
      filter(.data[["variable"]] %in% c(euec, "gdppop", "x")) %>%

      makeProjections(formul           = formul,
                      scenAssump       = scenAssump,
                      lambda           = lambda,
                      lambdaDelta      = lambdaDelta,
                      convReg          = "proportion",
                      convFactor       = "fullconv",
                      endOfHistory     = endOfHistory,
                      initCorrection   = regPars,
                      replacePars      = TRUE) %>%

      filter(.data[["variable"]] != "gdppop") %>%
      separate(col = "variable", into = c("enduse", "carrier"), sep = "\\.") %>%
      mutate(scenario = scen)

    if (euec == "space_cooling.elec") {
      euecProjection <- euecProjection %>%
        select(-"max", -"min")
    }

    return(euecProjection)
  }))



  # OUTPUT ---------------------------------------------------------------------

  return(efficiencyProjections)
}
