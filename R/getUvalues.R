#' Project Building Envelope U-Values
#'
#' This function projects building envelope U-values based on socioeconomic and climate factors.
#' The projections follow a regression model that accounts for GDP per capita, heating degree
#' days (HDD), and cooling degree days (CDD), with parameters estimated from historical data.
#' Scenario-specific assumptions modify these projections to reflect different
#' policy narratives, with the final values obtained through a linear/logit interpolation between
#' unmodified and modified projections. Lastly, historical projections are smoothed to minimize
#' the effect of climate variability and U-values are forced to not increase over time.
#'
#' @note The regional parameter resolution is preserved from the initial dataset used for
#' deriving the historical U-values. A mixed-effects regression approach accounts for
#' region-specific deviations while maintaining global patterns in the relationship between
#' U-values, climate factors, and economic development.
#'
#' @param config \code{data.frame} Configuration dataframe specifying scenario settings
#' @param uvaluePars \code{data.frame} Pre-calculated regression parameters for U-value projections
#' @param gdppop \code{data.frame} GDP per capita projections
#' @param hddcdd \code{data.frame} Heating and cooling degree days data
#' @param regionalmap \code{data.frame} Mapping between different regional resolutions
#' @param scenAssumpSpeed \code{data.frame} Convergence speed assumptions for scenarios
#'
#' @return \code{data.frame} Projected U-values by region, period, and scenario
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate select rename left_join semi_join group_by ungroup across all_of
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom magclass lowpass

getUValues <- function(config,
                       uvaluePars,
                       gdppop,
                       hddcdd,
                       regionalmap,
                       scenAssumpSpeed) {


  # PARAMETERS -----------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>% unique()

  # lower temporal boundary
  periodBegin <- config[scen, "periodBegin"] %>%
    unlist()

  # upper temporal boundary of historical data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()



  # PRE-PROCESS DATA -----------------------------------------------------------

  # gdppop
  gdppop <- gdppop %>%
    filter(.data[["scenario"]] == config[, "gdppopScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    removeColNa()


  # heating/cooling degree days
  hddcdd <- hddcdd %>%
    filter(.data[["scenario"]] == config[, "uvalueScen"]) %>%
    mutate(scenario = scen) %>%
    removeColNa()


  # scenario convergence speed assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)


  # minimal U-value
  scenAssumpFactor <- config[scen, "econCoef_uvalues_X_min"] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionalmap) %>%
    rename(scenAssumpFactor = "value")


  # calculate temporal convergence shares
  lambda <- compLambdaScen(scenAssumpSpeed   = scenAssumpSpeed,
                           lastIdenticalYear = endOfHistory,
                           startYearVector   = periodBegin)



  # PROCESS DATA ---------------------------------------------------------------

  # bind full data set
  data <- hddcdd %>%
    rbind(gdppop) %>%
    filter(.data$period >= periodBegin) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    left_join(uvaluePars %>%
                pivot_wider(names_from = "variable", values_from = "value"),
              by = "region") %>%
    semi_join(lambda, by = "period")


  # project historical and future u-values
  data <- data %>%
    mutate(uvalueProj = exp(.data$parINTERCEPT +
                              .data$parHDD * .data$HDD +
                              .data$parCDD * .data$CDD +
                              .data$parGDPPOP * log(.data$gdppop + 1) +
                              .data$parHDDGDPPOP * .data$HDD * log(.data$gdppop + 1) +
                              .data$parCDDGDPPOP * .data$CDD * log(.data$gdppop + 1)) +
             .data$minU) %>%

    # include scenario assumptionsx
    left_join(scenAssumpFactor, by = "region") %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%

    # merge historical and scenario projections
    mutate(uvalueScen = .data$uvalueProj * .data$scenAssumpFactor,
           uvalue = .data$uvalueProj * (1 - .data$fullconv) + .data$uvalueScen * .data$fullconv) %>%

    # smooth historical u-values and prohibit decrease over time
    group_by(across(all_of(c("region", "scenario")))) %>%
    mutate(uvalueSmooth = ifelse(.data$period >= endOfHistory, .data$uvalue, lowpass(.data$uvalue, i = 5)),
           uvalueMin = cummin(.data$uvalueSmooth)) %>%
    ungroup() %>%

    select("region", "period", "scenario", "uvalue" = "uvalueMin") %>%
    pivot_longer(cols = "uvalue", names_to = "variable", values_to = "value")


  return(data)
}
