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


  # scenario-specific temporal convergence assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)


  # scenario assumptions on percentage of future u-values rel. to hist. projections
  scenAssumpFactor <- config[scen, "econCoef_uvalues_X_min"] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionalmap) %>%
    rename(scenAssumpFactor = "value")


  # elasticity of marginal influence of climate factors beyond endOfHistory
  climateElas <- config[scen, "uvalues_climateElas"] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionalmap) %>%
    rename("climateElas" = "value")


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


  # obtain 5 year mean of last historical values to reduce bias of single data point
  dataEOH <- data %>%
    filter(.data$period %in% seq.int(endOfHistory - 4, endOfHistory)) %>%
    group_by(across(all_of(c("region", "scenario")))) %>%
    reframe(eohHDD    = mean(.data$HDD,    na.rm = TRUE),
            eohCDD    = mean(.data$CDD,    na.rm = TRUE),
            eohGDPPOP = mean(.data$gdppop, na.rm = TRUE))


  data <- data %>%
    # project historical and future u-values
    left_join(dataEOH, by = c("region", "scenario")) %>%
    left_join(climateElas, by = "region") %>%
    mutate(uvalueProj = ifelse(.data$period <= endOfHistory,

                               # Historical periods: use original regression formula
                               exp(.data$parINTERCEPT +
                                     .data$parHDD * .data$HDD +
                                     .data$parCDD * .data$CDD +
                                     .data$parGDPPOP * log(.data$gdppop + 1) +
                                     .data$parHDDGDPPOP * .data$HDD * log(.data$gdppop + 1) +
                                     .data$parCDDGDPPOP * .data$CDD * log(.data$gdppop + 1)) +
                                 .data$minU,

                               # Future periods: apply climate dampening
                               exp(.data$parINTERCEPT +
                                     .data$parHDD * (.data$eohHDD * (.data$HDD / .data$eohHDD)^.data$climateElas) +
                                     .data$parCDD * (.data$eohCDD * (.data$CDD / .data$eohCDD)^.data$climateElas) +
                                     .data$parGDPPOP * log(.data$gdppop + 1) +
                                     .data$parHDDGDPPOP * (.data$eohHDD * (.data$HDD / .data$eohHDD)^.data$climateElas)
                                     * log(.data$gdppop + 1) +
                                     .data$parCDDGDPPOP * (.data$eohCDD * (.data$CDD / .data$eohCDD)^.data$climateElas)
                                     * log(.data$gdppop + 1)) +
                                 .data$minU)) %>%

    # include scenario assumptions
    left_join(scenAssumpFactor, by = "region") %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%

    # merge historical and scenario projections
    mutate(uvalueScen = .data$uvalueProj * .data$scenAssumpFactor,
           uvalue     = .data$uvalueProj * (1 - .data$fullconv) + .data$uvalueScen * .data$fullconv) %>%

    # smooth historical u-values and prohibit decrease over time
    group_by(across(all_of(c("region", "scenario")))) %>%
    mutate(uvalueSmooth  = ifelse(.data$period >= endOfHistory, .data$uvalue,  lowpass(.data$uvalue, i = 5))) %>%
    filter(!is.na(.data$uvalueSmooth)) %>%

    # Find max historical value and period where it occurs
    mutate(maxHistValue = max(.data$uvalueSmooth[.data$period <= endOfHistory], na.rm = TRUE),
           periodMax    = .data$period[.data$period <= endOfHistory]
           [which.max(.data$uvalueSmooth[.data$period <= endOfHistory])]) %>%

    # enusure that u-values do not increase over time:
    #   1) periods before historical max: apply max value
    #   2) periods at max and after: keep original values
    #   3) apply cumulative min to full time-series
    arrange(.data$period) %>%
    mutate(uvalueCorrected = ifelse(.data$period < .data$periodMax, .data$maxHistValue, .data$uvalueSmooth),
           uvalueMin       = cummin(.data$uvalueCorrected)) %>%
    ungroup() %>%

    select("region", "period", "scenario", "uvalue" = "uvalueMin") %>%
    pivot_longer(cols = "uvalue", names_to = "variable", values_to = "value")


  return(data)
}
