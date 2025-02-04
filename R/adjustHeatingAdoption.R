#' Adjust Regional Heating Adoption Rates
#'
#' This function corrects space heating useful energy demand projections that were
#' initially derived using a global fit. It refines these projections by incorporating
#' projections of future adoption rates of space heating technologies
#' while accounting for regional trends, historical data, and global convergence.
#' The projection is based on a regression-derived slope and follows either
#' a constant or asymptotic adjustment, depending on a threshold condition.
#'
#' @param df A data frame containing historical and projected data for heating variables.
#' @param config A configuration data frame specifying scenario-specific parameters,
#' including decay rates, target years, and historical period endpoints.
#' @param lambda A data frame specifying transition factors for delta adjustments.
#' @param lambdaDelta A data frame containing precomputed delta transition values.
#'
#' @author Hagen Tockhorn
#'
#' @details
#' The method follows these key steps:
#'
#' - **Historical Trend Estimation:** A linear regression is performed on historical
#'   adoption rates (space heating per HDD) to determine the regional trend.
#' - **Threshold-Based Projection:** The threshold is defined such that only regions
#'   with a positive relative slope (as a percentage change per unit increase over time)
#'   are considered for continued growth and convergence. Regions that do not meet
#'   this criterion maintain a constant adoption rate at their last historical value.
#' - **Global Convergence:** Regional projections gradually align with a global estimate by 2070.
#' - **Deviation Adjustments:** Deviations between model projections and historical data are
#'   computed separately for regional and global approaches, ensuring consistency.
#' - **Short- to Mid-Term Adjustments:** The initial continuation/convergence of the adoption
#'   rate is accounted for, ensuring a smooth transition. Over the long term, these adjustments
#'   will transition into scenario-dependent projections that reflect different policy and climate pathways.
#'
#' @return A data frame with adjusted space heating useful energy demand projections.
#'
#' @importFrom dplyr %>% filter select rename mutate arrange group_by ungroup left_join summarise across any_of
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stats lm coef predict
#' @importFrom quitte getPeriods getRegs as.quitte


adjustHeatingAdoption <- function(df,
                                  config,
                                  lambda,
                                  lambdaDelta) {

  # PARAMETERS -----------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>%
    unique()


  # space_heating delta decay rate
  decayRate <- config[scen, "deltaDecayRate"] %>%
    unlist %>%
    as.numeric()

  # space_heating delta decay target year
  targetYear <- config[scen, "deltaTargetYear"] %>%
    unlist() %>%
    as.numeric()

  # last historical period
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist() %>%
    as.numeric()


  # relevant variables
  heatingVars <- c("space_heating_m2_Uval", "HDD")


  # target value for convergence of relative deviations between historical and projection data
  deltaGlobalTarget <- 1



  # PROCESS DATA ---------------------------------------------------------------

  # filter relevant variables
  data <- df %>%
    filter(.data[["variable"]] %in% heatingVars) %>%
    select(-"model", -"unit") %>%
    pivot_wider(names_from = "variable", values_from = "value")


  # filter historical data
  historicalData <- data %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    na.omit()


  # indicate projections from global approach
  data <- data %>%
    rename("projectionGlo" = "space_heating_m2_Uval") %>%
    filter(.data[["period"]] %in% getPeriods(lambda))



  # --- DETERMINE ADOPTION RATES AND MAKE INITIAL PROJECTIONS

  # determine historical heating trends w.r.t. HDDs
  heatingTrends <- do.call(rbind, lapply(getRegs(historicalData), function(reg) {
    histAdoptionRates <- historicalData %>%
      filter(.data[["region"]] == reg) %>%
      arrange(.data[["period"]]) %>%
      mutate(adoptionRate = .data[["space_heating_m2_Uval"]] / .data[["HDD"]])

    fit <- lm("adoptionRate ~ period", data = histAdoptionRates)

    data.frame(region        = reg,
               absSlope      = coef(fit)[2],
               relSlope      = (coef(fit)[2] / mean(histAdoptionRates$adoptionRate, na.rm = TRUE)) * 100,
               lastHistValue = tail(predict(fit), n = 1L),
               refYear       = max(histAdoptionRates$period))
  }))

  row.names(heatingTrends) <- NULL


  # Calculate regional delta projections from model assumptions
  projAdoptionRate <- data %>%
    left_join(heatingTrends, by = "region") %>%
    mutate(projAdoptionRate = if_else(.data[["period"]] > endOfHistory,
                                      # only consider regions with pos. rel. growth
                                      if_else(abs(.data[["relSlope"]]) > 0,
                                              {
                                                decayFactor <- -log(decayRate) / (targetYear - .data[["refYear"]])
                                                .data[["lastHistValue"]] +
                                                  (.data[["absSlope"]] / decayFactor) *
                                                    (1 - exp(-decayFactor * (.data[["period"]] - .data[["refYear"]])))
                                              },
                                              .data[["lastHistValue"]]),
                                      .data[["lastHistValue"]] + .data[["absSlope"]] *
                                        (.data[["period"]] - .data[["refYear"]]))) %>%
    select("region", "period", "projAdoptionRate")


  # Project future demand w/ regional estimates
  projData <- data %>%
    left_join(projAdoptionRate, by = c("region", "period")) %>%
    mutate(projectionReg = .data[["projAdoptionRate"]] * .data[["HDD"]]) %>%
    select(-any_of(c("absSlope", "lastHistValue")))


  # Project historical demand w/ regional estimates
  historicalData <- historicalData %>%
    left_join(projAdoptionRate, by = c("region", "period")) %>%
    mutate(predictionReg = .data[["projAdoptionRate"]] * .data[["HDD"]]) %>%
    select(-any_of(c("absSlope", "lastHistValue")))



  # --- DETERMINE DEVIATIONS BETWEEN PROJECTIONS AND HISTORICAL DATA

  # determine relative deviations (delta)
  regionalDeltas <- historicalData %>%
    group_by(across(all_of("region"))) %>%
    filter(.data[["period"]] == endOfHistory) %>%
    ungroup() %>%
    mutate(delta = .data[["space_heating_m2_Uval"]] / .data[["predictionReg"]]) %>%
    select("region", "delta")


  projData <- projData %>%
    # join transition factor (lambda)
    left_join(lambdaDelta, by = c("region", "period", "scenario")) %>%

    # join regional deltas
    left_join(regionalDeltas, by = "region") %>%
    mutate(deltaTarget = deltaGlobalTarget) %>%

    # compute temporal evolution of delta
    mutate(deltaFinal = .data[["deltaTarget"]] * .data[["lambda"]] + .data[["delta"]] * (1 - .data[["lambda"]])) %>%

    # make final regional projection with corrected delta factors
    mutate(projectionFinalReg = .data[["projectionReg"]] * .data[["delta"]]) %>%

    # merge regional and global projections
    mutate(projectionFinal = .data[["projectionGlo"]] * .data[["fullconv"]] +
             .data[["projectionFinalReg"]] * (1 - .data[["fullconv"]]))



  # OUTPUT ---------------------------------------------------------------------

  # remove initial projections with global fit
  df <- df %>%
    filter(!(.data[["variable"]] == "space_heating_m2_Uval" & .data[["period"]] > endOfHistory))

  # align columns between initial data frame and projection data
  keepColumns <- c(intersect(colnames(df), colnames(projData)), "projectionFinal")

  # prepare projection data frame for merging
  finalProjections <- projData %>%
    select(one_of(keepColumns)) %>%
    rename("space_heating_m2_Uval" = "projectionFinal") %>%
    pivot_longer(names_to = "variable", values_to = "value", cols = c("space_heating_m2_Uval")) %>%
    anti_join(df, by = c("scenario", "variable", "period", "region")) %>%
    as.quitte() %>%
    missingToNA()

  # Return combined original and new data
  return(rbind(df, finalProjections))
}
