#' Fill Historical Data Gaps Using Model Predictions
#'
#' Fills gaps in the historical data of a single specified variable using model predictions,
#' based on an estimate from a former regression. The data set must include the necessary variables
#' to ensure accurate estimation and correction.
#'
#' @param data \code{data.frame} Input data frame containing historical data on
#' @param estimate \code{nls/lm} Fitted model object
#' @param var \code{character} Name of the target variable
#' @param periodBegin \code{numeric} first year of historical data
#' @param endOfHistory \code{numeric} Last year of historical data
#'
#' @return \code{data.frame} Historical data with gaps filled using corrected predictions
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate group_by ungroup left_join case_when cur_data 
#'             group_modify select across all_of
#' @importFrom stats formula predict coef
#' @importFrom data.table :=
#' @importFrom quitte getPeriods interpolate_missing_periods
#' @importFrom rlang .data !!
#' @importFrom base unique setdiff names stop paste all any is.na

fillHistoricalData <- function(data, estimate, var, periodBegin, endOfHistory) {

  # Truncate data set
  data <- data %>%
    filter(.data[["period"]] <= endOfHistory,
           .data[["period"]] >= periodBegin)

  # Fill missing periods if necessary
  if (!endOfHistory %in% getPeriods(data)) {
    data <- data %>%
      interpolate_missing_periods(seq(periodBegin, endOfHistory), value = var)
  }


  # Extract dependent & independent variable from formula
  formula <- formula(estimate)
  dependentVar <- all.vars(formula[[2]])
  independentVar <- setdiff(all.vars(formula), c(dependentVar, names(coef(estimate))))


  # Check if all relevant variables exist within the data
  requiredVars <- unique(c("region", "period", dependentVar, independentVar))

  if (!all(requiredVars %in% names(data))) {
    stop("The dataset must contain the necessary variables: ", paste(requiredVars, collapse = ", "), ".")
  }


  # Create complete historical template
  historicalTemplate <- data %>%
    select("region", "period") %>%
    unique()


  # Calculate and interpolate correction factors
  correctionFactors <- data %>%
    mutate(prediction = predict(estimate, newdata = cur_data()),
           correctionFactor = .data[[var]] / .data[["prediction"]]) %>%
    # Get correction factors per region
    group_by(across(all_of("region"))) %>%
    group_modify(~ extrapolateMissingPeriods(.x, key = "correctionFactor", slopeOfLast = 5)) %>%
    ungroup() %>%
    select("region", "period", "scenario", "correctionFactor")


  # Fill historical data with corrected regression predictions
  historicalFilled <- historicalTemplate %>%
    left_join(data, by = c("region", "period")) %>%
    left_join(correctionFactors, by = c("region", "period", "scenario")) %>%

    # make predictions
    mutate(prediction = predict(estimate, newdata = cur_data())) %>%

    # Check if region has any non-NA values but not all values are NA
    group_by(across(all_of("region"))) %>%
    mutate(hasPartialData = any(!is.na(.data[[var]])) && !all(is.na(.data[[var]]))) %>%
    ungroup() %>%

    # fill up missing data points
    mutate(
      # Apply corrections following the three-tier logic:
      #   1. Original data if exists
      #   2. Corrected prediction if partial data is there
      #   3. Uncorrected prediction if no historical data
      !!var := case_when(
        !is.na(.data[[var]])      ~ .data[[var]],
        .data[["hasPartialData"]] ~ .data[["prediction"]] * .data[["correctionFactor"]],
        .default                  = .data[["prediction"]]
      )
    ) %>%
    select(-c("correctionFactor", "prediction", "hasPartialData")) %>%

    # label as history
    mutate(scenario = "history")

  return(historicalFilled)
}
