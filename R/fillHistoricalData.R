#' Fill Historical Data Gaps Using Model Predictions
#'
#' Fills gaps in the historical data of a single specified variable using model predictions,
#' based on an estimate from a former regression. The data set must include the necessary variables
#' to ensure accurate estimation and correction.
#'
#' @param data \code{data.frame} Input data frame containing historical data on
#' @param estimate \code{nls/lm} Fitted model object
#' @param endOfHistory \code{numeric} Last year of historical data
#' @param var \code{character} Name of the target variable
#'
#' @return \code{data.frame} Historical data with gaps filled using corrected predictions
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate group_by ungroup left_join case_when cur_data group_modify
#' @importFrom stats formula

fillHistoricalData <- function(data, estimate, endOfHistory, var) {

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
    filter(.data[["period"]] <= endOfHistory) %>%
    select("region", "period") %>%
    unique()


  # Calculate and interpolate correction factors
  correctionFactors <- data %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    mutate(prediction = predict(estimate, newdata = cur_data()),
           correctionFactor = .data[[var]] / .data[["prediction"]]) %>%
    # Get correction factors per region
    group_by(across(all_of("region"))) %>%
    group_modify(~ extrapolateMissingPeriods(.x, key = "correctionFactor", slopeOfLast = 5)) %>%
    ungroup() %>%
    select("region", "period", "scenario", "correctionFactor")


  # Fill historical data with corrected regression predictions
  historicalFilled <- historicalTemplate %>%
    left_join(data %>% filter(.data[["period"]] <= endOfHistory),
              by = c("region", "period")) %>%
    left_join(correctionFactors, by = c("region", "period", "scenario")) %>%

    # make predictions
    mutate(prediction = predict(estimate, newdata = cur_data())) %>%

    # fill up missing data points
    group_by(across(all_of("region"))) %>%
    mutate(
      # Check if region has any non-NA values but not all values are NA
      hasPartialData = any(!is.na(.data[[var]])) && !all(is.na(.data[[var]])),

      # Apply corrections following the three-tier logic:
      !!var := case_when(
        !is.na(.data[[var]])   ~ .data[[var]],                   # 1. Use original if exists
        isTRUE(hasPartialData) ~ prediction * correctionFactor,  # 2. Use corrected prediction if partial data
        TRUE                   ~ prediction                      # 3. Use uncorrected prediction if no historical data
      )
    ) %>%
    ungroup() %>%
    select(-c("correctionFactor", "prediction", "hasPartialData"))

  return(historicalFilled)
}
