#' Calculate share of values
#'
#' Calculates shares of elements in one column according to their value.
#'
#' @param data data.frame
#' @param colShare name of share column
#' @param colVal name of value column, default is "value"
#' @param ignoreColumns name of columns to ignore
#'
#' @importFrom dplyr mutate ungroup group_by ungroup across all_of %>%
#'
#' @author Antoine Levesque

calcShares <- function(data, colShare, colVal = "value", ignoreColumns = NULL) {

  cols <- setdiff(colnames(data), c(colShare, colVal, ignoreColumns))

  res <- data %>%
    group_by(across(all_of(cols))) %>%
    mutate(across(all_of(colVal), ~ .x / sum(.x, na.rm = TRUE))) %>%
    ungroup()

  return(res)
}
