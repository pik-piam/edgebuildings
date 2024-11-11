#' Calculate share of values
#'
#' Calculates shares of elements in one column according to their value.
#'
#' @param data data.frame
#' @param colShare name of share column
#' @param colVal name of value column, default is "value"
#' @param ignoreColumns name of columns to ignore
#'
#' @importFrom lazyeval interp
#' @importFrom dplyr mutate_ ungroup group_by ungroup across all_of %>%
#' @importFrom stats setNames
#'
#' @author Antoine Levesque

calcShares <- function(data, colShare, colVal = "value", ignoreColumns = NULL) {

  cols <- setdiff(colnames(data), c(colShare, colVal, ignoreColumns))
  form <- interp(~ x / sum(x, na.rm = TRUE), x = as.name(colVal))

  # TODO: find a way to replace mutate_ by mutate #nolint
  res <- data %>%
    group_by(across(all_of(cols))) %>%
    mutate_(.dots = setNames(list(form), colVal)) %>%
    ungroup()

  return(res)
}
