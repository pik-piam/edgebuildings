#' Make the history scenario part of all other scenarios
#'
#' @author Robin Hasse
#'
#' @param data either a list of data.frames, each with a history and one
#'   projection scenario or one dataframe with a history and multiple projection
#'   scenarios
#'
#' @importFrom dplyr %>% mutate filter

mergeHistory <- function(data) {
  if (is.data.frame(data)) {
    do.call(rbind, lapply(setdiff(data[["scenario"]], "history"), function(scen) {
      data %>%
        filter(.data[["scenario"]] %in% c("history", scen)) %>%
        mutate(scenario = scen)
    }))
  } else if (is.list(data)) {
    do.call(rbind, lapply(data, function(scen) {
      mutate(scen, scenario = setdiff(.data[["scenario"]], "history"))
    }))
  } else {
    stop("data has to be a list or a data.frame.")
  }
}
