#' Read projections
#'
#' Read all projection csv files in a given path and return them as one
#' data.frame
#'
#' @author Robin Hasse
#'
#' @param path character, path to run folder
#' @param mergeHist logical, merge history with each projection scenario if TRUE
#'
#' @importFrom utils read.csv
#' @export

readProjections <- function(path, mergeHist = TRUE) {
  list.files(path, "projections_.*\\.csv$", full.names = TRUE) %>%
    lapply(function(file) {
      # scen_name <- sub("^.*projections_(.*)\\.csv$", "\\1", file)
      out <- read.csv(file)
      if (isTRUE(mergeHist)) {
        out <- mergeHistory(out)
      }
      # out <- mutate(out, scenario = scen_name)
      return(out)
    }) %>%
    do.call(what = rbind)
}
