#' Historic and projected GDP
#'
#' Adds GDP boost if specified in scenario config.
#'
#' @param config scenario-wise parameter configuration
#' @param gdp data.frame containing gdp data
#' @param regionmap regional mapping
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte interpolate_missing_periods getPeriods
#' @importFrom dplyr mutate filter left_join select %>% .data
#'
#' @export

getGDP <- function(config,
                   gdp,
                   regionmap) {
  # PARAMETERS------------------------------------------------------------------

  scen <- row.names(config) %>% unique()
  if (length(scen) > 1) {
    stop("Scenario count more than one. Please adjust to single scenario input.")
  }


  # PRE-PROCESS DATA------------------------------------------------------------

  # gdp
  gdp <- gdp %>%
    filter(.data[["variable"]] == config[scen, "gdpScen"])

  # gdpBoost
  gdpBoost <- config[scen, "gdpBoost"]

  # upper temporal threshold of historic data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()


  # PROCESS DATA----------------------------------------------------------------

  # get region-wise gdp boost values
  gdpBoost <- buildScenInput(gdpBoost,
                             subtype = "mapping",
                             regionmap = regionmap) %>%
    rename(boost = "value")


  # GDP boost for SDP scenarios
  if (grepl("SDP", scen)) {
    #nolint start
    # split time horizon into periods
    lambda <- data.frame(period = c(endOfHistory, 2030, 2055, 2100),
                         value = c(0, 1, 1, 0.25),
                         variable = "lambda") %>%
      interpolate_missing_periods(getPeriods(gdp), expand.values = TRUE) %>%
      select("period", lambda = "value")

    gdp <- gdp %>%
      left_join(gdpBoost, by = "region") %>%
      left_join(lambda, by = "period") %>%
      mutate(value = .data[["value"]] * (1 + .data[["boost"]] * .data[["lambda"]])) %>%
      select(-"boost", -"lambda")
  }


  # OUTPUT----------------------------------------------------------------------

  gdp <- gdp %>%
    select("region", "period", "variable", "value")

  return(gdp)
}
