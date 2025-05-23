#' Historic and projected GDP
#'
#' Binds historic with scenario projections of population
#'
#' @param pop data.frame with population
#' @param gdp data.frame with GDP
#' @param fulltime data.frame logical, if \code{FALSE}, drop values after 2100
#' @returns data.frame with GDP per capita
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte getPeriods calc_addVariable
#' @importFrom dplyr bind_rows mutate filter %>% .data
#' @importFrom tidyr unite
#' @export

getGDPpop <- function(pop, gdp, fulltime = FALSE) {

  if (grepl("_boost", unique(gdp[["scenario"]]))) {
    pop <- mutate(pop, scenario = paste0(.data[["scenario"]], "_boost"))
  }

  if (unique(gdp[["scenario"]]) != unique(pop[["scenario"]])) {
    stop("Scenarios in gdp and pop do not match.")
  }

  gdppop <- bind_rows(pop, gdp) %>%
    unique() %>%
    calc_addVariable(gdppop = "gdp / pop", only.new = TRUE) %>%
    as.quitte() %>%
    missingToNA()

  if (!fulltime) {
    gdppop <- filter(gdppop, .data[["period"]] <= 2100)
  }

  return(gdppop)
}
