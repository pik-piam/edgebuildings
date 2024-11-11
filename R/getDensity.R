#' Calculate population density
#'
#' Population density in countries in cap/km2.
#'
#' @param fulltime boolean only return values until 2100 if TRUE
#' @param pop population data
#' @param surface surface area data
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte getPeriods
#' @importFrom dplyr mutate filter left_join %>% .data
#' @importFrom tidyr spread gather

getDensity <- function(fulltime = FALSE,
                       pop,
                       surface) {
  # unit conversion
  million2unit <- 1e6

  # prepare data
  pop     <- spread(pop, "variable", "value")
  surface <- spread(surface, "variable", "value")

  # calculate population density
  density <- pop %>%
    left_join(surface, by = c("region")) %>%
    mutate(density = .data[["pop"]] * million2unit / .data[["surface"]]) %>%
    dplyr::select(-"pop", -"surface") %>%
    gather(key = "variable", value = "value", "density") %>%
    unite(col = "variable", "variable", "scenario", sep = "_") %>%
    filter(.data[["value"]] != 0) %>%
    as.data.frame() %>%
    as.quitte() %>%
    missingToNA()

  if (!fulltime) {
    density <- density %>%
      filter(.data[["period"]] <= 2100)
  }

  return(density)
}
