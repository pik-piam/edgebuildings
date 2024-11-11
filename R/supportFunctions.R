# various functions for supporting EDGE-B data processing

#' Calculate global values of given variable(s)
#'
#' @param df data containing values to be aggregated
#' @param vars2agg variables to globally aggregate
#' @param nameGLO name of global region
#' @param weights aggregation weights
#'
#' @return global values for given variables
#'
#' @author Antoine Levesque
#'
#' @importFrom quitte aggregate_map

calcGlob <- function(df, vars2agg, nameGLO = "GLO", weights = NULL) {

  mapping <- data.frame(region = setdiff(getRegs(df %>%
                                                   filter(.data[["variable"]] %in% vars2agg)),
                                         nameGLO),
                        tar = nameGLO)

  tmp <- aggregate_map(df,
                       subset2agg = vars2agg,
                       mapping = mapping,
                       weights = weights,
                       by = c("region"))

  return(tmp)
}


#' Bind dataframes with common regions and periods
#'
#' Bind those rows of two dataframes that correspond to common regions and
#' periods.
#'
#' @param df1 data.frame to join
#' @param df2 data.frame to join
#'
#' @importFrom quitte getRegs getPeriods
#' @importFrom dplyr filter %>%
#'
#' @author Antoine Levesque

joinReduceYearsRegion <- function(df1, df2) {

  reg <- intersect(getRegs(df1), getRegs(df2))
  per <- intersect(getPeriods(df1), getPeriods(df2))
  tmp <- rbind(df1 %>%
                 filter(.data$region %in% reg,
                        .data$period %in% per),
               df2 %>%
                 filter(.data$region %in% reg,
                        .data$period %in% per))
  return(tmp)
}


# set all '(Missing)' values to NA
missingToNA <- function(df){
  df[df == "(Missing)"] <- NA
  return(df)
}


#' Split or merge scenario column w.r.t. historical periods
#'
#' @param df data.frame to be split/merged
#' @param reverse boolean, if TRUE scenario is split into "history"/SSPs
#' @param endOfHistory upper temporal boundary of historical data
#' @param scen scenario
#'
#' @return merged or split-up data.frame
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte getScenarios
#' @importFrom dplyr filter arrange mutate .data

history2SSPs <- function(df, reverse = FALSE, endOfHistory = NULL, scen = NULL) {

  if (isTRUE(reverse)) {
    # SSP --> history
    df <- rbind(df %>%
                  filter(.data[["period"]] <= endOfHistory) %>%
                  mutate(scenario = "history"),
                df %>%
                  filter(.data[["period"]] > endOfHistory))
  } else {
    # history --> SSP
    tmpHist <- df %>% filter(.data[["scenario"]] == "history")
    tmpScen <- df %>% filter(.data[["scenario"]] != "history")

    if (is.null(scen)) {
      scen <- getScenarios(tmpScen)
    }

    for (sc in scen) {
      tmpScen <- rbind(tmpScen, tmpHist %>% mutate(scenario = sc))
    }

    df <- tmpScen %>%
      arrange(.data[["period"]])
  }

  return(df)
}
