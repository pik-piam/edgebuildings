#' Project future floorspace demand
#'
#' Project floorspace demand of residential and commercial buildings from
#' historical data. This function aggregates the results from \code{getFloorspaceResidential}
#' and \code{getShareFloorCommercial}.
#'
#' @param config scenario-wise parameter configuration
#' @param resid data on residential floorspace
#' @param comShares data on absolute share of commercial building floorspace demand
#' @param comSharesIEA data on absolute share of commercial building FE demand (IEA)
#' @param regionmappingIEA region mapping for IEA dataset
#' @param regionmapping region mapping
#' @param scenAssumpSpeed region-wise scenario assumptions on temporal evolution
#'
#' @return data.frame w/ absolute and type-wise floorspace in m2
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte interpolate_missing_periods revalue.levels
#'   removeColNa factor.data.frame
#' @importFrom dplyr select filter mutate left_join group_by ungroup bind_rows
#'   arrange %>% .data
#' @importFrom stats complete.cases
#' @importFrom tidyr spread gather

getFloorspaceBuild <- function(config,
                               resid,
                               comShares,
                               comSharesIEA,
                               regionmappingIEA,
                               regionmapping,
                               scenAssumpSpeed) {
  # PARAMETERS------------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>% unique()

  # lower temporal boundary
  periodBegin <- config[scen, "periodBegin"] %>%
    unlist()

  # upper temporal boundary of historical data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()


  # PRE-PROCESS DATA------------------------------------------------------------

  #--- Data is made compliant with config file

  # residential buildings' floorspace
  resid <- resid %>%
    filter(.data[["scenario"]] == scen)

  # commercial share
  comShares <- comShares %>%
    filter(.data[["scenario"]] == scen)

  # scenario convergence speed assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)

  # create the temporal convergence shares
  lambda <- compLambdaScen(scenAssumpSpeed, startYearVector = 1960)



  # READ-IN DATA----------------------------------------------------------------

  # residential floorspace
  resid <- resid %>%
    filter(.data$variable == "m2")

  # commercial floorspace shares
  comSharesIEA <- comSharesIEA %>%
    interpolate_missing_periods(periodBegin:endOfHistory,
                                value = "share",
                                expand.values = TRUE)


  # regional mapping for normalisation to IEA floorspace data
  mappingFloor <- regionmappingIEA[c("iea", "EDGE_EUR_ETP")]

  # regions to be revalued
  mapReval <- mappingFloor[["EDGE_EUR_ETP"]]
  names(mapReval) <- mappingFloor$iea

  # revalue regions
  comSharesIEA <- revalue.levels(comSharesIEA, region = mapReval)
  # comSharesIEA <- na.omit(comSharesIEA)
  comSharesIEA <- comSharesIEA[complete.cases(comSharesIEA[, c("region", "period", "share")]), ]



  # PROCESS DATA----------------------------------------------------------------

  # join residential and commercial data
  build <- joinReduceYearsRegion(resid, comShares)

  # calculate full data
  buildFull <- build %>%

    # compute the commercial area and commercial share
    spread("variable", "value") %>%
    rename(residential = "m2") %>%
    mutate(commercial   = .data[["shareComm"]] * .data[["residential"]],
           comSharesHat = .data[["commercial"]] / .data[["residential"]]) %>%

    # correct commercial share with historical values
    left_join(comSharesIEA, by = c("period", "region", "unit")) %>%
    mutate(shareCommWished = .data[["shareComm"]] * .data[["share"]] / .data[["comSharesHat"]]) %>%

    # calculate deviation from historical data
    mutate(delta = .data[["shareCommWished"]] - .data[["shareComm"]]) %>%

    # project future delta values with lambda convergence shares
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    group_by(across(all_of("region"))) %>%
    mutate(delta = ifelse(.data[["period"]] > endOfHistory,
                          .data[["delta"]][.data[["period"]] == endOfHistory],
                          .data[["delta"]]),
           delta = .data[["delta"]] * (1 - .data[["lambda"]])) %>%

    # calculate corrected commercial floorspace values
    mutate(commercial = ifelse(.data[["period"]] <= endOfHistory,
                               .data[["commercial"]],
                               ifelse(is.na(.data[["delta"]]),
                                      .data[["commercial"]],
                                      (.data[["shareComm"]] + .data[["delta"]]) * .data[["residential"]]))) %>%
    filter(!(.data[["period"]] <= endOfHistory &
               is.na(.data[["share"]]) &
               length(.data[["share"]][!is.na(.data[["share"]])]) > 0)) %>%
    ungroup() %>%

    # aggregate residential and commercial floorspace
    mutate(buildings = .data[["residential"]] + .data[["commercial"]]) %>%
    dplyr::select("model", "scenario", "region", "unit", "period", "residential", "commercial", "buildings") %>%
    gather("variable", "value", "residential":"buildings")



  # OUTPUT----------------------------------------------------------------------

  data <- buildFull %>%
    as.data.frame() %>%
    as.quitte() %>%
    missingToNA()


  return(data)
}
