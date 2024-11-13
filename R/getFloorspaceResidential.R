#' Residential floorspace demand projections
#'
#' Takes historic floorspace demand, income projections, population density
#' projections and population projections and projects residential floorspace
#' demand at country level.
#'
#' @param config scenario-wise parameter configuration
#' @param floorspacePast data.frame historic floorspace demand
#' @param gdppop data.frame gdp per capita
#' @param density data.frame population density
#' @param pop data.frame population
#' @param surface data.frame with region surface area
#' @param regionmap regional mapping
#' @param scenAssump data.frame scenario-specific assumptions
#' @param scenAssumpSpeed data.frame scenario-specific temporal upper boundary
#'
#' @returns data.frame historic and scenario-specific projected residential
#' floorspace in million m2 and floorspace per capita in m2/cap on regional level
#'
#' @author Antoine Levesque, Hagen Tockhorn, Ricarda Rosemann
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr anti_join select filter mutate bind_rows %>% .data rename
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stats coef lm
#'
getFloorspaceResidential <- function(config,
                                     floorspacePast,
                                     gdppop,
                                     density,
                                     pop,
                                     surface,
                                     regionmap,
                                     scenAssump,
                                     scenAssumpSpeed) {

  # PARAMETERS------------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>% unique()

  # upper floorspace limit
  scenAssumpCap <- config[scen, "floorspaceCap"]

  # upper temporal threshold of historic data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()



  # READ-IN DATA----------------------------------------------------------------

  if (scenAssumpCap != "NULL") {
    scenAssumpCap <- scenAssumpCap %>%
      buildScenInput(subtype = "mapping",
                     regionmap = regionmap) %>%
      rename(capFloor = "value")
  }


  # PRE-PROCESS DATA------------------------------------------------------------

  #--- Data is made compliant with config file

  # gdppop
  gdppop <- gdppop %>%
    filter(.data[["scenario"]] == config[scen, "gdppopScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    missingToNA()

  # pop
  pop <- pop %>%
    filter(.data[["scenario"]] == config[scen, "popScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    missingToNA()

  # density
  density <- density %>%
    filter(.data[["scenario"]] == config[scen, "densityScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    missingToNA()


  # scenario parameter assumptions
  scenAssump <- scenAssump %>%
    filter(.data[["scenario"]] == scen) %>%
    select("region", "scenario", "floorspace")


  # scenario convergence speed assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)


  # historical floorspace
  floorspacePast <- floorspacePast %>%
    mutate(scenario = scen)


  # create the lambda vector
  lambda <- compLambdaScen(scenAssumpSpeed, endOfHistory, startYearVector = 1960)



  # PROCESS DATA----------------------------------------------------------------

  # introduce all the necessary information in fullData for the regression
  fullData <- bind_rows(floorspacePast, gdppop, density, pop) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    filter(!is.na(.data[["gdppop"]]))


  # TODO: transition the lower part towards a regional regression #nolint

  # for the estimation, only take the rows with data for floorspace per capita
  modelData <- filter(fullData, !is.na(.data[["m2cap"]]))

  # Extract last year with historic data available
  endOfData <- max(modelData[["period"]]) # This only works if data availability is the same for all regions

  # create the estimate object from the regression
  estimate <- lm("log(m2cap) ~  log(gdppop) + log(density)", data = modelData)

  # withdraw the coeffient informations. We do not use predict for the out-of-sample data,
  # because of the way of dealing with discrepancies between estimation and actual data
  intercept      <- unname(coef(estimate)["(Intercept)"])
  coefLogGdp     <- unname(coef(estimate)["log(gdppop)"])
  coefLogDensity <- unname(coef(estimate)["log(density)"])

  # make the estimation for m2cap
  modelData$m2caphat <- exp(unname(predict(estimate, newdata = modelData)))

  # Combine estimation data with the fullData object
  fullData <- rbind(
    modelData,
    fullData %>%
      anti_join(modelData, by = c("period", "region")) %>%
      mutate(m2caphat = NA)
  )

  # Add the delta of the last time period with data to fullData
  fullData <- addDelta(fullData, endOfData)

  # make the prediction with the convergence of lambda towards 0 in 2200 in a logit fashion
  projectionData <- predictFullDelta(fullData, coefLogGdp, coefLogDensity, intercept,
                                     scenAssump, lambda, endOfData, endOfHistory)

  # cap floorspace projections if required by scenario assumptions
  if (!is.null(scenAssumpCap) && is.data.frame(scenAssumpCap)) {
    projectionData <- capFloorProjections(projectionData, scenAssumpCap, lambda, endOfHistory)
  }

  # calculate full aggregated floorspace
  finalProjections <- projectionData %>%
    mutate(m2 = .data[["m2hatConv"]] * .data[["pop"]]) %>%
    select("model", "scenario", "region", "unit", "period", "m2hatConv", "m2") %>%
    rename("m2cap" = "m2hatConv") %>%
    pivot_longer(cols = "m2cap":"m2", names_to = "variable", values_to = "value") %>%
    as.quitte() %>%
    missingToNA()

  return(finalProjections)
}



# INTERNAL FUNCTIONS -----------------------------------------------------------

#' Calculate the difference between expected and calculated log(m2cap)
#' for the last time period with floorspace data.
#'
#' @param data data frame containing historic and estimated floorspace data
#' @param endOfData numeric, time period from which to compute the delta value
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate ungroup
#'
addDelta <- function(data, endOfData) {
  data %>%
    group_by(across(all_of(c("region")))) %>%
    mutate(delta = (log(.data[["m2cap"]][.data[["period"]] == endOfData])
                    - log(.data[["m2caphat"]][.data[["period"]] == endOfData]))) %>%
    ungroup()
}


#' Calculate full residential floorspace prediction
#'
#'
#' As the income elasticity will evolve over time according to scenario assumptions,
#' the elasticity at a certain point in time will only influence the incremental
#' floor space demand. This implies a stepwise calculation of future floor space demand
#' based on the value in the previous time step. The resulting iterative formula can be
#' rearranged to an equivalent explicit formulation of the following form:
#' Regression Model + Delta + growth correction term,
#' where the latter corrects the regression model for the fact that the changed income
#' elasticity only acts on income growth, i.e. this accounts for the original iterative approach.
#'
#' @param fullData full dataset containing predicted floorspace values
#' @param coefLogGdpEstimate logarithmic income elasticity parameter
#' @param coefLogDensityEstimate logarithmic pop density elasticity parameter
#' @param interceptEstimate logarithmic intercept parameter
#' @param scenAssump parameter scenario assumptions
#' @param lambda temporal convergence factors
#' @param endOfData upper temporal boundary of available historical data
#' @param endOfHistory last historic time period, i.e. before scenario assumptions take effect
#'   TODO: Can be removed as soon as lambda is fixed!
#'
#' @importFrom dplyr %>% .data filter lag left_join mutate select arrange group_by
#'   ungroup across all_of
#' @importFrom quitte getPeriods
#'
predictFullDelta <- function(fullData,
                             coefLogGdpEstimate,
                             coefLogDensityEstimate,
                             interceptEstimate,
                             scenAssump,
                             lambda,
                             endOfData) {

  # Apply scenario assumptions: Modify the logarithmic income elasticity gradually
  fullData <- fullData %>%
    filter(.data[["period"]] %in% getPeriods(lambda)) %>%
    left_join(scenAssump, by = c("region", "scenario")) %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(coefLogGdp = ifelse(.data[["period"]] == endOfHistory, # Temporary: Can be removed once lambda is fixed to be 0 in endOfData
                               coefLogGdpEstimate,
                               .data[["fullconv"]] * (coefLogGdpEstimate * .data[["floorspace"]])
                               + (1 - .data[["fullconv"]]) * coefLogGdpEstimate),
           coefLogDensity = coefLogDensityEstimate,
           intercept = interceptEstimate) %>%
    select(-"floorspace", -"lambda", -"fullconv")

  modelData <- fullData %>%
    filter(.data[["period"]] <= endOfData) %>%
    mutate(m2hat = .data[["m2cap"]])

  # Compute the floorspace projection from the regression model, the initial deviation delta
  # and the term to correct for that the modified income elasticity only acts on income growth
  projectionData <- fullData %>%
    arrange(.data[["period"]]) %>%
    group_by(across(all_of("region"))) %>%
    filter(.data[["period"]] >= endOfData) %>% # Need last historic time step and all after that
    mutate(coefLogGdpDiff = c(-diff(.data[["coefLogGdp"]]), NA),
           growthCorr = lag(cumsum(.data[["coefLogGdpDiff"]] * log(.data[["gdppop"]]))),
           m2hat = exp(.data[["growthCorr"]] + .data[["delta"]]
                       + .data[["intercept"]] + log(.data[["gdppop"]]) * .data[["coefLogGdp"]]
                       + log(.data[["density"]]) * .data[["coefLogDensity"]])) %>%
    ungroup() %>%
    filter(.data[["period"]] > endOfData) %>%
    select(-"coefLogGdpDiff", -"growthCorr")

  # Combine projections with historic data
  rbind(modelData, projectionData) %>%

    # Gradually decrease the region-specific deviation delta from the regression model:
    # Instead of delta we now apply (1-lambda) * delta.
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(lambda = ifelse(.data[["period"]] == endOfHistory, .data[["fullconv"]], .data[["lambda"]])) %>% # temporary until lambda is fixed
    mutate(m2hatConv = .data[["m2hat"]] / exp(.data[["lambda"]] * .data[["delta"]])) %>%
    select("model", "scenario", "region", "unit", "period", "pop", "gdppop", "m2hatConv")
}


#' Correct floorspace projections with upper floorspace limit
#'
#' If already the last time point with historic data exceeds the cap, the floorspace
#' demand is gradually decreased towards the cap.
#'
#' Otherwise the minimum of the projected floorspace demand and the cap is taken.
#'
#' @param projectionData data.frame containing projected floorspace data
#' @param capFloor country-wise upper floorspace boundary
#' @param lambda temporal convergence shares
#' @param endOfHistory upper temporal boundary of historical data
#'
#' @importFrom dplyr filter left_join group_by mutate select across all_of

capFloorProjections <- function(projectionData, capFloor, lambda, endOfHistory) {
  # historical data
  histData <-  projectionData %>%
    filter(.data[["period"]] <= endOfHistory)

  # scenario / projected data
  projectionData %>%
    left_join(capFloor, by = "region") %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    group_by(across(all_of("region"))) %>%

    # choose future delta values as last delta of historical data
    mutate(deltaHist = max(0,
                           .data[["m2hatConv"]][as.character(.data[["period"]]) == as.character(endOfHistory)]
                           - .data[["capFloor"]])) %>%
    filter(.data[["period"]] > endOfHistory) %>%

    # project delta and capped floorspace values
    mutate(deltaFin =  .data[["deltaHist"]] * (1 - .data[["lambda"]]),
           m2hatConv = ifelse(.data[["deltaHist"]] == 0,
                              # Historic floorspace does not exceed the cap
                              pmin(.data[["capFloor"]], .data[["m2hatConv"]]),
                              # Historic floorspace already exceeds the cap
                              .data[["capFloor"]] + .data[["deltaFin"]])) %>%
    select(-"deltaHist", -"deltaFin", -"capFloor", -"fullconv", -"lambda") %>%
    ungroup() %>%
    rbind(histData)
}
