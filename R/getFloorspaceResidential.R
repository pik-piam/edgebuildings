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
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte character.data.frame replace_column overwrite getScenarios
#' getPeriods removeColNa
#' @importFrom dplyr anti_join select select_ filter mutate one_of left_join
#'   group_by min_rank desc arrange lag ungroup bind_rows across all_of %>%
#'   .data
#' @importFrom tidyr spread gather
#' @importFrom dplyr left_join group_by mutate select ungroup rename filter %>%
#'   .data bind_rows
#' @importFrom purrr is_null
#' @importFrom stats coef lm

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
  lambda <- compLambdaScen(scenAssumpSpeed, startYearVector = 1960)



  # PROCESS DATA----------------------------------------------------------------

  # global population density
  densityGLO <- globDensity(surface, density)


  # introduce all the necessary information in fullData for the regression
  fullData <- bind_rows(floorspacePast, gdppop, density, pop) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    filter(!is.na(.data[["gdppop"]]))


  # TODO: transition the lower part towards a regional regression #nolint

  # for the estimation, only take the rows with data for floorspace per capita
  modelData <- filter(fullData, !is.na(.data[["m2cap"]]))

  # Extract last year with historic data available
  lastYearWithData <- max(modelData[["period"]]) # This only works if data availability is the same for all regions

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
  fullData <- addDelta(fullData, lastYearWithData)

  # make the prediction with the convergence of lambda towards 0 in 2200 in a logit fashion
  projectionData <- predictFullDelta(fullData, coefLogGdp, coefLogDensity, intercept,
                             scenAssump, lambda, densityGLO, lastYearWithData)

  # cap floorspace projections if required by scenario assumptions
  if (!is_null(scenAssumpCap) && is.data.frame(scenAssumpCap)) {
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

# calculate global population density
globDensity <- function(surface, density) {
  left_join(density %>% spread("variable", "value"),
            surface %>% spread("variable", "value"),
            by = "region") %>%
    gather(key = "variable", value = "value", one_of("density", "surface")) %>%
    calcGlob("density", weights = "surface")
}


#' Calculate the difference between expected and calculated log(m2cap)
#' for the last time period with floorspace data.
#'
#' @param data data frame containing historic and estimated floorspace data
#' @param lastYearWithData numeric, time period from which to compute the delta value
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate ungroup
#'
addDelta <- function(data, lastYearWithData) {
  data %>%
    group_by(across(all_of(c("region")))) %>%
    mutate(delta = log(.data[["m2cap"]][.data[["period"]] == lastYearWithData])
                   - log(.data[["m2caphat"]][.data[["period"]] == lastYearWithData])) %>%
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
#' @param coefLogGdpEst logarithmic income elasticity parameter
#' @param coefLogDensityEst logarithmic pop density elasticity parameter
#' @param interceptEst logarithmic intercept parameter
#' @param scenAssump parameter scenario assumptions
#' @param lambda temporal convergence factors
#' @param densityGlobal global population density
#' @param endOfHistory upper temporal boundary of historical data
#'
#' @importFrom dplyr filter left_join mutate select arrange group_by ungroup across all_of
#' @importFrom quitte getPeriods overwrite as.quitte removeColNa
#' @importFrom tidyr gather
#'
predictFullDelta <- function(fullData,
                             coefLogGdpEstimate,
                             coefLogDensityEstimate,
                             interceptEstimate,
                             scenAssump,
                             lambda,
                             densityGlobal,
                             endOfHistory) {

  # Apply scenario assumptions: Modify the logarithmic income elasticity gradually
  fullData <- fullData %>%
    filter(.data[["period"]] %in% getPeriods(lambda)) %>%
    left_join(scenAssump, by = c("region", "scenario")) %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(coefLogGdp = ifelse(.data[["period"]] == endOfHistory, # Temporary: Can be removed once lambda is fixed to be 0 in endOfHistory
                               coefLogGdpEstimate,
                               .data[["fullconv"]] * (coefLogGdpEstimate * .data[["floorspace"]])
                               + (1 - .data[["fullconv"]]) * coefLogGdpEstimate),
           coefLogDensity = coefLogDensityEstimate,
           intercept = interceptEstimate) %>%
    select(-"floorspace", -"lambda", -"fullconv")

  modelData <- fullData %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    mutate(m2hat = .data[["m2cap"]])

  # Compute the floorspace projection from the regression model, the initial deviation delta
  # and the term to correct for that the modified income elasticity only acts on income growth
  projectionData <- fullData %>%
    arrange(.data[["period"]]) %>%
    group_by(across(all_of("region"))) %>%
    filter(.data[["period"]] >= endOfHistory) %>% # Need last historic time step and all after that
    mutate(coefLogGdpDiff = c(-diff(.data[["coefLogGdp"]]), NA),
           growthCorr = lag(cumsum(.data[["coefLogGdpDiff"]] * log(.data[["gdppop"]]))),
           m2hat = exp(.data[["growthCorr"]] + .data[["delta"]]
                       + .data[["intercept"]] + log(.data[["gdppop"]]) * .data[["coefLogGdp"]]
                       + log(.data[["density"]]) * .data[["coefLogDensity"]])) %>%
    ungroup() %>%
    filter(.data[["period"]] > endOfHistory) %>%
    select(-"coefLogGdpDiff", -"growthCorr")

  # Combine projections with historic data
  projectionData <- rbind(modelData, projectionData)

  # calculate global values
  m2hatGLO <- function(df) {
    df <- df %>%
      select("scenario", "region", "period", "gdppop", "m2hat", "pop", "coefLogGdp", "coefLogDensity") %>%
      gather("variable", "value", one_of("gdppop", "m2hat", "pop", "coefLogGdp", "coefLogDensity")) %>%
      as.quitte() %>%
      missingToNA() %>%
      calcGlob(c("gdppop", "m2hat", "coefLogGdp", "coefLogDensity"), weights = "pop") %>%
      rbind(densityGlobal) %>%
      spread("variable", "value") %>%
      mutate(alpha = log(.data[["m2hat"]]) - .data[["coefLogDensity"]] * log(.data[["density"]]) -
               .data[["coefLogGdp"]] * log(.data[["gdppop"]])) %>%
      select("period", "scenario", "alpha") %>%
      removeColNa()
    return(df)
  }

  tmpGlob <- m2hatGLO(projectionData)

  # the regional delta decreases (in absolute value) with time
  tmp <- projectionData %>%
    left_join(tmpGlob, by = c("period", "scenario")) %>%
    left_join(lambda,  by = c("region", "period", "scenario")) %>%
    mutate(lambda    = ifelse(.data[["period"]] > endOfHistory,
                              .data[["lambda"]],
                              .data[["fullconv"]]),
           m2hatGlo  = exp(.data[["alpha"]] + .data[["coefLogDensity"]] * log(.data[["density"]]) +
                             .data[["coefLogGdp"]] * log(.data[["gdppop"]])),
           deltaGlo  = .data[["m2hat"]] - .data[["m2hatGlo"]],
           deltaGlo  = .data[["deltaGlo"]] * (1 - .data[["lambda"]]),
           m2hatConv = .data[["m2hatGlo"]] + .data[["deltaGlo"]]) %>%
    select("model", "scenario", "region", "unit", "period", "pop", "gdppop", "m2hatConv")

  return(tmp)
}


#' Correct floorspace projections with upper floorspace limit
#'
#' @param df data.frame containing projected floorspace data
#' @param capFloor country-wise upper floorspace boundary
#' @param lambda temporal convergence shares
#' @param endOfHistory upper temporal boundary of historical data
#'
#' @importFrom dplyr filter left_join group_by mutate select across all_of

capFloorProjections <- function(df, capFloor, lambda, endOfHistory) {
  # historical data
  tmpHist <-  df %>%
    filter(.data[["period"]] <= endOfHistory)

  # scenario / projected data
  tmpScen <- df %>%
    left_join(capFloor, by = "region") %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    group_by(across(all_of("region"))) %>%

    # choose future delta values as last delta of historical data
    mutate(deltaHist = max(0,
                           .data[["m2hatConv"]][as.character(.data[["period"]]) == as.character(endOfHistory)]
                           - .data[["capFloor"]])) %>%
    filter(.data[["period"]] > endOfHistory) %>%

    # project delta and capped floorspace values
    mutate(deltaFin =  .data[["deltaHist"]] *
             (1 - .data[["lambda"]]),
           m2hatConv = ifelse(.data[["deltaHist"]] == 0,
                              pmin(.data[["capFloor"]], .data[["m2hatConv"]]),
                              .data[["capFloor"]] + .data[["deltaFin"]])) %>%
    select(-"deltaHist", -"deltaFin", -"capFloor", -"fullconv", -"lambda") %>%
    ungroup()

  tmp <- rbind(tmpHist, tmpScen)
  return(tmp)
}
