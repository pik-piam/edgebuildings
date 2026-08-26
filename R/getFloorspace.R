#' Floor space demand projections
#'
#' Projects residential and commercial floor space demand at region level from
#' historic floor space, income and population density.
#'
#' Historic floor space per capita (\code{floorspacePast}) holds both
#' \code{"residential"} and \code{"commercial"} data. Each time series is projected with a single global
#' regression whose coefficients are supplied in \code{floorspaceCoefs}. The
#' projection combines the regression estimate with the region-specific deviation
#' at the last historic year (\code{logResidual}), a growth-correction term
#' accounting for the scenario-evolving income elasticity, and a lambda
#' convergence of that deviation towards the regression.
#' Residential floor space depends on income and population density, commercial
#' floor space on income alone. An optional upper cap is applied to residential
#' floor space only.
#'
#' The function returns residential, commercial and their sum (\code{buildings}),
#' each in absolute floor space (m2, bare variable names) and per capita
#' (m2/cap, \code{"_m2cap"} suffix).
#'
#' @param config scenario-wise parameter configuration
#' @param floorspacePast data.frame historic residential and commercial floor
#'   space per capita, distinguished by the \code{variable} column
#' @param floorspaceCoefs data.frame global regression coefficients, with the
#'   coefficient name in the \code{variable} column (e.g.
#'   \code{"(Intercept)_residential"}) and its value in \code{value}
#' @param gdppop data.frame gdp per capita
#' @param density data.frame population density
#' @param pop data.frame population
#' @param regionmap regional mapping
#' @param scenAssump data.frame scenario-specific assumptions
#' @param scenAssumpSpeed data.frame scenario-specific temporal convergence speed
#'
#' @returns data.frame historic and scenario-specific projected floor space in
#'   absolute (m2) and per-capita (m2/cap) terms on regional level
#'
#' @author Antoine Levesque, Hagen Tockhorn, Ricarda Rosemann
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter mutate select rename left_join bind_rows %>% .data
#' @importFrom tidyr pivot_wider

getFloorspace <- function(config,
                          floorspacePast,
                          floorspaceCoefs,
                          gdppop,
                          density,
                          pop,
                          regionmap,
                          scenAssump,
                          scenAssumpSpeed) {

  # PARAMETERS -----------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>% unique()

  # upper temporal threshold of historic data
  endOfHistory <- config[scen, "endOfHistory"] %>% unlist()

  # optional upper floor space limit (residential only)
  capFloor <- config[scen, "floorspaceCap"]
  capFloor <- if (capFloor != "NULL") {
    capFloor %>%
      buildScenInput(subtype = "mapping", regionmap = regionmap) %>%
      rename(capFloor = "value")
  } else {
    NULL
  }


  # PRE-PROCESS DATA -----------------------------------------------------------

  # merge socio-economic drivers
  drivers <- gdppop %>%
    .prepScenInput(config[scen, "gdppopScen"], scen) %>%
    select("region", "period", "scenario", "gdppop" = "value") %>%
    left_join(pop %>%
                .prepScenInput(config[scen, "popScen"], scen) %>%
                select("region", "period", "pop" = "value"),
              by = c("region", "period")) %>%
    left_join(density %>%
                .prepScenInput(config[scen, "densityScen"], scen) %>%
                select("region", "period", "density" = "value") %>%
                # the regression coefficients were fitted on the density from
                # mredgebuildings::calcDensity (pop / (surface * 1e3)), which is
                # 1000x smaller than getDensity's cap/km2; rescale to match
                mutate(density = .data[["density"]] / 1e3),
              by = c("region", "period"))

  # scenario parameter and convergence speed assumptions
  scenAssump <- scenAssump %>%
    filter(.data[["scenario"]] == scen) %>%
    select("region", "scenario", "floorspace")

  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)

  # temporal convergence factors (shared by both variables)
  lambda <- compLambdaScen(scenAssumpSpeed, endOfHistory, startYearVector = 1960)


  # PROCESS DATA ---------------------------------------------------------------

  # per-variable model specification: residential uses income + density and is
  # capped, commercial uses income only and is not capped
  specs <- list(
    residential = list(predictors = c("gdppop", "density"), capFloor = capFloor),
    commercial  = list(predictors = c("gdppop"),            capFloor = NULL)
  )

  projections <- lapply(names(specs), function(varName) {
    spec <- specs[[varName]]

    # historic floor space per capita of this variable
    hist <- floorspacePast %>%
      filter(.data[["variable"]] == varName) %>%
      select("region", "period", "m2cap" = "value")

    # assemble regression frame: drivers + historic floor space per capita
    data <- drivers %>%
      left_join(hist, by = c("region", "period")) %>%
      filter(!is.na(.data[["gdppop"]]))

    # global regression coefficients supplied with the input
    coefs <- .extractCoefficients(floorspaceCoefs, varName, spec$predictors)

    # project; scenario assumptions, the cap and the history-matching deviation
    # (logResidual) are handled inside the pipeline
    data %>%
      .projectFloorspace(spec$predictors, coefs) %>%
      .incorporateScenarioAssumptions(scenAssump, lambda, endOfHistory, capFloor = spec$capFloor) %>%
      mutate(variable = varName)
  })


  # AGGREGATE ------------------------------------------------------------------

  # residential and commercial floor space, absolute and per capita
  sectorData <- bind_rows(projections) %>%
    mutate(m2 = .data[["m2capProjected"]] * .data[["pop"]])

  # aggregate to total buildings floor space
  buildings <- sectorData %>%
    select(-"m2capProjected") %>%
    pivot_wider(names_from = "variable", values_from = "m2") %>%
    mutate(m2 = .data[["residential"]] + .data[["commercial"]],
           m2capProjected = .data[["m2"]] / .data[["pop"]],
           variable = "buildings") %>%
    select(-"residential", -"commercial")

  allData <- bind_rows(sectorData, buildings)


  # OUTPUT ---------------------------------------------------------------------

  data <- bind_rows(allData %>%
                      select("scenario", "region", "period", "variable", "value" = "m2"),
                    allData %>%
                      select("scenario", "region", "period", "variable", "value" = "m2capProjected") %>%
                      mutate(variable = paste0(.data[["variable"]], "_m2cap"))) %>%
    as.quitte() %>%
    missingToNA()

  return(data)
}



# INTERNAL FUNCTIONS -----------------------------------------------------------

#' Make a socio-economic driver compliant with the config file
#'
#' @param df data.frame with a scenario column
#' @param cfgScen scenario name to select from \code{df}
#' @param scen target scenario name to relabel to
#'
#' @importFrom dplyr filter mutate %>% .data

.prepScenInput <- function(df, cfgScen, scen) {
  df %>%
    filter(.data[["scenario"]] == cfgScen) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    missingToNA()
}


#' Extract the global regression coefficients
#'
#' The input carries the coefficients of a single global regression, stored as
#' \code{variable} entries suffixed with the floor space type (e.g.
#' \code{"(Intercept)_residential"}).
#'
#' @param floorspaceCoefs data.frame global regression coefficients, with the
#'   coefficient name in \code{variable} and its value in \code{value}
#' @param varName floor space type (\code{"residential"} or \code{"commercial"})
#' @param predictors character vector of predictor columns, must include
#'   \code{"gdppop"}
#'
#' @returns named list of scalar coefficients \code{intercept},
#'   \code{elasIncome} and, for the residential model, \code{elasDensity}
#'
#' @importFrom dplyr filter %>% .data

.extractCoefficients <- function(floorspaceCoefs, varName, predictors) {

  # map the suffixed input variables of this floor space type to coefficient names
  coefMap <- c("(Intercept)"  = "intercept",
               "log(gdppop)"  = "elasIncome",
               "log(density)" = "elasDensity")
  names(coefMap) <- paste0(names(coefMap), "_", varName)

  coefs <- floorspaceCoefs %>%
    filter(.data[["variable"]] %in% names(coefMap))
  values <- stats::setNames(coefs[["value"]], coefMap[coefs[["variable"]]])

  # a missing density coefficient contributes zero (income-only model)
  list(
    intercept   = values[["intercept"]],
    elasIncome  = values[["elasIncome"]],
    elasDensity = if ("density" %in% predictors) values[["elasDensity"]] else 0
  )
}


#' Apply the floor space regression coefficients
#'
#' Attaches the global regression coefficients to the regression frame, computes
#' the fitted per-capita values and the region-specific log-residual at the last
#' year with historic data. The model is selected purely through
#' \code{predictors}, so the same code path serves the residential (income +
#' density) and commercial (income only) models.
#'
#' @param fullData regression frame with columns \code{m2cap} and the predictors
#' @param predictors character vector of predictor columns, must include
#'   \code{"gdppop"}
#' @param coefs global regression coefficients, see \code{.extractCoefficients}
#'
#' @returns list with the augmented data (carrying the coefficient columns), the
#'   predictor set and the last year with historic data
#'
#' @importFrom dplyr filter mutate group_by ungroup across all_of %>% .data

.projectFloorspace <- function(fullData, predictors, coefs) {

  hasDensity <- "density" %in% predictors

  # last year with historic floor space
  modelData <- filter(fullData, !is.na(.data[["m2cap"]]))
  endOfHistory <- max(modelData[["period"]]) # assumes uniform data availability across regions

  # attach the global coefficients and compute fitted per-capita floor space
  # and the region-specific deviation at the last historic year
  fullData <- fullData %>%
    mutate(intercept   = coefs[["intercept"]],
           elasIncome  = coefs[["elasIncome"]],
           elasDensity = coefs[["elasDensity"]],
           densityTerm = if (hasDensity) .data[["elasDensity"]] * log(.data[["density"]]) else 0,
           m2capFitted = exp(.data[["intercept"]] + .data[["elasIncome"]] * log(.data[["gdppop"]]) +
                               .data[["densityTerm"]])) %>%
    group_by(across(all_of("region"))) %>%
    mutate(logResidual = log(.data[["m2cap"]][.data[["period"]] == endOfHistory]) -
             log(.data[["m2capFitted"]][.data[["period"]] == endOfHistory])) %>%
    ungroup() %>%
    select(-"densityTerm")

  list(data = fullData,
       predictors = predictors,
       endOfHistory = endOfHistory)
}


#' Project floor space incorporating scenario assumptions
#'
#' Builds the floor space projection from the fitted model. The income
#' elasticity is gradually modified according to scenario assumptions; as this
#' modified elasticity only acts on income growth, the projection is corrected
#' by a growth term (equivalent to the underlying iterative formulation). The
#' region-specific deviation \code{logResidual} converges towards the regression
#' via lambda. When \code{capFloor} is supplied (residential only), an upper cap
#' is applied: if historic floor space already exceeds the cap it is gradually
#' decreased towards it, otherwise the minimum of projection and cap is taken.
#'
#' @param model output of \code{.projectFloorspace}
#' @param scenAssump scenario parameter assumptions with an income-elasticity
#'   modifier \code{floorspace}
#' @param lambda temporal convergence factors
#' @param endOfHistory last historic time period
#' @param capFloor country-wise upper floor space boundary, or \code{NULL}
#'
#' @returns data.frame with per-capita projected floor space (\code{m2capProjected})
#'
#' @importFrom dplyr filter mutate select arrange group_by ungroup lag left_join
#'   bind_rows across all_of %>% .data

.incorporateScenarioAssumptions <- function(model, scenAssump, lambda, endOfHistory,
                                            capFloor = NULL) {

  hasDensity <- "density" %in% model$predictors

  # gradually modify the income elasticity (income term only); the base
  # elasticity is the per-region regression coefficient carried on model$data
  fullData <- model$data %>%
    filter(.data[["period"]] %in% unique(lambda[["period"]])) %>%
    left_join(scenAssump, by = c("region", "scenario")) %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(elasIncome = .data[["fullconv"]] * (.data[["elasIncome"]] * .data[["floorspace"]]) +
             (1 - .data[["fullconv"]]) * .data[["elasIncome"]])

  # historic part: keep observed per-capita floor space
  histData <- fullData %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    mutate(m2capRaw = .data[["m2cap"]])

  # projected part: regression estimate + deviation + growth correction
  projData <- fullData %>%
    arrange(.data[["period"]]) %>%
    group_by(across(all_of("region"))) %>%
    filter(.data[["period"]] >= endOfHistory) %>% # need last historic step and all after it
    mutate(densityTerm = if (hasDensity) .data[["elasDensity"]] * log(.data[["density"]]) else 0,
           elasIncomeDiff = c(-diff(.data[["elasIncome"]]), NA),
           growthCorr = lag(cumsum(.data[["elasIncomeDiff"]] * log(.data[["gdppop"]]))),
           m2capRaw = exp(.data[["growthCorr"]] + .data[["logResidual"]] + .data[["intercept"]] +
                            log(.data[["gdppop"]]) * .data[["elasIncome"]] +
                            .data[["densityTerm"]])) %>%
    ungroup() %>%
    filter(.data[["period"]] > endOfHistory) %>%
    select(-"densityTerm", -"elasIncomeDiff", -"growthCorr")

  # combine and converge the deviation towards the regression
  projection <- bind_rows(histData, projData) %>%
    mutate(m2capProjected = .data[["m2capRaw"]] / exp(.data[["lambda"]] * .data[["logResidual"]]))

  # apply the upper cap (residential only)
  if (!is.null(capFloor) && is.data.frame(capFloor)) {
    histCap <- filter(projection, .data[["period"]] <= endOfHistory)

    projection <- projection %>%
      left_join(capFloor, by = "region") %>%
      group_by(across(all_of("region"))) %>%

      # excess over the cap at the last historic year
      mutate(capExcess = max(0, .data[["m2capProjected"]][.data[["period"]] == endOfHistory] -
                               .data[["capFloor"]])) %>%
      filter(.data[["period"]] > endOfHistory) %>%

      # cap the projection or, if already exceeded, decrease towards the cap
      mutate(m2capProjected = ifelse(.data[["capExcess"]] == 0,
                                     pmin(.data[["capFloor"]], .data[["m2capProjected"]]),
                                     .data[["capFloor"]] + .data[["capExcess"]] *
                                       (1 - .data[["lambda"]]))) %>%
      ungroup() %>%
      select(-"capFloor", -"capExcess") %>%
      bind_rows(histCap)
  }

  projection %>%
    select("scenario", "region", "period", "pop", "m2capProjected")
}
