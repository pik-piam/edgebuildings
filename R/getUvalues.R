#' Historic and projected conductivity of building shell
#'
#' Historic data for conductivity of building shell by the EU Buildings Observatory
#' and IEA ETSAP along with historic and projected data on GDP per capita for
#' various scenarios is used to calibrate a regression model to project the
#' scenario-wise evolution of conductivity values.
#'
#' Average U-values are estimated as the arithmetic mean of floor, walls, windows
#' and roofs U-values, implying an assumption of equal surface of each of these
#' buildings' compomnents.
#'
#' As an estimation model we assume an exponential relation between the sum of
#' annual degree days (HDD/CDD) and the U-value. As the wished level of
#' insulation might be unaffordable for low-income households/businesses, we multiply
#' the U-value computed by the previous equation by a coefficient decreasing from
#' two to one when the per capita income of the region considered moves from US$0 to US$15000.
#'
#' @param config scenario-wise parameter configuration
#' @param uvaluesResCom residential and commercial uvalue data
#' @param uvaluesETSAP uvalue data
#' @param gdppop gdp per capita
#' @param hddcdd hdd cdd data
#' @param pop population data
#' @param regionalmap regional mapping
#' @param scenAssumpSpeed scenario-specific upper temporal evolution boundary
#'
#' @returns data.frame
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate left_join %>% .data
#' @importFrom quitte calc_addVariable_ removeColNa
#' @importFrom tidyr spread
#' @importFrom zoo rollmean
#'
#' @export

getUvalues <- function(config,
                       uvaluesResCom,
                       uvaluesETSAP,
                       gdppop,
                       hddcdd,
                       pop,
                       regionalmap,
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

  # Walls maximum Uvalues * 75% + windows max uvalues * 25%
  maxUValue <- 2 * 0.75 + 5 * 0.25

  # READ-IN DATA----------------------------------------------------------------

  regionmap <- regionalmap[, c("countryname", "iso")]

  # PRE-PROCESS DATA------------------------------------------------------------

  #--- Data is made compliant with config file

  # gdppop
  gdppop <- gdppop %>%
    filter(.data[["variable"]] == config[, "gdppopScen"]) %>%
    unique() %>%
    sepVarScen() %>%
    mutate(scenario = scen)


  # heating/cooling degree days
  hddcdd <- hddcdd %>%
    filter(.data[["scenario"]] == config[, "uvalueScen"]) %>%
    mutate(scenario = scen)

  # population
  pop <- pop %>%
    filter(.data[["variable"]] == config[, "popScen"]) %>%
    unique() %>%
    sepVarScen() %>%
    mutate(scenario = scen)


  # scenario convergence speed assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)


  # minimal U-value
  scenAssumpFactor <- config[scen, "econCoef_uvalues_X_min"] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionalmap) %>%
    rename(scenAssumpFactor = "value")


  # calculate temporal convergence shares
  lambda <- compLambdaScen(scenAssumpSpeed, startYearVector = periodBegin)



  # PROCESS DATA----------------------------------------------------------------

  # join datasets
  uvaluesResCom <- uvaluesResCom %>%
    filter(!is.na(.data$value)) %>%
    left_join(regionmap, by = c("region" = "countryname")) %>%
    select(-"region", -"variable", -"period") %>%
    rename("region" = "iso",
           "uvalue" = "value") %>%
    filter(!is.na(.data$region))


  regionSelection <- c("CHN", "USA")


  uvaluesETSAP <- uvaluesETSAP %>%
    filter(!is.na(.data$value), .data$region %in% regionSelection) %>%
    select("region", "uvalue" = "value")

  uvalues <- rbind(uvaluesResCom, uvaluesETSAP)

  # prepare and mean degree day data
  hddcddHist <- hddcdd %>%
    filter(.data[["period"]] <= endOfHistory)

  hddcddScen <- hddcdd %>%
    filter(.data[["period"]] > endOfHistory)

  nMean <- periodBegin - min(hddcddHist$period) + 1

  hddcddHist <- hddcddHist %>%
    group_by(across(all_of(c("region", "variable", "scenario")))) %>%
    mutate(value = c(rep(NA, nMean - 1),
                     rollmean(.data[["value"]], nMean, align = "right"))) %>%
    ungroup() %>%
    filter(!is.na(.data[["value"]]))

  hddcdd <- rbind(hddcddHist, hddcddScen)

  hddcdd <- hddcdd %>%
    # make periods congruent
    filter(.data[["period"]] %in% getPeriods(lambda)) %>%

    # sum up degree days
    calc_addVariable_(list("HDDCDD" = "HDD + CDD"), only.new = TRUE)



  # set minimum U-value
  minUValue <- 3 / 4 * min(uvalues$uvalue)


  # prepare the HDD and CDD data for the estimation
  hddcddEstimate <- hddcdd %>%
    spread(key = "variable", value = "value") %>%
    filter(.data[["period"]] == endOfHistory) %>%
    removeColNa()


  # UTrans serves as estimation variable
  dataEstimate <- uvalues %>%
    mutate(UTrans = log(.data[["uvalue"]] - minUValue)) %>%
    left_join(hddcddEstimate, by = c("region")) %>%
    filter(!is.na(.data[["region"]]))


  # check regional resolution matching
  if (!any(getRegs(uvalues) %in% getRegs(hddcdd))) {
    stop("Region mismatch between UValues and HDDCDD data. Choose different regional
         resolution for HDDCDD data or add ISO dataset.")
  }


  # adapt hddcdd to time periods
  hddcdd <- hddcdd %>%
    spread(key = "variable", value = "value") %>%
    filter(.data[["period"]] >= getPeriods(dataEstimate))


  # Correct outliers
  dataEstimate <- dataEstimate %>%
    filter(!.data[["region"]] %in% c("PRT", "MLT"))


  # make the estimation and the prediction
  datPred <- makeUValuePrediction(dataEstimate, hddcdd, "UTrans ~ HDDCDD")


  # add scenario assumptions
  tmp <- datPred %>%
    implementScenAssump(scenAssumpFactor, lambda, minUValue, endOfHistory) %>%
    filter(!is.na(.data[["value"]])) %>%
    correctRegions(lambda, endOfHistory)


  # linearise change to avoid that uvalue estimates reproduce the climate variability
  tmp <- tmp %>%
    lineariseChange(periodBegin, endOfHistory) %>%
    includeInputDataAndCorrect(dataEstimate, gdppop, lambda, endOfHistory) %>%
    mutate(value = ifelse(.data[["variable"]] == "uvalue",
                          pmin(maxUValue, .data[["value"]]),
                          .data[["value"]]))



  # OUTPUT----------------------------------------------------------------------

  data <- tmp %>%
    filter(.data[["variable"]] == "uvalue") %>%
    uValueCannotIncrease(.data[["value"]])

  return(data)
}



# INTERNAL FUNCTIONS------------------------------------------------------------

#' Make prediction of future U-Values
#'
#' Future U-values are predicted by assuming an exponential relationship between
#' the sum of annual degree days (HDD + CDD) and the heat conduction value of the
#' outer building shell. Further remarks on the assumptions are in the description
#' of \code{getUvalues}.
#'
#' @param dataEst estimate for prediction
#' @param dataPred future degree day values for Uvalue prediction
#' @param formul correlation formular for variables of interest
#'
#' @return scenario-wise U-value predictions
#'
#' @importFrom stats lm predict
#' @importFrom tidyr gather

makeUValuePrediction <- function(dataEst, dataPred, formul) {
  # extract parameters
  formul <- as.formula(formul)
  vars   <- all.vars(formul)

  # make regression
  estimate <- lm(formul, dataEst)

  # predict new uvalues
  dataPred$UTrans <- predict(estimate, newdata = dataPred)

  dataPred <- gather(dataPred, key = "variable", value = "value", vars)

  return(dataPred)
}



#' Implement scenario assumptions on U-value temporal evolution
#'
#' @param tmp data.frame with historic and projected U-values
#' @param scenAssumpFactor region-wise scenario value assumptions
#' @param lambda temporal convergence shares
#' @param minU minimum U-value
#' @param endOfHistory upper temporal boundary of historical data
#'
#' @return corrected future U-values
#'
#' @importFrom dplyr mutate left_join select
#' @importFrom tidyr spread gather
#' @importFrom quitte getVars

implementScenAssump <- function(tmp, scenAssumpFactor, lambda, minU, endOfHistory) {
  keepCols <- setdiff(c(colnames(tmp)), c("variable", "value"))
  keepVars <- c(getVars(tmp), "uvalue")

  # compute proj as linear combination of regression result and scenario modified parameters
  tmp <- tmp %>%
    spread(key = "variable", value = "value") %>%
    mutate(minProj = minU,
           proj = exp(.data[["UTrans"]]) + .data[["minProj"]])

  tmp <- tmp %>%
    uValueCannotIncrease(.data[["proj"]]) %>%
    left_join(scenAssumpFactor, by = "region") %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(scen = ifelse(.data[["period"]] > endOfHistory,
                         .data[["proj"]] * .data[["scenAssumpFactor"]],
                         .data[["proj"]]),
           uvalue  =  .data[["scen"]] * .data[["fullconv"]] +
             .data[["proj"]] * (1 - .data[["fullconv"]])) %>%
    select(-"lambda", -"fullconv")

  # process output
  res <- tmp[c(keepCols, keepVars)] %>%
    gather(key = "variable", value = "value", keepVars)

  return(res)
}


#' Linearise U-value temporal evolution
#'
#' Smoothing the temporal evolution of future U-values to neglect the impact
#' of variability of degree daysby doing a linear interpolation of uvalues in the
#' time period of \code{endOfHistory} and 2100.
#'
#' @param df data.frame with U-value projections
#' @param endOfHistory upper temporal boundary of historical data
#' @param periodBegin lower temporal boundary of historical data
#'
#' @return linearised U-values
#'
#' @importFrom dplyr select filter mutate group_by across all_of inner_join
#' @importFrom quitte interpolate_missing_periods

lineariseChange <- function(df, periodBegin, endOfHistory) {
  # define time periods
  yearsFinal <- seq(2080, 2100, 5)
  scenPeriod <- c(periodBegin:endOfHistory,
                  seq(endOfHistory + 5, 2100, 5))

  regScen <- df %>%
    select("region", "scenario") %>%
    unique()

  tmp <- df %>%
    filter(.data[["variable"]] == "uvalue") %>%
    mutate(scenario = .data[["scenario"]])

  tmpBack <- df %>%
    filter(.data[["variable"]] != "uvalue")

  tmpHist <- tmp %>%
    filter(.data[["period"]] == endOfHistory)

  tmpPast <- tmp %>%
    filter(.data[["period"]] < endOfHistory)

  tmpFinal <- tmp %>%
    filter(.data[["period"]] %in% yearsFinal) %>%
    group_by(across(all_of(c("region", "scenario")))) %>%
    mutate(value = mean(.data[["value"]])) %>%
    filter(.data[["period"]] == 2100) %>%
    ungroup()

  tmpRes <- rbind(tmpHist, tmpFinal) %>%
    interpolate_missing_periods(period = scenPeriod,
                                expand.values = TRUE) %>%
    inner_join(regScen, by = c("region", "scenario"))

  return(rbind(tmpBack, tmpPast, tmpRes))
}



#' Correct U-values for regional historical deviations
#'
#' @param df data.frame with historical and future U-values
#' @param lambda temporal convergence shares
#' @param endOfHistory upper temporal boundary of historical data
#'
#' @return corrected U-values
#'
#' @importFrom dplyr mutate filter select left_join
#' @importFrom quitte getRegs

correctRegions <- function(df, lambda, endOfHistory) {

  correctVec <- c("RUS" = "LVA")

  scenAssumpReg <- data.frame(region = getRegs(df), stringsAsFactors = FALSE) %>%
    mutate(regionTarget = ifelse(.data[["region"]] %in% names(correctVec),
                                 correctVec[.data[["region"]]],
                                 .data[["region"]]))

  tmpHist <- df %>%
    filter(.data[["period"]] == endOfHistory,
           .data[["variable"]] == "uvalue") %>%
    select("region", "value")

  tmpHistDelta <- tmpHist %>% rename(regionTarget = "region",
                                     valueTarget  = "value")

  tmpDelta <- tmpHist %>%
    left_join(scenAssumpReg, by = "region") %>%
    left_join(tmpHistDelta, by = "regionTarget") %>%
    mutate(delta = .data[["value"]] - .data[["valueTarget"]]) %>%
    select("region", "delta")

  tmp <- df %>%
    left_join(tmpDelta, by = "region") %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(value = ifelse(.data[["variable"]] == "uvalue",
                          .data[["value"]] - .data[["delta"]] * (1 - .data[["fullconv"]]),
                          .data[["value"]])) %>%
    select(-"delta", -"lambda", -"fullconv")

  return(tmp)
}


includeInputDataAndCorrect <- function(dfScen, dfData, gdp, lambda, endOfHistory) {

  dfData <- dfData %>% select("uvalue", "region", "period")

  # Take the mean of the commercial and residential uvalues
  dfData <- dfData %>%
    group_by(across(all_of(c("region", "period")))) %>%
    reframe(uvalue = mean(.data[["uvalue"]], na.rm = TRUE)) %>%
    ungroup()

  # Extend the historical data set
  dfData <- dfData %>%
    interpolate_missing_periods(period = getPeriods(dfScen),
                                expand.values = TRUE,
                                value = "uvalue")

  tmpKeep <- dfScen %>%
    filter(.data[["variable"]] != "uvalue") %>%
    select("region", "period", "scenario", "variable", "value")

  gdp <- gdp %>%
    select("scenario", "period", "region", "variable", "value") %>%
    spread(key = "variable", value = "value")

  tmp <- dfScen %>%
    filter(.data[["variable"]] == "uvalue") %>%
    left_join(dfData, by = c("region", "period")) %>%
    left_join(gdp, by = c("scenario", "period", "region"))

  # linear combination between scenario trajectories and historical values
  multiUValues <- 1.5

  tmp <- tmp %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    group_by(across(all_of("region"))) %>%
    mutate(uvalue = ifelse(is.na(.data[["uvalue"]]),
                           pmax(.data[["value"]],
                                (multiUValues - ((multiUValues - 1) / 20000) *
                                   .data[["gdppop"]]) * .data[["value"]]),
                           .data[["uvalue"]]),
           maxFactor = pmax(0, pmin(1, (20000 - .data[["gdppop"]][.data[["period"]] == endOfHistory]) / 20000)),
           factor = .data[["maxFactor"]] - pmax(0, pmin(1, (20000 - .data[["gdppop"]]) / 20000)),
           uvalue = .data[["uvalue"]] * (1 - .data[["factor"]] / multiUValues),
           delta = .data[["uvalue"]] - .data[["value"]],
           value = .data[["value"]] + .data[["delta"]] * (1 - .data[["fullconv"]])) %>%
    ungroup() %>%
    select("region", "period", "scenario", "variable", "value")

  return(rbind(tmpKeep, tmp))
}


#' cap uvalue
#'
#' @param df data.frame
#' @param col column name
#'
#' @importFrom dplyr arrange group_by across all_of mutate ungroup enquo
#'   quo_name n
#' @importFrom data.table :=

uValueCannotIncrease <- function(df, col) {
  quoCol <- enquo(col)
  quoColName <- quo_name(quoCol)

  fun <- function(val, nb) {
    do.call(pmin,
            c(lapply(0:(nb - 1), lag, x = val), na.rm = TRUE))
  }

  tmp <- df %>%
    arrange(.data[["period"]]) %>%
    group_by(across(all_of(c("region")))) %>%
    mutate(!!quoColName := fun(!!quoCol, n())) %>%
    ungroup()
  return(tmp)
}
