#' Project future values from historical data using linear and non-linear regressions
#'
#' @param df \code{data.frame} Input data containing the historical data
#' @param formul \code{formula} Projection model formula
#' @param scenAssump \code{data.frame} Economic scenario assumptions for projections
#' @param lambda \code{data.frame} Convergence factors (0 to 1) over time for scenario transitions
#' @param lambdaDelta \code{data.frame} Convergence factors (0 to 1) over time for delta targets
#' @param maxReg \code{numeric} Maximum allowed value for projections (optional)
#' @param outliers \code{character} Vector of region names to exclude from regression (optional)
#' @param apply0toNeg \code{logical} Replace negative projections with zero (default TRUE)
#' @param transformVariable \code{character} Variable transformation to apply (optional)
#' @param transformVariableScen \code{character} Scenario-specific variable transformation (optional)
#' @param applyScenFactor \code{logical} Apply scenario scaling factor (default FALSE)
#' @param convReg \code{character} Convergence type: "absolute" for value differences or "proportion" for ratios
#' @param avoidLowValues \code{logical} Prevent projections falling below initial/final values (default FALSE)
#' @param endOfHistory \code{numeric} Last year of historical data (default 2020)
#' @param interpolate \code{logical} Use all historical points vs only latest (default TRUE)
#' @param initCorrection \code{data.frame} Corrected parameters for non-scenario projections (optional)
#' @param replacePars \code{logical} Replace fit parameters with scenario assumptions (optional)
#'
#' @return \code{data.frame} Combined input data and calculated projections
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte mutate_text
#' @importFrom dplyr rename_ select if_else case_when cur_data
#' @importFrom tidyselect any_of
#' @importFrom stats lm nls coef
#' @importFrom rlang parse_expr sym


makeProjections <- function(df,
                            formul,
                            scenAssump,
                            lambda,
                            lambdaDelta,
                            maxReg = NULL,
                            outliers = NULL,
                            apply0toNeg = TRUE,
                            transformVariable = NULL,
                            transformVariableScen = NULL,
                            applyScenFactor = FALSE,
                            convReg = "absolute",
                            avoidLowValues = FALSE,
                            endOfHistory = 2025,
                            interpolate = TRUE,
                            initCorrection = NULL,
                            replacePars = FALSE) {

  # FUNCTIONS-------------------------------------------------------------------

  # transform variables
  transVar <- function(dfTrans, form, var) {
    form <- gsub("VAR", var, form)
    return(dfTrans %>% mutate_(.dots = setNames(list(interp((form))), var)))
  }



  # PARAMETERS------------------------------------------------------------------

  cols <- colnames(df)

  # left-hand side of regression formula
  lhs  <- all.vars(formul[[2]])

  # right-hand side of regression formula
  rhs  <- all.vars(formul[[3]])

  if (!is.null(transformVariableScen)) {
    transformVariableScen <- transformVariableScen[1]
  }



  # PROCESS DATA----------------------------------------------------------------

  #--- Process input data

  # Prepare data by removing NAs and extracting variables needed for formula
  fullData <- df %>%
    mutate(unit = NA) %>%
    removeColNa() %>%
    filter(.data[["variable"]] %in% all.vars(formul)) %>%
    spread(key = "variable", value = "value")

  # Split historical and projection data
  historicalData <- fullData %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    filter(!is.na(.data[[lhs]]))

  # For projection: if no RHS variables, use unique combinations, otherwise use full data
  projectionData <- if (length(rhs) == 0) {
    df %>%
      select("scenario", "region", "period") %>%
      unique()
  } else {
    fullData %>% removeColNa()
  }

  # Filter to relevant projection periods
  projectionData <- projectionData %>%
    filter(.data[["period"]] %in% getPeriods(lambda))



  #--- Model estimation

  # Remove outliers from historical data for model estimation
  modelData <- historicalData %>%
    filter(!(.data[["region"]] %in% outliers))

  # If not interpolating, use only most recent observation per region
  if (!interpolate) {
    modelData <- modelData %>%
      group_by(across(all_of("region"))) %>%
      filter(rank(.data[["period"]]) == max(rank(.data[["period"]]))) %>%
      ungroup()
  }

  # Create either non-linear or linear model based on formula structure
  estimate <- if (any(grepl("\\SS", formul))) {
    # Replace zeros with small values to avoid infinity issues in non-linear model
    modelData[modelData[lhs] == 0, lhs] <- min(modelData[modelData[lhs] != 0, lhs]) / 10

    nls(formul, modelData, control = list(maxiter = 500), trace = FALSE)
  } else {
    lm(formul, modelData)
  }


  #--- Initial parameter corrections
  if (!is.null(initCorrection) && (lhs %in% unique(initCorrection$variable))) {
    initCorrection <- initCorrection[initCorrection$variable == lhs, ]
    estimateParameters <- estimate$m$getPars()

    # create list of parameters (corrected where applicable)
    for (param in names(estimate$m$getPars())) {
      if (!is.na(initCorrection[[param]])) {
        estimateParameters[param] <- as.numeric(initCorrection[[param]])
      }
    }
    # update parameters
    invisible(estimate$m$setPars(estimateParameters))
  }


  #--- Projections and incorporation of scenario assumptions

  # Projections with uncorrected global fit (historical continuation)
  projectionData$projectionReg <- predict(estimate, newdata = projectionData)


  # Apply regional maximum if specified
  if (!is.null(maxReg)) {
    projectionData$projectionReg <- pmin(maxReg, projectionData$projectionReg)
  }


  # Projections with scenario-specific assumptions
  projectionData <- makeScenarioProjections(data = projectionData,
                                            fitModel = estimate,
                                            scenAssump,
                                            lhs,
                                            transformVariableScen,
                                            applyScenFactor,
                                            replacePars)


  # Merge scenario projections with regression projections
  projectionData <- projectionData %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(projection = .data[["projectionScen"]] * .data[["fullconv"]] +
             .data[["projectionReg"]] * (1 - .data[["fullconv"]])) %>%
    select(-c("lambda", "fullconv"))


  # Fill missing gaps in historical data
  historicalData <- fillHistoricalData(data         = fullData,
                                       estimate     = estimate,
                                       endOfHistory = endOfHistory,
                                       var          = lhs)

  # Make predictions with global fit
  historicalData$prediction <- predict(estimate, newdata = historicalData)


  #--- Post-processing transformations

  # Apply variable transformations if specified
  if (!is.null(transformVariable)) {
    for (col in c(lhs, "prediction")) {
      historicalData <- transVar(historicalData, transformVariable, col)
    }
    for (col in c("projectionReg", "projectionScen", "projection")) {
      projectionData <- transVar(projectionData, transformVariable, col)
    }
  } else if (!is.null(transformVariableScen)) {
    for (col in c("prediction", lhs)) {
      historicalData <- transVar(historicalData, transformVariableScen, col)
    }
  }


  #--- Smooth out transition between history and projections (aka minimize deltas)

  # Set up delta calculation based on convergence type
  if (convReg == "absolute") {
    deltaFormula           <- paste0("delta = -prediction + ", lhs)
    projectionFinalFormula <- "projectionFinal = projection + deltaFinal"
    deltaGlobalTarget      <- 0
  } else if (convReg == "proportion") {
    deltaFormula           <- paste0("delta = (1/prediction) * ", lhs)
    projectionFinalFormula <- "projectionFinal = projection * deltaFinal"
    deltaGlobalTarget      <- 1
  } else {
    stop("convReg must be in c('absolute','proportion')")
  }

  deltaFinal <- "deltaFinal = deltaTarget * lambda + delta * (1 - lambda)"


  projectionDeltas <- historicalData %>%
    # filter last historical data point
    filter(.data[["period"]] == endOfHistory) %>%

    # determine deviation between projections and last historical point
    mutate_text(deltaFormula) %>%
    select("region", "delta")


  projectionData <- projectionData %>%
    # join relevant data
    left_join(projectionDeltas, by = "region") %>%
    left_join(lambdaDelta, by = c("region", "period", "scenario")) %>%

    # define lambda target value
    mutate(deltaTarget = deltaGlobalTarget) %>%

    # determine progression of delta values with transition factors lambda
    mutate_text(deltaFinal) %>%

    # correct projections w/ appropriate deltas
    mutate_text(projectionFinalFormula) %>%

    # clean up
    select(-c("fullconv", "lambda"))



  #--- Special handling for space cooling

  if (lhs == "space_cooling_m2_CDD_Uval") {
    # Handle non-outlier regions normally
    projectionNonOutliers <- projectionData %>%
      filter(!(.data[["region"]] %in% outliers))

    # For outlier regions, use growth rates instead of absolute values
    projectionOutliers <- projectionData %>%
      filter(.data[["region"]] %in% outliers) %>%
      group_by(across(all_of("region"))) %>%
      tidyr::fill(.data[[lhs]], .direction = "down") %>%
      mutate(
        previousScenario = lag(.data[["projectionScen"]]),
        growthRate = ifelse(.data[["period"]] <= 2020,
                            1,
                            c(0, .data[["projectionScen"]] / .data[["previousScenario"]])),
        growthRate = cumprod(.data[["growthRate"]]),
        projectionFinal = .data[[lhs]] * .data[["growthRate"]]
      ) %>%
      ungroup() %>%
      select(-c("growthRate", "previousScenario"))

    projectionData <- rbind(projectionOutliers, projectionNonOutliers)
  }


  #--- Final value adjustments

  # Replace negative values with zero if specified
  if (apply0toNeg) {
    negativeValues <- projectionData %>%
      filter(.data[["projectionFinal"]] < 0) %>%
      select("period", "region", "scenario") %>%
      unique()

    if (nrow(negativeValues) > 0) {
      cat("Negative values replaced with 0\n")
      print(as.data.frame(negativeValues))
      projectionData <- projectionData %>%
        mutate(projectionFinal = pmax(0, .data[["projectionFinal"]]))
    }
  }

  # Apply minimum value constraints if specified
  if (avoidLowValues) {
    projectionData <- projectionData %>%
      group_by(across(all_of(c("scenario", "region")))) %>%
      mutate(
        # First check: values shouldn't exceed 2100 projections
        projectionFinal = case_when(
          scenario == "history" ~ projectionFinal,
          any(projectionFinal[period == endOfHistory + 5] >= projectionScen[period == 2100]) ~
            pmax(projectionFinal, projectionScen[period == 2100][1]),
          TRUE ~ projectionFinal
        )
      ) %>%
      mutate(
        # Second check: values shouldn't fall below initial projections
        projectionFinal = case_when(
          scenario == "history" ~ projectionFinal,
          any(projectionFinal[period == endOfHistory + 5] <= projectionScen[period == 2100]) ~
            pmax(projectionFinal, projectionFinal[period == endOfHistory + 5][1]),
          TRUE ~ projectionFinal
        )
      ) %>%
      ungroup()
  }


  # Prepare historical data for output
  historicalData <- historicalData %>%
    pivot_longer(cols = all_of(lhs), names_to = "variable", values_to = "value") %>%
    select("region", "period", "scenario", "variable", "value")



  # OUTPUT----------------------------------------------------------------------

  keepColumns <- c(intersect(cols, colnames(projectionData)), "projectionFinal")

  # Final dataset with (filled/existing) historical data and scenario projections
  finalProjections <- projectionData %>%
    # add projections
    select(one_of(keepColumns)) %>%
    rename(!!sym(lhs) := "projectionFinal") %>%
    gather("variable", "value", one_of(lhs)) %>%
    filter(.data[["period"]] > endOfHistory) %>%

    # add filled historical data
    rbind(historicalData) %>%

    as.quitte() %>%
    missingToNA()

  # Prepare output data frame
  df <- rbind(finalProjections,
              df %>%
                filter(.data[["variable"]] != lhs) %>%
                as.quitte())

  return(df)
}
