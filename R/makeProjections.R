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
#' @param endOfHistory \code{numeric} Last year of historical data
#' @param periodBegin \code{numeric} First year of historical data
#' @param interpolate \code{logical} Use all historical points vs only latest (default TRUE)
#' @param initCorrection \code{data.frame} Corrected parameters for non-scenario projections (optional)
#' @param replacePars \code{logical} Replace fit parameters with scenario assumptions (optional)
#'
#' @return \code{data.frame} Combined input data and calculated projections
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom dplyr group_by filter ungroup mutate select left_join rename
#'             mutate_ case_when across slice_max
#' @importFrom tidyr fill spread gather pivot_longer
#' @importFrom stats nls lm predict coef
#' @importFrom rlang parse_expr sym .data
#' @importFrom quitte as.quitte mutate_text
#' @importFrom tidyselect any_of all_of one_of
#' @importFrom magrittr %>%


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
                            periodBegin = 1990,
                            interpolate = TRUE,
                            initCorrection = NULL,
                            replacePars = FALSE) {


  # PARAMETERS------------------------------------------------------------------

  cols <- colnames(df)

  # left-hand side of regression formula
  lhs  <- all.vars(formul[[2]])

  # right-hand side of regression formula
  rhs  <- all.vars(formul[[3]])

  if (!is.null(transformVariableScen)) {
    transformVariableScen <- transformVariableScen[1]
  }

  if (is.null(endOfHistory)) {
    endOfHistory <- max(unique(filter(df, .data[["scenario"]] == "history")[["period"]]))
  }


  # PROCESS DATA----------------------------------------------------------------

  #--- Process input data

  # Prepare data by removing NAs and extracting variables needed for formula
  fullData <- df %>%
    mutate(unit = NA) %>%
    filter(.data[["variable"]] %in% all.vars(formul)) %>%
    spread(key = "variable", value = "value")

  # Split historical and projection data
  historicalData <- fullData %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    filter(!is.na(.data[[lhs]])) %>%
    removeColNa()

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
    filter(.data[["period"]] %in% getPeriods(lambda)) %>%
    filter(.data[["period"]] > endOfHistory)



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
  projectionData <- .makeScenarioProjections(data                  = projectionData,
                                             fitModel              = estimate,
                                             scenAssump            = scenAssump,
                                             lhs                   = lhs,
                                             transformVariableScen = transformVariableScen,
                                             applyScenFactor       = applyScenFactor,
                                             replacePars           = replacePars)


  # Merge scenario projections with regression projections
  projectionData <- projectionData %>%
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(projection = .data[["projectionScen"]] * .data[["fullconv"]] +
             .data[["projectionReg"]] * (1 - .data[["fullconv"]])) %>%
    select(-c("lambda", "fullconv"))



  #--- Fill historical data

  # Fill missing gaps in historical data
  historicalData <- .fillHistoricalData(data                  = fullData,
                                        estimate              = estimate,
                                        var                   = lhs,
                                        periodBegin           = periodBegin,
                                        endOfHistory          = endOfHistory,
                                        convReg               = convReg,
                                        transformVariable     = transformVariable,
                                        transformVariableScen = transformVariableScen)



  #--- Post-processing transformations

  # Apply variable transformations if specified
  if (!is.null(transformVariable)) {
    for (col in c("projectionReg", "projectionScen", "projection")) {
      projectionData <- .transVar(projectionData, transformVariable, col)
    }
  }



  #--- Smooth out transition between history and projections (aka minimize deltas)

  # Get correction formulas to smooth deviations (delta) between empirical data and projections
  deltaFormulas <- .getDeltaFormulas(lhs = lhs, convReg = convReg, deltaTarget = TRUE)

  # Obtain deltas from last historical data point
  projectionDeltas <- historicalData %>%
    group_by(across(all_of("region"))) %>%
    filter(!is.na(.data[["delta"]])) %>%
    slice_max(.data[["period"]], n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select("region", "delta")


  # correct scenario projections
  projectionData <- projectionData %>%
    # join relevant data
    left_join(projectionDeltas, by = "region") %>%
    left_join(lambdaDelta, by = c("region", "period", "scenario")) %>%

    # define lambda target value
    mutate(deltaTarget = deltaFormulas[["deltaGlobalTarget"]]) %>%

    # determine progression of delta values with transition factors lambda
    mutate_text(deltaFormulas[["deltaFinal"]]) %>%

    # correct projections w/ appropriate deltas
    mutate_text(deltaFormulas[["projectionFinalFormula"]]) %>%

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
        growthRate = ifelse(.data[["period"]] <= endOfHistory,
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



  #--- Merge existing historical data with projections

  keepColumns <- c(intersect(cols, colnames(projectionData)), lhs, "projectionFinal")

  # If length(rhs) == 0, projectionData has no lhs column, so we add this as NAs
  if (!lhs %in% colnames(projectionData)) projectionData[lhs] <- NA

  # Final dataset with (filled/existing) historical data and scenario projections
  combinedData <- rbind(historicalData %>%
                          select(one_of(keepColumns)),
                        projectionData %>%
                          select(one_of(keepColumns)))


  # Combine existing data with projections
  finalProjections <- combinedData %>%
    mutate(!!lhs := ifelse(is.na(.data[[lhs]]),
                           .data[["projectionFinal"]],
                           .data[[lhs]])) %>%
    select(-"projectionFinal") %>%
    gather("variable", "value", one_of(lhs))



  # OUTPUT----------------------------------------------------------------------

  finalProjections <- finalProjections %>%
    as.quitte() %>%
    missingToNA()

  # Prepare output data frame
  df <- rbind(finalProjections,
              df %>%
                filter(.data[["variable"]] != lhs) %>%
                as.quitte())

  return(df)
}




#' Transform Variables in a Data Frame
#'
#' This function applies a transformation to a specified variable in a data frame.
#'
#' @param dfTrans \code{data.frame} A data frame containing the variable to be transformed.
#' @param form \code{character} A string representing the transformation formula.
#' @param var \code{character} The name of the variable to be transformed.
#'
#' @return \code{data.frame} A data frame with the transformed variable.
#'
#' @importFrom dplyr mutate_
#' @importFrom lazyeval interp

.transVar <- function(dfTrans, form, var) {
  form <- gsub("VAR", var, form)
  return(dfTrans %>% mutate_(.dots = setNames(list(interp((form))), var)))
}




#' Generate Delta Formulas
#'
#' This function generates a list of formulas for calculating deltas and projections
#' based on the specified conversion regime.
#'
#' @param lhs \code{character} A character string representing the left-hand side of the delta formula.
#' @param convReg \code{character} A character string specifying the conversion regime.
#' @param deltaTarget \code{logical} Indicate whether to use a target for the delta calculation..
#'
#' @return \code{list} A list of formulas for calculating deltas and projections.

.getDeltaFormulas <- function(lhs,
                              convReg = "absolute",
                              deltaTarget = TRUE) {
  if (convReg == "absolute") {
    formulas <- list(
      deltaFormula           = paste0("delta = -projection + ", lhs),
      projectionFinalFormula = "projectionFinal = projection + deltaFinal",
      deltaGlobalTarget      = 0
    )
  } else if (convReg == "proportion") {
    formulas <- list(
      deltaFormula           = paste0("delta = (1/projection) * ", lhs),
      projectionFinalFormula = "projectionFinal = projection * deltaFinal",
      deltaGlobalTarget      = 1
    )
  } else {
    stop("convReg must be in c('absolute','proportion')")
  }

  formulas$deltaFinal <- if (isTRUE(deltaTarget)) {
    "deltaFinal = deltaTarget * lambda + delta * (1 - lambda)"
  } else {
    "deltaFinal = delta"
  }

  return(formulas)
}




#' Fill Historical Data Gaps Using Model Projections
#'
#' Fills gaps in the historical data of a single specified variable using model projections,
#' based on an estimate from a former regression. The data set must include the necessary variables
#' to ensure accurate estimation and correction.
#'
#' @param data \code{data.frame} Input data frame containing historical data on
#' @param estimate \code{nls/lm} Fitted model object
#' @param var \code{character} Name of the target variable
#' @param periodBegin \code{numeric} first year of historical data
#' @param endOfHistory \code{numeric} Last year of historical data
#' @param convReg \code{character} Convergence type: "absolute" for value differences or "proportion" for ratios
#' @param transformVariable \code{character} Variable transformation to apply (optional)
#' @param transformVariableScen \code{character} Scenario-specific variable transformation (optional)
#'
#' @return \code{data.frame} Historical data with gaps filled using corrected projections
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate group_by ungroup left_join case_when
#'             group_modify select across all_of arrange
#' @importFrom tidyr fill
#' @importFrom stats formula predict coef
#' @importFrom data.table :=
#' @importFrom quitte getPeriods interpolate_missing_periods
#' @importFrom rlang .data !!

.fillHistoricalData <- function(data,
                                estimate,
                                var,
                                periodBegin,
                                endOfHistory,
                                convReg = "absolute",
                                transformVariable     = NULL,
                                transformVariableScen = NULL) {

  # Truncate data set
  data <- data %>%
    filter(.data[["period"]] <= endOfHistory,
           .data[["period"]] >= periodBegin)

  # Fill missing periods if necessary
  if (!endOfHistory %in% getPeriods(data)) {
    data <- data %>%
      interpolate_missing_periods(seq(periodBegin, endOfHistory), value = var)
  }


  # Extract dependent & independent variable from formula
  formula <- formula(estimate)
  dependentVar <- all.vars(formula[[2]])
  independentVar <- setdiff(all.vars(formula), c(dependentVar, names(coef(estimate))))


  # Check if all relevant variables exist within the data
  requiredVars <- unique(c("region", "period", dependentVar, independentVar))

  if (!all(requiredVars %in% names(data))) {
    stop("The dataset must contain the necessary variables: ", paste(requiredVars, collapse = ", "), ".")
  }


  # Make projection with global fit
  data$projection <- predict(estimate, newdata = data)


  # Apply variable transformations if specified
  if (!is.null(transformVariable)) {
    for (col in c(var, "projection")) {
      data <- .transVar(data, transformVariable, col)
    }
  } else if (!is.null(transformVariableScen)) {
    for (col in c(var, "projection")) {
      data <- .transVar(data, transformVariableScen, col)
    }
  }


  # Get correction formulas to smooth deviations (delta) between empirical data and projections
  deltaFormulas <- .getDeltaFormulas(lhs = var, convReg = convReg, deltaTarget = FALSE)


  # Calculate deltas and correct projections
  historicalFilled <- data %>%

    # calculate delta
    mutate_text(deltaFormulas[["deltaFormula"]]) %>%
    mutate_text(deltaFormulas[["deltaFinal"]]) %>%

    # continue delta of last historical data point
    group_by(across(all_of("region"))) %>%
    arrange(.data[["period"]]) %>%
    fill(.data[["deltaFinal"]], .direction = "down") %>%
    ungroup() %>%

    # make final projections
    mutate_text(deltaFormulas[["projectionFinalFormula"]]) %>%

    # label as history
    mutate(scenario = "history") %>%

    # keep the "delta" and "projection" column
    select(-"deltaFinal")


  return(historicalFilled)
}




#' Generate Scenario-Specific Projections
#'
#' Generates projections for multiple scenarios and regions, applying scenario-specific
#' parameter corrections. The function requires baseline projections (projectionReg)
#' that were generated using the original, uncorrected model fit.
#'
#' @param data \code{data.frame} Base projection data for all scenarios and regions.
#'        Must contain column 'projectionReg' with baseline projections from the
#'        uncorrected model fit, as well as columns 'scenario' and 'region'
#' @param fitModel \code{nls/lm} Fitted model object of class 'nls' or 'lm'
#' @param scenAssump \code{data.frame} Scenario-specific assumptions for parameter corrections
#' @param lhs \code{character} Left-hand side variable name from model
#' @param transformVariableScen \code{character} Optional scenario-specific variable transformation
#' @param applyScenFactor \code{logical} Whether to apply scenario scaling factors (default FALSE)
#' @param replacePars \code{logical} Replace parameters with scenario values vs scaling (default FALSE)
#'
#' @return \code{data.frame} Combined scenario projections with column 'projectionScen'
#'         containing the scenario-specific projections. The original baseline projections
#'         remain in column 'projectionReg'
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate group_by ungroup pull
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom quitte getRegs getScenarios
#' @importFrom stats predict
#' @importFrom lazyeval interp

.makeScenarioProjections <- function(data,
                                     fitModel,
                                     scenAssump,
                                     lhs,
                                     transformVariableScen = NULL,
                                     applyScenFactor = FALSE,
                                     replacePars = FALSE) {

  # PROCESS DATA ---------------------------------------------------------------

  # Generate projections for each scenario
  projections <- do.call("rbind", lapply(getScenarios(data), function(scen) {

    # Process each region within the scenario
    scenarioResults <- do.call("rbind", lapply(getRegs(data), function(reg) {


      # Extract scenario-specific variables from assumptions
      scenarioVars <- grep(paste0(lhs, "_X_"), colnames(scenAssump), value = TRUE)
      varNames <- gsub(paste0(lhs, "_X_"), "", scenarioVars)


      # Filter data for current scenario and region
      currentScenario <- data %>%
        filter(.data[["scenario"]] == scen, .data[["region"]] == reg)


      # copy fit model to prevent parameter mix-ups
      currentFitModel <- fitModel


      if (inherits(currentFitModel, "nls")) {
        # Modify NLS parameters based on scenario assumptions
        parameters <- currentFitModel$m$getPars()
        formula    <- currentFitModel$m$formula()[[3L]]

        for (var in varNames) {
          correctionValue <- scenAssump %>%
            filter(.data[["scenario"]] == scen, .data[["region"]] == reg) %>%
            pull(paste0(lhs, "_X_", var))

          if (replacePars) {
            parameters[var] <- correctionValue
          } else {
            parameters[var] <- parameters[var] * correctionValue
          }
        }
        currentScenario$projectionScen <- eval(formula, c(as.list(currentScenario), as.list(parameters)))
      } else {
        # Modify linear model coefficients based on scenario assumptions
        for (var in varNames) {
          correctionValue <- scenAssump %>%
            filter(.data[["scenario"]] == scen, .data[["region"]] == reg) %>%
            pull(paste0(lhs, "_X_", var))

          if (replacePars) {
            currentFitModel$coefficients[var] <- correctionValue
          } else {
            currentFitModel$coefficients[var] <- currentFitModel$coefficients[var] * correctionValue
          }
        }
        currentScenario$projectionScen <- predict(currentFitModel, newdata = currentScenario)

        # Apply scenario-specific transformations if specified
        if (!is.null(transformVariableScen)) {
          currentScenario <- .transVar(currentScenario, transformVariableScen, "projectionScen")
          currentScenario <- .transVar(currentScenario, transformVariableScen, "projectionReg")
        }

        # Apply scenario factors if specified
        if (applyScenFactor) {
          scenFactor <- scenAssump %>%
            filter(.data[["scenario"]] == scen, .data[["region"]] == reg) %>%
            pull(paste0(lhs, "_FACTOR"))
          currentScenario$projectionScen <- scenFactor * currentScenario$projectionReg
        }
      }
      return(currentScenario)
    }))
    return(scenarioResults)
  }))

  return(projections)
}
