#' Project future values from historical data using linear and non-linear regressions
#'
#' @param df \code{data.frame} Input data containing the historical data
#' @param formul \code{formula} Projection model formula
#' @param scenAssumpEcon \code{data.frame} Economic scenario assumptions for projections
#' @param lambda \code{data.frame} Convergence factors (0 to 1) over time for scenario transitions
#' @param lambdaDelta \code{data.frame} Convergence factors (0 to 1) over time for delta targets
#' @param scenAssumpCorrect \code{data.frame} Scenario-specific corrections
#' @param maxReg \code{numeric} Maximum allowed value for projections (optional)
#' @param outliers \code{character} Vector of region names to exclude from regression (optional)
#' @param apply0toNeg \code{logical} Replace negative projections with zero (default TRUE)
#' @param transformVariable \code{character} Variable transformation to apply (optional)
#' @param transformVariableScen \code{character} Scenario-specific variable transformation (optional)
#' @param applyScenFactor \code{logical} Apply scenario scaling factor (default FALSE)
#' @param convReg \code{character} Convergence type: "absolute" for value differences or "proportion" for ratios
#' @param avoidLowValues \code{logical} Prevent projections falling below initial/final values (default FALSE)
#' @param endOfHistory \code{numeric} Last year of historical data (default 2020)
#' @param scenAssumpRegion \code{data.frame} Regional convergence assumptions (optional)
#' @param interpolate \code{logical} Use all historical points vs only latest (default TRUE)
#'
#' @return \code{data.frame} Combined input data and calculated projections
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom quitte mutate_text
#' @importFrom dplyr rename_ select if_else case_when
#' @importFrom tidyselect any_of
#' @importFrom stats lm nls coef
#' @importFrom rlang parse_expr sym


makeProjections <- function(df,
                            formul,
                            scenAssumpEcon,
                            lambda,
                            lambdaDelta,
                            scenAssumpCorrect,
                            maxReg = NULL,
                            outliers = NULL,
                            apply0toNeg = TRUE,
                            transformVariable = NULL,
                            transformVariableScen = NULL,
                            applyScenFactor = FALSE,
                            convReg = "absolute",
                            avoidLowValues = FALSE,
                            endOfHistory = 2020,
                            scenAssumpRegion = NULL,
                            interpolate = TRUE) {

  # FUNCTIONS-------------------------------------------------------------------

  # extract the scenario assumption value
  pickValue <- function(df, scen, reg, enduseCol) {
    df[df$scenario == scen & df$region == reg, enduseCol][[1]]
  }

  extractList <- function(x, item) {
    res <- lapply(x, function(y) y[[item]])
    res <- do.call(rbind, res)
    return(res)
  }


  transVar <- function(dfTrans, form, var) {
    form <- gsub("VAR", var, form)
    return(dfTrans %>% mutate_(.dots = setNames(list(interp((form))), var)))
  }



  # PARAMETERS------------------------------------------------------------------

  cols <- colnames(df)
  lhs  <- all.vars(formul[[2]])
  rhs  <- all.vars(formul[[3]])

  if (!is.null(transformVariableScen)) {
    transformVariableScen <- transformVariableScen[1]
  }



  # PROCESS DATA----------------------------------------------------------------

  #--- Process input data

  # Prepare data by removing NAs and extracting variables needed for formula
  fullData <- df %>%
    removeColNa() %>%
    filter(.data[["variable"]] %in% all.vars(formul)) %>%
    select(-"unit") %>%
    spread(key = "variable", value = "value")

  # Split historical and projection data
  historicalData <- fullData %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    na.omit()

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
    nls(formul, modelData, control = list(maxiter = 500), trace = TRUE)
  } else {
    lm(formul, modelData)
  }


  #--- Projections and incorporation of scenario assumptions

  # Projections with uncorrected global fit (historical continuation)
  projectionData$projectionReg <- predict(estimate, newdata = projectionData)
  historicalData$prediction    <- predict(estimate, newdata = historicalData)


  # Apply regional maximum if specified
  if (!is.null(maxReg)) {
    projectionData$projectionReg <- pmin(maxReg, projectionData$projectionReg)
  }

  # Projections with scenario-specific assumptions
  projectionList <- lapply(getScenarios(projectionData), function(scen) {
    scenarioResults <- do.call("rbind", lapply(getRegs(projectionData), function(reg) {

      # Extract scenario-specific variables from assumptions
      scenarioVars <- grep(paste0(lhs, "_X_"), colnames(scenAssumpEcon), value = TRUE)
      varNames <- gsub(paste0(lhs, "_X_"), "", scenarioVars)

      currentScenario <- projectionData %>%
        filter(.data[["scenario"]] == scen, .data[["region"]] == reg)

      if (scen == "history") {
        # For historical scenario, use regional projections
        currentScenario$projectionScen <- currentScenario$projectionReg
      } else {
        currentEstimate <- estimate

        if (inherits(currentEstimate, "nls")) {
          # Modify NLS parameters based on scenario assumptions
          parameters <- currentEstimate$m$getPars()
          formula <- currentEstimate$m$formula()[[3L]]

          for (var in varNames) {
            parameters[var] <- parameters[var] *
              pickValue(scenAssumpEcon, scen, reg, paste0(lhs, "_X_", var))
          }

          currentScenario$projectionScen <- eval(formula, c(as.list(currentScenario), as.list(parameters)))
        } else {
          # Modify linear model coefficients based on scenario assumptions
          for (var in varNames) {
            currentEstimate$coefficients[var] <- currentEstimate$coefficients[var] *
              pickValue(scenAssumpEcon, scen, reg, paste0(lhs, "_X_", var))
          }

          currentScenario$projectionScen <- predict(currentEstimate, newdata = currentScenario)

          # Apply scenario-specific transformations if specified
          if (!is.null(transformVariableScen)) {
            currentScenario <- transVar(currentScenario, transformVariableScen, "projectionScen")
            currentScenario <- transVar(currentScenario, transformVariableScen, "projectionReg")
          }

          # Apply scenario factors if specified
          if (applyScenFactor) {
            scenarioFactor <- pickValue(scenAssumpEcon, scen, reg, paste0(lhs, "_FACTOR"))
            currentScenario$projectionScen <- scenarioFactor * currentScenario$projectionReg
          }
        }
      }

      # Merge historical continuation with scenario projections
      currentScenario <- currentScenario %>%
        left_join(lambda, by = c("region", "period", "scenario")) %>%
        mutate(projection = .data[["projectionScen"]] * .data[["fullconv"]] +
                 .data[["projectionReg"]] * (1 - .data[["fullconv"]])) %>%
        select(-c("lambda", "fullconv"))
      return(currentScenario)
    }))
    list(scenarioResults = scenarioResults)
  })

  projectionData <- extractList(projectionList, "scenarioResults")


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


  # --- Smooth out transition between history and projections (aka minimize deltas)

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


  regionalDeltas <- historicalData %>%
    # filter last historical data point
    group_by(across(all_of("region"))) %>%
    filter(.data[["period"]] == endOfHistory) %>%
    ungroup() %>%

    # determine deviation between projections and last historical point
    mutate_text(deltaFormula()) %>%
    select("region", "delta") %>%
    rbind(data.frame(region = "GLO", delta = deltaGlobalTarget))


  projectionData <- projectionData %>%
    # join relevant data
    left_join(regionalDeltas, by = "region") %>%
    left_join(scenAssumpRegion, by = "region") %>%
    left_join(rename(regionalDeltas, deltaTarget = "delta"),
              by = c(regionTarget = "region")) %>%
    left_join(lambdaDelta, by = c("region", "period", "scenario")) %>%

    # determine progression of delta values with transition factors lambda
    mutate_text(DeltaFinal) %>%

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



  # OUTPUT----------------------------------------------------------------------

  keepColumns <- c(intersect(cols, colnames(projectionData)), "projectionFinal")

  finalProjections <- projectionData %>%
    select(one_of(keepColumns)) %>%
    rename(!!sym(lhs) := "projectionFinal") %>%
    gather("variable", "value", one_of(lhs)) %>%
    anti_join(df, by = c("scenario", "variable", "period", "region")) %>%
    as.quitte() %>%
    missingToNA()

  # Return combined original and new data
  return(rbind(df, finalProjections))

}
