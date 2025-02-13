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
#' @importFrom utils setNames
#' @importFrom lazyeval interp

makeScenarioProjections <- function(data,
                                    fitModel,
                                    scenAssump,
                                    lhs,
                                    transformVariableScen = NULL,
                                    applyScenFactor = FALSE,
                                    replacePars = FALSE) {

  # FUNCTIONS ------------------------------------------------------------------

  transVar <- function(dfTrans, form, var) {
    form <- gsub("VAR", var, form)
    return(dfTrans %>% mutate_(.dots = setNames(list(interp((form))), var)))
  }



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


      if (scen == "history") {
        # For historical scenario, use regional projections
        currentScenario$projectionScen <- currentScenario$projectionReg
      } else {
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
            currentScenario <- transVar(currentScenario, transformVariableScen, "projectionScen")
            currentScenario <- transVar(currentScenario, transformVariableScen, "projectionReg")
          }

          # Apply scenario factors if specified
          if (applyScenFactor) {
            scenFactor <- scenAssump %>%
              filter(.data[["scenario"]] == scen, .data[["region"]] == reg) %>%
              pull(paste0(lhs, "_FACTOR"))
            currentScenario$projectionScen <- scenFactor * currentScenario$projectionReg
          }
        }
      }
      return(currentScenario)
    }))
    return(scenarioResults)
  }))

  return(projections)
}
