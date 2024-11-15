#' Project future from historic data with linear and non-linear regressions
#'
#' @author Antoine Levesque
#'
#' @param df data.frame with required variables
#' @param formul character, projection formula
#' @param scenAssumpEcon data.frame with projection parameters
#' @param lambda data.frame with values that go from zero to 1 over time
#'   describing the transition towards the projection scenario
#' @param lambdaDelta data.frame with values that go from zero to 1 over time
#'   describing the transition towards the projection scenario for delta targets
#' @param scenAssumpCorrect data.frame with scenario corrections
#' @param maxReg maximum value
#' @param outliers character vector with regions not to be considered in
#'   regression
#' @param apply0toNeg logical, if \code{TRUE}, negative values are replaced with
#'   zero
#' @param transformVariable variable transformation
#' @param transformVariableScen scenario variable transformation
#' @param applyScenFactor logical, if \code{TRUE} a given scaling factor is
#'   applied
#' @param convReg character, describing what deltas are considered:
#'   \itemize{
#'     \item \code{"absolute"}: absolute deviations from prediction
#'     \item \code{"relative"}: relative deviations from prediction
#'   }
#' @param avoidLowValues logical, if \code{TRUE} projection values are clipped
#'   to avoid that they fall below the start value (for increasing projections)
#'   or final value (for decreasing projections)
#' @param endOfHistory upper temporal boundary of historical data
#' @param scenAssumpRegion regional convergence assumptions
#' @param interpolate logical
#' @returns data.frame with added projection
#'
#' @importFrom quitte mutate_text
#' @importFrom dplyr rename_ select
#' @importFrom stats lm nls coef


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
    df[df$scenario == scen & df$region == reg, enduseCol]
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

  # withdraw the unit information and reduce to the variables in the formula
  tmp <- df %>%
    removeColNa() %>%
    filter(.data[["variable"]] %in% all.vars(formul)) %>%
    select(-"unit") %>%
    spread(key = "variable", value = "value")

  # historical data
  tmpHist <- tmp %>%
    filter(.data[["period"]] <= endOfHistory) %>%
    na.omit()

  # tmpProj is the set (scenario,region, period) if there the regression is only made on the intercept
  tmpProj <- tmp %>%
    removeColNa()

  if (length(rhs) == 0) {
    tmpProj <- df %>%
      select("scenario", "region", "period") %>%
      unique()
  }

  # match periods of temporal convergence shares
  tmpProj <- tmpProj %>%
    filter(.data[["period"]] %in% getPeriods(lambda))

  # Create the estimation object, depending upon nls or lm
  dat <- tmpHist %>% filter(!(.data[["region"]] %in% outliers))

  # Note: Only one observation per region because of the low variability offered by a second point.
  #       So the second point only overweights the corresponding region
  if (!interpolate) {
    dat <- dat %>%
      group_by(across(all_of("region"))) %>%
      filter(rank(.data[["period"]]) == max(rank(.data[["period"]]))) %>%
      ungroup()
  }

  # non-linear regression
  if (any(grepl("\\SS", formul))) {
    # replace 0s with small values to avoid Inf issues
    dat[dat[lhs] == 0, lhs] <- min(dat[dat[lhs] != 0, lhs]) / 10
    estimate <- nls(formul, dat, control = list(maxiter = 500), trace = TRUE)
  }

  # linear regression
  else { # nolint
    estimate <- lm(formul, dat)
  }

  # predict from estimation object
  dat$pred <- predict(estimate)
  # make prediction from historical data
  tmpHist$pred <- predict(estimate, newdata = tmpHist)


  # compute the estimation with unchanged parameters for the future
  tmpProj$projReg <- predict(estimate, newdata = tmpProj)

  if (!is.null(maxReg)) {
    tmpProj$projReg <- pmin(maxReg, tmpProj$projReg)
  }


  # Parameters are changed according to the scenario assumption and the prediction
  # is re-done with the new parameters. The  data from the first prediction is
  # then faded into the data from the prediction with new parameters.
  #
  # Regression parameters are corrected scenario- and region-wise.

  listProj <- lapply(getScenarios(tmpProj), function(scen) {
    tmpScen <- do.call("rbind", lapply(getRegs(tmpProj), function(reg) {
      # params stores the information on the parameters
      params <- data.frame(formula = as.character(formul),
                           scenario = scen)

      # only take the scenario of interest from tmpProj
      tmpScen <- tmpProj %>%
        filter(.data[["scenario"]] == scen,
               .data[["region"]] == reg)

      # modify the estimate object with the scenario assumption
      estimateScen <- estimate

      # loop over the variables in the rhs of the equation, except for intercept
      colScen <- grep(paste0(lhs, "_X_"), colnames(scenAssumpEcon), value = TRUE)
      varScen <- gsub(paste0(lhs, "_X_"), "", colScen)

      if (scen == "history") {
        tmpScen <- tmpScen %>%
          mutate(projScen = .data[["projReg"]])

        # storing the regression parameter information
        regCoefs <- as.data.frame(t(coef(estimateScen)))
      } else {
        # if nls was used, include the parameters to the current environment
        if (estimateScen$call[1] == "nls()") {
          # extract the parameter values and the formula from the estimate object
          pars <- estimateScen$m$getPars()

          # If correction to be included, change the values of parameters
          if (lhs %in% colnames(scenAssumpCorrect)) {
            corrections <- na.omit(scenAssumpCorrect[c("parameter", lhs)])
            pars[corrections$parameter] <- corrections[[lhs]]
          }

          form <- estimateScen$m$formula()[[3L]]

          # include the parameters into the environment as objects
          for (i in seq_along(pars)) {
            assign(names(pars[i]), pars[i])
          }

          # modify the estimate object according to the scenario assumptions
          for (var in varScen) {
            assign(var, get(var) * pickValue(scenAssumpEcon, scen, reg, paste0(lhs, "_X_", var)))
            pars[var] <- get(var)
          }

          # storing the parameter information
          regCoefs <- as.data.frame(t(pars))

          # use the formula to compute the projScen column. The environment should
          # now contain the parameters from the formula
          tmpScen$projScen <- eval(form, as.list(tmpScen))

          rm(list = names(pars))

        } else if (estimateScen$call[1] == "lm()") {
          # modify the estimate object according to the scenario assumptions
          for (var in varScen) {
            estimateScen[["coefficients"]][var] <- estimateScen[["coefficients"]][var] *
              pickValue(scenAssumpEcon, scen, reg, paste0(lhs, "_X_", var))
          }

          # if (lhs == "appliances_light_elas") {
          #   estimateScen[["coefficients"]]["I(gdppop^(-1/2))"] <- -266.717914148041
          #   estimateScen[["coefficients"]]["(Intercept)"] <- -6.87122756665672
          # }

          # storing the parameter information
          regCoefs <- as.data.frame(t(coef(estimateScen)))

          # use the predict function to compute the projScen column
          tmpScen$projScen <- predict(estimateScen, newdata = tmpScen)

          # In case the scenario assumption applies on the transformed variable
          if (!is.null(transformVariableScen)) {
            tmpScen <- transVar(tmpScen, transformVariableScen, "projScen")
            tmpScen <- transVar(tmpScen, transformVariableScen, "projReg")
          }

          # In case the scenario assumption is a factor of the prediction
          if (applyScenFactor) {
            scenFactor <- pickValue(scenAssumpEcon, scen, reg, paste0(lhs, "_FACTOR"))
            tmpScen$projScen <- scenFactor * tmpScen$projReg
          }
        }
      }

      params <- cbind(params, regCoefs)
      params <- params %>%
        gather(key = "parameter", value = "value", colnames(regCoefs))

      # proj column: linear combination of regression result and  scenario modified parameters
      tmpScen <- tmpScen %>%
        left_join(lambda, by = c("region", "period", "scenario")) %>%
        mutate(proj = .data[["projScen"]] * .data[["fullconv"]] +
                 .data[["projReg"]] * (1 - .data[["fullconv"]])) %>%
        select(-"lambda", -"fullconv")

      return(tmpScen)
    }))
    return(list(tmpScen = tmpScen))
  })

  tmpProj <- extractList(listProj, "tmpScen")

  # transform variable if necesarry
  if (!is.null(transformVariable)) {
    tmpHist <- transVar(tmpHist, transformVariable, lhs)
    tmpHist <- transVar(tmpHist, transformVariable, "pred")
    tmpProj <- transVar(tmpProj, transformVariable, "projReg")
    tmpProj <- transVar(tmpProj, transformVariable, "projScen")
    tmpProj <- transVar(tmpProj, transformVariable, "proj")
  } else if (!is.null(transformVariableScen)) {
    tmpHist <- transVar(tmpHist, transformVariableScen, "pred")
    tmpHist <- transVar(tmpHist, transformVariableScen, lhs)
  }


  # == deltas

  # compute delta values (= deviation from projections)
  if (convReg == "absolute") {
    deltaGlobalTarget  <- 0
    computeDeltaForm   <- paste0("delta = - pred + ", lhs)
    computeprojFinalForm <- "projFinal = proj + deltaFinal"
  } else if (convReg == "proportion") {
    deltaGlobalTarget  <- 1
    computeDeltaForm   <- paste0("delta =  (1/pred) *  ", lhs)
    computeprojFinalForm <- "projFinal = proj * deltaFinal"
  } else {
    stop("convReg must be in c('absolute','proportion')")
  }

  deltaDF <- tmpHist %>%
    mutate_text(computeDeltaForm) %>%
    group_by(across(all_of("region"))) %>%
    filter(.data[["period"]] == endOfHistory) %>%
    reframe(delta = mean(.data[["delta"]], na.rm = TRUE)) %>%
    rbind(data.frame(region = "GLO", delta = deltaGlobalTarget))

  # include regional convergence assumptions for outliers and not outliers and bind them
  tmpProj <- tmpProj %>%
    left_join(deltaDF, by = c("region")) %>%
    left_join(scenAssumpRegion, by = c("region")) %>%
    left_join(rename(deltaDF,
                     "regionTarget" = "region",
                     "deltaTarget"  = "delta"),
              by = c("regionTarget"))

  # temporally converge deltas toward delta target
  tmpProj <- tmpProj %>%
    left_join(lambdaDelta, by = c("region", "period", "scenario")) %>%
    mutate(deltaFinal = .data[["deltaTarget"]] * .data[["lambda"]] +
             .data[["delta"]] * (1 - .data[["lambda"]])) %>%
    select(-"fullconv", -"lambda") %>%
    mutate_text(computeprojFinalForm)


  if (lhs == "space_cooling_m2_CDD_Uval") {
    # calculate final values for non-outliers
    tmpProjNonOut <- tmpProj %>%
      filter(!(.data[["region"]] %in% outliers))

    # use growth of predictions for outlier values
    tmpProjOut <- tmpProj %>%
      filter(.data[["region"]] %in% outliers) %>%
      group_by(across(all_of(c("region")))) %>%
      tidyr::fill(.data[[lhs]], .direction = "down") %>%
      mutate(projScen1 = lag(.data[["projScen"]])) %>%
      ungroup() %>%
      mutate(growth = ifelse(.data[["period"]] <= 2020,
                             1,
                             c(0, .data[["projScen"]] / .data[["projScen1"]]))) %>%
      group_by(across(all_of(c("region")))) %>%
      mutate(growth = cumprod(.data[["growth"]])) %>%
      ungroup() %>%
      mutate(projFinal = .data[[lhs]] * .data[["growth"]]) %>%
      select(-"growth", -"projScen1")

    tmpProj <- rbind(tmpProjOut, tmpProjNonOut)
  }


  # replace negative values with 0
  exclude <- tmpProj %>%
    filter(.data[["projFinal"]] < 0) %>%
    select("period", "region", "scenario") %>%
    unique()

  if (nrow(exclude) > 0 && apply0toNeg) {
    cat("some values are negative and replaced with 0 \n")
    print(as.data.frame(exclude))
    tmpProj <- tmpProj %>%
      mutate(projFinal = pmax(0, .data[["projFinal"]]))
  }

  if (avoidLowValues) {
    tmpProj <- tmpProj %>%
      group_by(across(all_of(c("scenario", "region")))) %>%
      mutate(logictest = switch(unique(.data[["scenario"]]),
                                history = FALSE,
                                any(.data[["projFinal"]][.data[["period"]] == endOfHistory + 5] >=
                                      .data[["projScen"]][.data[["period"]] == 2100])),
             projFinal = ifelse(.data[["logictest"]],
                                pmax(.data[["projFinal"]],
                                     .data[["projScen"]][.data[["period"]] == 2100]),
                                .data[["projFinal"]])) %>%
      ungroup() %>%
      group_by(across(all_of(c("scenario", "region")))) %>%
      mutate(logictest = switch(unique(.data[["scenario"]]),
                                history = FALSE,
                                any(.data[["projFinal"]][.data[["period"]] == endOfHistory + 5] <=
                                      .data[["projScen"]][.data[["period"]] == 2100])),
             projFinal = ifelse(.data[["logictest"]],
                                pmax(.data[["projFinal"]],
                                     .data[["projFinal"]][.data[["period"]] == endOfHistory + 5]),
                                .data[["projFinal"]])) %>%
      ungroup() %>%
      select(-"logictest")
  }


  # OUTPUT----------------------------------------------------------------------

  # == Clean the DF and return it
  keepCols <- c(intersect(cols, colnames(tmpProj)), "projFinal")

  tmpProj <- tmpProj  %>%
    select(one_of(keepCols)) %>%
    rename_(.dots = setNames("projFinal", lhs)) %>%
    gather("variable", "value", one_of(lhs)) %>%
    anti_join(df, by = c("scenario", "variable", "period", "region")) %>%
    as.quitte() %>%
    missingToNA()

  return(rbind(df, tmpProj))

}
