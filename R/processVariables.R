#' Calculate product
#'
#' This function calculates the product of two variables.
#'
#' @param data data.frame to work on
#' @param newNames variable name of the product
#' @param vars2sumprod variables to calculate product of
#' @param factor scaling factor
#' @param units character unit of calculated variable
#' @param na.rm logical, remove NAs
#' @param completeMissing logical, if \code{TRUE} fill missing values with zero
#' @param only.new logical, drop all variables but the newly calculated
#' @param variable character, name of variable column
#' @param value character, name of value column
#' @returns data.frame with product variable
#'
#' @author Antoine Levesque

calcAddProd <- function(data, newNames, vars2sumprod = NULL, factor = NULL,
                        units = NA, na.rm = TRUE, completeMissing = FALSE,
                        only.new = FALSE, variable = "variable",
                        value = "value") {

  res <- calc_addSumProd(data, newNames, vars2sumprod, collapse_sign = "*",
                         factor = factor, units = units, na.rm = na.rm,
                         completeMissing = completeMissing, only.new = only.new,
                         variable = variable, value = value)

  return(res)
}



#' Calculate ratio
#'
#' This function calculates the ratio of two variables.
#'
#' @param data data.frame to work on
#' @param newNames variable name of the product
#' @param numerators variable to be the numerator
#' @param denominators variable of denominator
#' @param factor scaling factor
#' @param units character unit of calculated variable
#' @param na.rm logical, remove NAs
#' @param only.new logical, drop all variables but the newly calculated
#' @param variable character, name of variable column
#' @param value character, name of value column
#' @returns data.frame with ratio variable
#'
#' @author Antoine Levesque

calc_addRatio <- function(data, newNames, numerators, denominators,
                          factor = NULL, units = NA, na.rm = TRUE,
                          only.new = FALSE, variable = "variable",
                          value = "value") {
  #--- Internal function
  addTicks <- function(x) paste0("`", x, "`")
  #---

  newNames <- addTicks(newNames)
  numerators <- addTicks(numerators)
  denominators <- addTicks(denominators)

  #--- Create the ratio formulas and add factors if necessary
  formulas <- paste(numerators, "/", denominators)
  if (!all(is.null(factor))) formulas <- paste(formulas, "*", factor)

  #--- Transform to a list
  .dots    <- as.list(formulas)
  names(.dots) <- newNames

  #--- Add the unit information
  if (!all(is.na(units))) {
    if (length(units) == length(.dots)) {
      for (i in 1:length(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units[i])
    } else if (1 == length(units)) {
      for (i in 1:length(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units)
    } else
      stop("units must be of the same length as ... or of length one.")
  }


  # call calc_addVariable_

  res <- calc_addVariable_(data, .dots,  only.new = only.new,
                           variable = variable, value = value)

  return(res)
}





#' Calculate sum of variables
#'
#' This function calculates the sum of variables.
#'
#' @param data data.frame to work on
#' @param newNames variable name of the sum
#' @param vars2sumprod variables to calculate sum of
#' @param factor scaling factor
#' @param units character unit of calculated variable
#' @param na.rm logical, remove NAs
#' @param completeMissing logical, if \code{TRUE} fill missing values with zero
#' @param only.new logical, drop all variables but the newly calculated
#' @param variable character, name of variable column
#' @param value character, name of value column
#' @returns data.frame with sum variable
#'
#' @author Antoine Levesque

calc_addSum <- function(data, newNames, vars2sumprod = NULL, factor = NULL,
                        units = NA, na.rm = FALSE, completeMissing = TRUE,
                        only.new = FALSE, variable = "variable", value = "value") {

  res <- calc_addSumProd(data, newNames, vars2sumprod, collapse_sign = "+",
                         factor = factor, units = units, na.rm = na.rm,
                         completeMissing = completeMissing, only.new = only.new,
                         variable = variable, value = value)

  return(res)
}





#' Calculate sum product
#'
#' This function calculates the sum product of two variables.
#'
#' @param data data.frame to work on
#' @param newNames variable name of the sum product
#' @param vars2sumprod variables to calculate sum product of
#' @param collapse_sign operation between vars2sumprod, has to be "*" for sum
#'   product
#' @param factor scaling factor
#' @param units character unit of calculated variable
#' @param na.rm logical, remove NAs
#' @param completeMissing logical, if \code{TRUE} fill missing values with zero
#' @param only.new logical, drop all variables but the newly calculated
#' @param variable character, name of variable column
#' @param value character, name of value column
#' @returns data.frame with product variable
#'
#' @author Antoine Levesque
#'
#' @importFrom quitte calc_addVariable_
calc_addSumProd <- function(data, newNames, vars2sumprod, collapse_sign,
                            factor = NULL, units = NA, na.rm = FALSE,
                            completeMissing = TRUE, only.new = FALSE,
                            variable = "variable", value = "value") {
  # Internal function
  addTicks <- function(x) paste0("`", x, "`")

  # in case a named list is provided instead of newNames + vars2sumprod
  if (is.list(newNames) & !is.null(names(newNames)) & is.null(vars2sumprod)) {
    vars2sumprod <- unname(newNames)
    newNames <- names(newNames)
  }

  if (!is.list(vars2sumprod)) {
    vars2sumprod <- list(vars2sumprod)
  }

  newNames <- addTicks(newNames)
  vars2sumprod <- lapply(vars2sumprod, addTicks)

  # Create the sum formulas and add factors if necessary
  .dots <- lapply(vars2sumprod, paste, collapse = collapse_sign)
  if (!all(is.null(factor))) {
    .dots <- lapply(.dots, paste, "*", factor)
  }

  # Give new names to the list
  names(.dots) <- newNames

  # Add the factors, if necessary
  if (!all(is.na(factor))) {
    if (length(factor) == length(.dots)) {
      for (i in 1:length(.dots)) {
        .dots[i] <- paste(.dots[i], "*", factor[i])
      }
    } else if (1 == length(factor)) {
      .dots <- lapply(.dots, paste, "*", factor)
    } else {
      stop("factor must be of the same length as newNames or of length one.")
    }
  }

  # Add the unit information, if necessary
  if (!all(is.na(units))) {
    if (length(units) == length(.dots)) {
      for (i in 1:length(.dots)) {
        .dots[i][[1]] <- c(.dots[i][[1]], units[i])
      }
    } else if (1 == length(units)) {
      for (i in 1:length(.dots)) {
        .dots[i][[1]] <- c(.dots[i][[1]], units)
      }
    } else {
      stop("units must be of the same length as ... or of length one.")
    }
  }

  # call calc_addVariable_
  res <- calc_addVariable_(data, .dots,  only.new = only.new, na.rm = na.rm,
                           completeMissing = completeMissing,
                           variable = variable, value = value)

  return(res)
}





#' Spread some variables
#'
#' @param df data.frame
#' @param vars2spread variables to be spread into
#' @param colVar column of variables
#' @param valueCol column of values
#'
#' @importFrom quitte getColValues
#' @importFrom tidyr spread_ gather

spreadSomeVariables <- function(df, vars2spread, colVar = "variable", valueCol = "value") {
  all_vars <- getColValues(df, colVar = colVar)
  keepVars <- setdiff(all_vars, vars2spread)

  df <- df %>% spread_(colVar, valueCol) %>%
    gather(colVar, valueCol, keepVars)

  return(df)
}





#' Calculate Difference
#'
#' This function calculates the difference of variables.
#'
#' @param data data.frame to work on
#' @param newNames variable name of the difference
#' @param vars2sumprod variables to calculate difference of
#' @param factor scaling factor
#' @param units character unit of calculated variable
#' @param na.rm logical, remove NAs
#' @param completeMissing logical, if \code{TRUE} fill missing values with zero
#' @param only.new logical, drop all variables but the newly calculated
#' @param variable character, name of variable column
#' @param value character, name of value column
#' @returns data.frame with difference variable
#'
#' @author Antoine Levesque

calc_addSubs <- function(data, newNames, vars2sumprod = NULL, factor = NULL,
                         units = NA, na.rm = FALSE, completeMissing = TRUE,
                         only.new = FALSE, variable = "variable",
                         value = "value") {

  res <- calc_addSumProd(data, newNames, vars2sumprod, collapse_sign = "-",
                         factor = factor, units = units, na.rm = na.rm,
                         completeMissing = completeMissing, only.new = only.new,
                         variable = variable, value = value)

  return(res)
}
