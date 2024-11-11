#' Extrapolate missing values beyond existing periods
#'
#' @param chunk grouped data.frame
#' @param key column holding values for extrapolation
#' @param slopeOfLast number of values for boundary regression
#' @returns data.frame with extrapolated column \code{key}
#'
#' @author Robin Hasse
#'
#' @importFrom utils tail head

extrapolateMissingPeriods <- function(chunk, key, slopeOfLast = 5) {
  # remove NAs
  outChunk <- chunk
  chunk <- chunk[!is.na(chunk[[key]]), ]

  # not enough values available for regression
  if (nrow(chunk) < 2) {
    # constant value
    if (nrow(chunk) == 1) {
      outChunk[[key]] = unique(chunk[[key]])
    } else {
      # no value
      outChunk[[key]] <- NA
    }
  } else {
    upperPeriod <- max(chunk$period)
    lowerPeriod <- min(chunk$period)

    form <- as.formula(paste(key, "~ period"))

    # linear regression at upper and lower end
    mUpper <- lm(form, tail(arrange(chunk, "period"), slopeOfLast))
    mLower <- lm(form, head(arrange(chunk, "period"), slopeOfLast))

    # extrapolate both ends
    outChunk[["valueUpper"]] <- predict(mUpper, newdata = outChunk["period"])
    outChunk[["valueLower"]] <- predict(mLower, newdata = outChunk["period"])

    # shift extrapolation to match last data points
    outChunk[["valueUpper"]] <- outChunk[["valueUpper"]] *
      as.numeric(outChunk[outChunk$period == upperPeriod, key] /
                   outChunk[outChunk$period == upperPeriod, "valueUpper"])
    outChunk[["valueLower"]] <- outChunk[["valueLower"]] *
      as.numeric(outChunk[outChunk$period == lowerPeriod, key] /
                   outChunk[outChunk$period == lowerPeriod, "valueLower"])

    # fill missing lower/upper ends
    outChunk[[key]] <- ifelse(outChunk[["period"]] > max(chunk$period),
                              outChunk[["valueUpper"]],
                              outChunk[[key]])
    outChunk[[key]] <- ifelse(outChunk[["period"]] < min(chunk$period),
                              outChunk[["valueLower"]],
                              outChunk[[key]])
    outChunk[["valueUpper"]] <- NULL
    outChunk[["valueLower"]] <- NULL
  }

  return(outChunk)
}
