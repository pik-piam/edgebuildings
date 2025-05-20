#' Shift AC penetration S-curve to match regional reference values
#'
#' Calibrates AC penetration projections by horizontally shifting the S-curve to
#' align with region-specific reference values. The function applies a phi2 parameter
#' shift that preserves the S-curve shape while matching empirical observations.
#' The function handles the constraint that the penetration rate can not exceed the
#' climate maximum saturation (\code{coefCDD}) and in that case assumes that the
#' reference value denotes the availability.
#'
#' The S-curve follows: availability = phi1 / (1 + exp((phi2 - gdppop) / phi3))
#'
#' @param data \code{data.frame} projection data
#' @param fitPars \code{list} S-curve parameters ( with names "Asym", "phi2", "phi3")
#' @param acOwnershipRates \code{data.frame} AC penetration reference values
#' @param dataCol \code{character} Column name in data containing projection values to calibrate
#'
#' @return Data frame with same structure as input, with calibrated values in dataCol
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter mutate select left_join group_by ungroup rename across
#' @importFrom tidyr replace_na
#'
#' @export

shiftACpenetration <- function(data,
                               fitPars,
                               acOwnershipRates,
                               dataCol) {

  # obtain columns to ensure consistent output
  cols <- colnames(data)

  # filter NA's and 0's from ownership rates
  acOwnershipRates <- acOwnershipRates %>%
    select(-"variable") %>%
    rename("refPenetration" = "value") %>%
    filter(!is.na(.data$refPenetration),
           .data$refPenetration > 0)


  # determine shift in phi2 parameter from AC penetration S-curve
  phi2Shift <- data %>%

    # scale availabilites to unit
    mutate(projAvailability = .data[[dataCol]] / fitPars[["Asym"]]) %>%

    # calculate regional target phi2 and difference to global parameter
    left_join(acOwnershipRates, by = c("region", "period")) %>%
    filter(!is.na(.data$refPenetration)) %>%
    group_by(across(all_of("region"))) %>%
    mutate(
      # if CM > reference: reference == penetration, else: availability (avoids NAs)
      logTerm = ifelse(.data$coefCDD > .data$refPenetration,
                       log((.data$coefCDD / .data$refPenetration) - 1),
                       log(1 / .data$refPenetration - 1)),

      # determine reference phi2 and regional shift
      phi2Ref = .data$gdppop + fitPars[["phi3"]] * .data$logTerm,
      phi2Shift = .data$phi2Ref - fitPars[["phi2"]]
    ) %>%
    ungroup() %>%
    select("region", "phi2Shift")


  # calculate availability curves with shifted parameter - leave data w/o reference values untouched
  shiftedProjectionData <- data %>%
    left_join(phi2Shift, by = "region") %>%
    mutate(
      # if no reference value exists, shift defaults to 0
      phi2Shift = replace_na(.data$phi2Shift, 0),

      # shift projected availability
      projAvailability    = .data[[dataCol]] / fitPars[["Asym"]],
      shiftedAvailability = 1 / (1 + exp((fitPars[["phi2"]] + .data$phi2Shift - .data$gdppop) /
                                           fitPars[["phi3"]])),

      # re-transform the shifted availability to its initial order of magnitude
      shiftedProjection = .data$shiftedAvailability * fitPars[["Asym"]]
    ) %>%
    select(-dataCol) %>%
    rename(!!dataCol := "shiftedProjection") %>%
    select(cols)

  return(shiftedProjectionData)
}
