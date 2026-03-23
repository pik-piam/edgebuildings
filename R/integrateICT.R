#' Integrate Exogenous ICT Demand Projections
#'
#' This function integrates exogenous ICT (Information and Communication Technology)
#' demand projections by subtracting historical ICT demand from appliances & lighting (A&L)
#' electricity demand where it was previously included. The function first calculates
#' historical ICT shares relative to total A&L electricity demand, then continues these
#' shares with constant values where data is missing. The resulting ICT demand is then
#' subtracted from A&L electricity to create a separate ICT demand category, while future
#' projections are taken directly from the exogenous ICT data according to the specified
#' scenario.
#'
#' @param ict \code{data.frame} Exogenous ICT electricity demand data containing
#'   historical and scenario projections.
#' @param fe \code{data.frame} Historical final energy data
#' @param config \code{data.frame} Configuration dataframe specifying scenario settings
#' @param expandProjections \code{logical} If \code{FALSE} (default), integrates historical
#'   ICT data by subtracting it from A&L electricity demand. If \code{TRUE}, extrapolates
#'   ICT demand projections with constant values for missing future periods.
#'
#' @return \code{list} A list with two elements:
#'   \item{fe}{Updated final energy data}
#'   \item{feICT}{Complete ICT electricity demand data}
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter select mutate left_join right_join .data %>%
#' @importFrom quitte as.quitte interpolate_missing_periods

integrateICT <- function(ict, fe, config, expandProjections = FALSE) {

  # PARAMETERS -----------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>%
    unique()

  # technological scenario
  ictScen <- config[scen, "techScen"] %>%
    unlist() %>%
    (\(x) strsplit(x, "_")[[1]])() %>%
    as.list() %>%
    setNames(c("ssp", "estimate"))

  # lower temporal boundary
  periodBegin <- config[scen, "periodBegin"] %>%
    unlist()

  # upper temporal boundary of historical data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()



  # PROCESS DATA ---------------------------------------------------------------

  ## Extrapolate historical ICT demand and correct appliances_light fe demand ====

  if (isFALSE(expandProjections)) {

    # Extract AL electricity demand
    appliancesElec <- fe %>%
      filter(.data$enduse == "appliances_light", .data$carrier == "elec")

    # Filter ICT data and calculate historical shares
    ictDemand <- ict %>%
      filter(.data$scenario %in% c("history", ictScen$ssp),
             .data$variable == ictScen$estimate)

    # Calculate historical ICT/appliances ratio
    ictRatio <- ictDemand %>%
      filter(.data$scenario == "history") %>%
      select("region", "period", "value") %>%
      right_join(appliancesElec, by = c("region", "period"), suffix = c("ICT", "App")) %>%
      mutate(ratio = .data$valueICT / .data$valueApp) %>%
      select("region", "period", "ratio") %>%

      # extrapolate missing entries w/ constant historical ratio
      interpolate_missing_periods(value = "ratio", expand.values = TRUE)

    # Check for ratios > 1
    if (any(ictRatio$ratio > 1, na.rm = TRUE)) {
      affectedRegions <- ictRatio %>%
        filter(.data$ratio > 1) %>%
        pull(.data$region) %>%
        unique()
      warning("Historic ICT demands exceed A&L demand in regions: ", paste(affectedRegions, collapse = ", "))
    }

    # Fill historical ICT demands and combine with future scenarios
    ictComplete <- ictDemand %>%
      filter(.data$scenario == "history") %>%
      interpolate_missing_periods(periodBegin:endOfHistory) %>%
      left_join(ictRatio, by = c("region", "period")) %>%
      left_join(appliancesElec %>%
                  select("region", "period", "value"),
                by = c("region", "period"),
                suffix = c("ICT", "Total")) %>%
      mutate(value = ifelse(!is.na(.data$valueICT), .data$valueICT, .data$valueTotal * .data$ratio),
             .keep = "unused") %>%
      rbind(ictDemand %>%
              filter(.data$scenario != "history") %>%
              mutate(scenario = scen)) %>%
      mutate(variable = "ict.elec|fe") %>%
      select(-"enduse", -"carrier") %>%
      as.quitte() %>%
      missingToNA()

    # Subtract ICT from AL electricity demand
    appliancesElec <- appliancesElec %>%
      left_join(ictComplete[, c("region", "period", "value")],
                by = c("region", "period"),
                suffix = c("", "ICT")) %>%
      mutate(value = .data$value - .data$valueICT, .keep = "unused")

    # Rebuild final energy data without ICT in appliances
    fe <- fe %>%
      filter(!(.data$enduse == "appliances_light" & .data$carrier == "elec")) %>%
      rbind(appliancesElec)


    return(list(fe = fe,
                feICT = ictComplete))

  } else {

    ## Extrapolate future ICT demand constantly ====

    # original projections run only until 2050

    extrapolatedDemand <- ict %>%
      interpolate_missing_periods(unique(fe$period), expand.values = TRUE)

    # assume equality between final and useful energy for the ICT sector
    fe %>%
      rbind(extrapolatedDemand,
            extrapolatedDemand %>%
              mutate(variable = sub("fe", "ue", .data$variable)))
  }

}
