#' Integrate Exogenous ICT Demand Projections
#'
#' This function integrates exogenous ICT (Information and Communication Technology)
#' demand projections by subtracting historical ICT demand from appliances & lighting (A&L)
#' electricity demand where it was previously included. The function first calculates
#' historical ICT shares relative to total A&L electricity demand, then continues these
#' shares with constant values where data is missing. The resulting ICT demand is then
#' subtracted from A&L electricity to create a separate ICT demand category, while future
#' projections are taken directly from the exogenous ICT data according to the specified
#' scenario. Assumes equality between final and useful energy for ICT sector.
#'
#' Regional convergence can be bypassed by appending \code{"_raw"} to the \code{techScen}
#' value in the config. When applying regional convergence, the function uses
#' a three-phase approach:
#' \itemize{
#'   \item \strong{Pre-2030:} No convergence applied (original values preserved)
#'   \item \strong{2030-2050:} Regional per-capita demands converge toward global average
#'         using formula: newRatio = offset + oldRatio^(1/exponent), with linear
#'         interpolation of parameters from (0,1) to (0.31,1.5)
#'   \item \strong{Post-2050:} USA per-capita demand fixed at 2050 converged level;
#'         other regions converge linearly toward 85\% of USA per-capita by 2150
#' }
#'
#' @param ict \code{data.frame} Exogenous ICT electricity demand data containing
#'   historical and scenario projections.
#' @param fe \code{data.frame} Historical final energy data
#' @param config \code{data.frame} Configuration dataframe specifying scenario settings.
#'   If \code{techScen} ends with \code{"_raw"}, source projections are used without convergence.
#' @param pop \code{data.frame} Population data for per-capita calculations and regional
#'   convergence. Required when postprocessing with convergence.
#' @param postprocess \code{logical} If \code{FALSE} (default), integrates historical
#'   ICT data by subtracting it from A&L electricity demand. If \code{TRUE}, applies
#'   regional convergence (unless \code{"_raw"} suffix present) and returns combined
#'   FE and UE data.
#'
#' @return When \code{postprocess = FALSE}: \code{list} with elements \code{fe} (updated
#'   final energy data) and \code{feICT} (complete ICT electricity demand).
#'   When \code{postprocess = TRUE}: \code{data.frame} combining original FE data with
#'   ICT FE and UE demand (assuming FE = UE for ICT).
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter select mutate left_join right_join group_by ungroup .data %>%
#' @importFrom quitte as.quitte interpolate_missing_periods

integrateICT <- function(ict, fe, config, pop, postprocess = FALSE) {

  # PARAMETERS -----------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>%
    unique()

  techScen <- config[scen, "techScen"] %>%
    unlist()

  # if "_raw" is appended to techScen, only source projections are used
  useSourceProjections <- grepl("_raw$", techScen)

  ictScen <- sub("_raw$", "", techScen) %>%
    (\(x) strsplit(x, "_")[[1]])() %>%
    as.list() %>%
    setNames(c("ssp", "estimate"))


  # lower temporal boundary
  periodBegin <- config[scen, "periodBegin"] %>%
    unlist()


  # upper temporal boundary of historical data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()


  ## convergence parameters ====

  # year when regional per capita convergence is complete (asymptotic convergence of regional developments)
  shorttermTargetYear <- 2050

  # year when convergence toward relative regional per capita target is complete (linear interpolation)
  longtermTargetYear <- 2150

  # region to take per capita demand from as post-shorttermTargetYear target
  convergenceTargetRegion <- "USA"

  # factor for relative target
  relPerCapTarget <- 0.85

  # temporal progression of convergence parameters (10%, 28%, 55%, 100%)
  convParams <- data.frame(
    period = c(2020, 2025, 2030, 2035, 2040, 2045, 2050),
    offset = c(0, 0, 0, 0.031, 0.0868, 0.1705, 0.31),
    exponent = c(1, 1, 1, 1.05, 1.14, 1.275, 1.5)
  )



  # PROCESS DATA ---------------------------------------------------------------

  ## Extrapolate historical ICT demand and correct appliances_light fe demand ====

  if (isFALSE(postprocess)) {

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

    # Apply regional convergence if not using source projections
    if (isFALSE(useSourceProjections)) {

      ict <- ict %>%
        # Join population data
        left_join(pop %>%
                    filter(.data$scenario == ictScen$ssp) %>%
                    select("region", "period", pop = "value"),
                  by = c("region", "period")) %>%

        # Calculate per-capita ICT demand
        mutate(ictPerCap = .data$value / .data$pop) %>%

        # Calculate global weighted average per-capita for each period (for <=2050)
        group_by(.data$period) %>%
        mutate(globalPerCap = sum(.data$value, na.rm = TRUE) / sum(.data$pop, na.rm = TRUE)) %>%
        ungroup() %>%

        # Join convergence parameters (for <=2050)
        left_join(convParams, by = "period") %>%

        # Apply pre-2050 convergence formula
        mutate(
          oldRatio = .data$ictPerCap / .data$globalPerCap,
          newRatio = .data$offset + .data$oldRatio^(1 / .data$exponent),
          ictPerCapConverged = .data$newRatio * .data$globalPerCap
        )

      # Get baseline per-capita for each region at convergence target year
      baselinePerCapita <- ict %>%
        filter(.data$period == shorttermTargetYear) %>%
        select("region", "perCapBaseline" = "ictPerCapConverged")

      # Get per-capita target from convergenceTargetRegion
      convergenceTarget <- ict %>%
        filter(.data$period == shorttermTargetYear,
               .data$region == convergenceTargetRegion) %>%
        pull("ictPerCapConverged")

      ict <- ict %>%
        left_join(baselinePerCapita, by = "region") %>%

        # Apply post-2050 convergence
        mutate(
          # For post-2050: interpolate toward target
          convergenceTarget = convergenceTarget,
          interpolFactor = pmin(1, pmax(0, (.data$period - shorttermTargetYear) /
                                          (longtermTargetYear - shorttermTargetYear))),
          targetPerCap = ifelse(.data$region == convergenceTargetRegion,
                                .data$convergenceTarget,
                                relPerCapTarget * .data$convergenceTarget),
          ictPerCapConverged = ifelse(.data$period <= shorttermTargetYear,
                                      .data$ictPerCapConverged,
                                      .data$perCapBaseline +
                                        (.data$targetPerCap - .data$perCapBaseline) *
                                          .data$interpolFactor),

          # Convert back to absolute demand
          valueConverged = .data$ictPerCapConverged * .data$pop,

          # Fill missing historical data
          value = ifelse(is.na(.data$valueConverged), .data$value, .data$valueConverged)
        ) %>%

        # Clean up intermediate columns
        select(-"pop", -"ictPerCap", -"globalPerCap", -"oldRatio", -"newRatio",
               -"offset", -"exponent", -"ictPerCapConverged", -"perCapBaseline",
               -"convergenceTarget", -"interpolFactor", -"targetPerCap", -"valueConverged")
    }

    # add total ICT demand (ict.elec|fe = ict|fe)
    ict <- rbind(ict,
                 ict %>%
                   mutate(variable = sub("\\.elec", "", .data$variable)))

    # assume equality between final and useful energy for the ICT sector
    rbind(fe,
          ict,
          ict %>%
            mutate(variable = sub("fe", "ue", .data$variable)))
  }

}
