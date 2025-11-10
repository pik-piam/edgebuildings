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
#'
#' @return \code{list} A list with two elements:
#'   \item{fe}{Updated final energy data}
#'   \item{feICT}{Complete ICT electricity demand data}
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter select mutate left_join right_join .data %>%
#' @importFrom quitte as.quitte interpolate_missing_periods

integrateICT <- function(ict, fe, config) {

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

  # Extract AL electricity demand
  appliancesElec <- fe %>%
    filter(.data$enduse == "appliances_light", .data$carrier == "elec")

  # Filter ICT data and calculate historical shares
  ictDemand <- ict %>%
    filter(.data$scenario %in% c("history", ictScen$ssp),
           .data$variable == ictScen$estimate)

  # Calculate historical ICT/appliances share
  ictShares <- ictDemand %>%
    filter(.data$scenario == "history") %>%
    select(.data$region, .data$period, .data$value) %>%
    right_join(appliancesElec, by = c("region", "period"), suffix = c("ICT", "App")) %>%
    mutate(share = .data$valueICT / .data$valueApp) %>%
    select("region", "period", "share") %>%

    # extrapolate missing entries w/ constant historical share
    interpolate_missing_periods(value = "share", expand.values = TRUE)


  # Fill historical ICT demands and combine with future scenarios
  ictComplete <- ictDemand %>%
    filter(.data$scenario == "history") %>%
    interpolate_missing_periods(periodBegin:endOfHistory) %>%
    left_join(ictShares, by = c("region", "period")) %>%
    left_join(appliancesElec %>%
                select("region", "period", "value"),
              by = c("region", "period"),
              suffix = c("ICT", "Total")) %>%
    mutate(value = ifelse(!is.na(.data$valueICT), .data$valueICT, .data$valueTotal * .data$share),
           .keep = "unused") %>%
    rbind(ictDemand %>%
            filter(.data$scenario != "history") %>%
            mutate(scenario = scen)) %>%
    mutate(variable = "ict.elec|fe") %>%
    select(-"enduse", -"carrier") %>%
    as.quitte()

  # Subtract ICT from AL electricity demand
  appliancesElec <- appliancesElec %>%
    left_join(ictComplete %>% select(.data$region, .data$period, .data$value),
              by = c("region", "period"),
              suffix = c("", "ICT")) %>%
    mutate(value = .data$value - .data$valueICT, .keep = "unused")

  # Rebuild final energy data without ICT in appliances
  fe <- fe %>%
    filter(!(.data$enduse == "appliances_light" & .data$carrier == "elec")) %>%
    rbind(appliancesElec)


  # OUTPUT ---------------------------------------------------------------------

  return(list(fe = fe,
              feICT = ictComplete))
}
