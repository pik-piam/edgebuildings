#' Project Space Cooling Energy Demand
#'
#' This function projects future space cooling useful energy demand and models air
#' conditioner (AC) penetration rates as a function of GDP per capita and cooling degree
#' days (CDD) using a logistic function with climate-adjusted parameters.
#'
#' Space cooling energy demand is calculated using:
#' \deqn{space\_cooling =  \phi_1 \cdot floor space \cdot uvalue \cdot CDD \cdot penetration rate}
#'
#' The AC penetration rate component follows the logistic formula:
#' \deqn{penetration = 1 / (1 + \exp(\alpha - \beta \cdot gdppop^\gamma \cdot CDD^\delta))}
#'
#' The model incorporates two key assumptions:
#' \enumerate{
#'   \item All regional penetration curves originate from the same point at
#'         gdppop = 0 and CDD = 0
#'   \item The curve elongation parameter (\eqn{\phi_3}) decreases monotonically
#'         with rising CDDs to account for dampened/accelerated technology adoption
#'         due to regional climate
#' }
#'
#' For regions with historical data, regional beta coefficients are derived to
#' match the last historical reference point. The regional scaling parameter (\eqn{\phi_1}),
#' or cooling activity, is derived from a linear fit on historical data and converges
#' toward a global activity factor over time, controlled by GDP per capita thresholds
#' and time-based convergence factors.
#'
#' The function assumes that the input \code{data} contains a single non-"history"
#' scenario.
#'
#'
#' @param data data frame with historical and scenario data
#' @param config data frame specifying scenario settings
#' @param acOwnershipRates data frame with historical AC penetration rates
#' @param regPars data frame with regression parameters for AC penetration estimation
#' @param endOfHistory last period of historical data
#' @param lambda data frame with convergence factors (0 to 1) over time for scenario transitions
#' @param outliers list or vector of outlier regions where global beta parameter shall be applied
#'
#' @return A data frame with the same structure as the input \code{data}, but with
#'   projected space cooling energy demand extending the
#'   historical "space_cooling" variable data.
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter select rename left_join group_by
#'   slice_max ungroup mutate right_join anti_join across all_of .data %>%
#' @importFrom tidyr pivot_wider pivot_longer

projectSpaceCooling <- function(data,
                                config,
                                acOwnershipRates,
                                regPars,
                                endOfHistory,
                                lambda,
                                outliers = NULL) {

  # PARAMETERS -----------------------------------------------------------------

  # Extract current scenario (non-historical)
  scen <- unique(data$scenario[data$scenario != "history"]) %>%
    as.character()

  # Conversion factor: days to seconds
  day2sec <- 24 * 3600 # h/d * s/h

  # GDP per capita threshold for activity convergence [USD/cap]
  # TODO: make this a config parameter
  gdpThreshold <- 7e+04


  # PROCESS DATA ---------------------------------------------------------------

  # Store column structure for output formatting
  cols <- colnames(data)

  # Convert regression parameters to named vector for easy access
  regPars <- setNames(regPars$value, regPars$variable)


  ## Regional beta corrections ====
  # Calculate region-specific beta coefficients to match historical AC penetration
  # rates at the last observed data point

  # Prepare model data with historical AC penetration, GDP per capita, and CDD
  modelData <- acOwnershipRates %>%
    filter(!is.na(.data$value),
           .data$value != 0) %>%
    select("region", "period", "value") %>%
    rename("penetration" = "value") %>%
    # Join GDP per capita and CDD data
    left_join(data %>%
                filter(.data$variable %in% c("CDD", "gdppop")) %>%
                select("region", "period", "variable", "value") %>%
                pivot_wider(names_from = "variable", values_from = "value"),
              by = c("region", "period"))

  # Extract global fit parameters from logistic regression
  alpha <- regPars[["alpha"]]  # Intercept parameter
  beta  <- regPars[["beta"]]   # Global scaling coefficient
  gamma <- regPars[["gamma"]]  # CDD exponent
  delta <- regPars[["delta"]]  # GDP per capita exponent

  # Calculate region-specific beta coefficients
  betaReg <- modelData %>%
    # For each region, use the most recent historical data point
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Derive regional beta by inverting the logistic function
    mutate(betaReg = (alpha - log(1 / .data$penetration - 1)) / (.data$gdppop^delta * .data$CDD^gamma),
           betaReg = ifelse(.data$betaReg < 0, NA, .data$betaReg)) %>%
    # Ensure all regions are represented
    right_join(data %>%
                 select("region") %>%
                 unique(),
               by = "region") %>%
    mutate(# Use global beta for regions without valid historical data
           betaReg = ifelse((is.na(.data$betaReg) | .data$betaReg < 0),
                            beta,
                            .data$betaReg),
           # Use global beta for outlier regions
           betaReg = ifelse(.data$region %in% outliers, beta, .data$betaReg)) %>%
    select("region", "betaReg")


  ## Projections ====
  # Project AC penetration rates and calculate unscaled cooling demand

  projectionData <- data %>%
    filter(.data$variable %in% c("CDD", "gdppop", "space_cooling", "buildings", "uvalue")) %>%
    select("region", "period", "variable", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    left_join(betaReg, by = "region") %>%
    mutate(
      # Project AC penetration rate using logistic function with regional beta
      acPenetration = 1 / (1 + exp(alpha - .data$betaReg * .data$gdppop^delta * .data$CDD^gamma)),

      # Calculate unscaled demand [EJ/yr] to determine regional activity factors
      # Formula: floor space [m²] * U-value [W/m²/K] * CDD [K·days] * conversion to EJ
      unscaledDemand = .data$buildings * .data$uvalue * .data$CDD * day2sec * .data$acPenetration / 1e12
    )

  # Extract historical data points for fitting activity factors
  fitData <- projectionData %>%
    filter(!is.na(.data$space_cooling),
           .data$space_cooling > 0)


  ### Activity factors ====
  # Derive regional cooling activity factors (phi_1) from historical data using
  # linear regression, then converge towards global activity over time

  # Calculate regional activity factors from historical fit
  coolingActivity <- do.call(rbind, lapply(unique(projectionData$region), function(reg) {
    # Extract regional historical data
    regionalFitData <- fitData %>%
      filter(.data$region == reg)

    # If no historical data available, use all projection data for that region
    if (nrow(regionalFitData) == 0) {
      regionalFitData <- projectionData %>%
        filter(.data$region == reg)
    }

    # Fit linear model through origin to determine regional activity factor
    regEstimate <- lm("space_cooling ~ 0 + unscaledDemand",
                      data = regionalFitData)

    # Apply regional activity factor to all periods
    projectionData %>%
      filter(.data$region == reg) %>%
      mutate(activityReg = regEstimate$coefficients[["unscaledDemand"]]) %>%
      select("region", "period", "activityReg")
  }))

  # Calculate global activity factor (excluding China due to comparitively low values)
  estimate <- lm("space_cooling ~ 0 + unscaledDemand",
                 data = fitData %>%
                   filter(.data$region != "CHN"))

  coolingActivity$activityGlo <- estimate$coefficients[["unscaledDemand"]]

  # Implement convergence of regional to global activity factors
  coolingActivity <- coolingActivity %>%
    mutate(
      activityGlo = estimate$coefficients[["unscaledDemand"]],
      # If regional activity exceeds global, maintain regional value (no convergence)
      # Otherwise, allow convergence to global activity
      activityGlo = ifelse(.data$activityReg > .data$activityGlo, .data$activityReg, .data$activityGlo)
    ) %>%
    left_join(lambda %>%
                select("region", "period", "fullconv"),
              by = c("region", "period")) %>%
    left_join(projectionData %>%
                select("region", "period", "gdppop"),
              by = c("region", "period")) %>%
    group_by(across(all_of("region"))) %>%
    mutate(
      # Calculate GDP-driven convergence factor (0 to 1)
      lambdaGDP = (.data$gdppop - .data$gdppop[.data$period == endOfHistory]) /
        (gdpThreshold - .data$gdppop[.data$period == endOfHistory]),
      lambdaGDP = ifelse(.data$lambdaGDP < 0, 0, ifelse(.data$lambdaGDP > 1, 1, .data$lambdaGDP)),

      # Combine temporal and income-driven convergence
      lambda = pmin(1, pmin(.data$fullconv, .data$lambdaGDP)),

      # Interpolate between regional and global activity based on convergence factor
      activity = .data$activityReg * (1 - .data$lambda) + .data$activityGlo * .data$lambda
    ) %>%
    ungroup() %>%
    select("region", "period", "activity")


  ### Project UE cooling demand ====
  # Calculate final cooling useful energy demand by applying activity factors

  projections <- projectionData %>%
    left_join(coolingActivity, by = c("region", "period")) %>%
    # Project future cooling useful energy demands [EJ/yr]
    mutate(proj = .data$activity * .data$unscaledDemand)


  ## Prepare Output ====
  # Merge historical and projected data with smooth transition

  # Calculate offset between historical and projected demand at end of history
  # This ensures continuity in the transition from historical to projected data
  delta <- projections %>%
    filter(!is.na(.data$space_cooling),
           .data$period <= endOfHistory) %>%
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(delta = .data$space_cooling - .data$proj) %>%
    select("region", "delta")

  # Merge historical and projected data with smooth convergence
  mergedData <- projections %>%
    left_join(delta, by = "region") %>%
    left_join(lambda, by = c("region", "period")) %>%
    mutate(
      # For future periods: blend offset-adjusted projection with pure projection
      # using convergence factor (maintains historical baseline initially, then converges)
      space_cooling = ifelse(is.na(.data[["space_cooling"]]),
                             (.data$proj + .data$delta) * (1 - .data$fullconv) + .data$proj * .data$fullconv,
                             .data[["space_cooling"]]),
      scenario = ifelse(.data$period <= endOfHistory, "history", scen),
      unit = NA,
      model = NA
    ) %>%
    pivot_longer(cols = c("space_cooling", "acPenetration"), names_to = "variable", values_to = "value") %>%
    select(all_of(cols))

  # Combine with original dataset
  data <- data %>%
    anti_join(mergedData, by = c("region", "period", "variable")) %>%
    rbind(mergedData)

  return(data)
}
