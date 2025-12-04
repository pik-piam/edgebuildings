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
#' \deqn{penetration = 1 / (1 + \exp(\alpha - \beta \cdot gdppop^\delta \cdot CDD^\gamma))}
#'
#' The model incorporates regional adjustments through region-specific beta
#' coefficients (\eqn{\beta_{reg}}) that are calibrated to match historical
#' AC penetration rates. This allows for regional variations in adoption patterns
#' while maintaining the global functional form of the penetration curve.
#'
#' For regions with historical data, regional beta coefficients are derived to
#' match the last historical reference point. The regional scaling parameter (\eqn{\phi_1}),
#' or cooling activity, is derived from a linear fit on historical data. Regional
#' activity factors converge toward a globally-derived activity factor over time,
#' controlled by GDP per capita thresholds and time-based convergence factors. The
#' global convergence target is adjusted by tolerance bands that scale the deviation
#' based on how far the regional value differs from the global estimate, allowing
#' for more gradual convergence for extreme regional deviations.
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
#' @param toleranceTable data frame with tolerance bands for regional to global activity convergence
#'
#' @returns A data frame with the same structure as the input \code{data}, but with
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
                                toleranceTable = NULL) {

  # PARAMETERS -----------------------------------------------------------------

  # Extract current scenario
  scen <- row.names(config) %>%
    unique()

  # Conversion factor: days to seconds
  day2sec <- 24 * 3600

  # GDP per capita threshold for activity convergence [USD/cap]
  incomeThresholdCooling <- config[scen, "incomeThresholdCooling"] %>%
    unlist()

  # Global activity scaling factor for convergence target
  coolingActivityGloFactor <- config[scen, "coolingActivityGloFactor"] %>%
    unlist()


  # PROCESS DATA ---------------------------------------------------------------

  ## Extract AC penetration regression parameters ====
  # Store column structure for output formatting
  cols <- colnames(data)

  # Convert regression parameters to named vector for easy access
  regPars <- setNames(regPars$value, regPars$variable)

  # Extract global fit parameters from logistic regression
  alpha <- regPars[["alpha"]]  # Intercept parameter
  beta  <- regPars[["beta"]]   # Global scaling coefficient
  gamma <- regPars[["gamma"]]  # CDD exponent
  delta <- regPars[["delta"]]  # GDP per capita exponent


  ## Regional beta corrections ====
  # Calculate region-specific beta coefficients to match historical AC penetration
  # rates at the last observed data point, using corrected gamma and delta exponents

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

  # Calculate region-specific beta coefficients using corrected exponents
  betaReg <- modelData %>%
    # For each region, use the most recent historical data point
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # Derive regional beta by inverting the logistic function with corrected exponents
    mutate(betaReg = (alpha - log(1 / .data$penetration - 1)) / (.data$gdppop^(delta) * .data$CDD^(gamma)),
           betaReg = ifelse(.data$betaReg < 0, NA, .data$betaReg)) %>%
    # Ensure all regions are represented
    right_join(data %>%
                 select("region") %>%
                 unique(),
               by = "region") %>%
    mutate(# Use global beta for regions without valid historical data
           betaReg = ifelse((is.na(.data$betaReg) | .data$betaReg < 0),
                            beta,
                            .data$betaReg)) %>%
    select("region", "betaReg")

  ## Projections ====
  # Project AC penetration rates and calculate unscaled cooling demand

  projectionData <- data %>%
    filter(.data$variable %in% c("CDD", "gdppop", "space_cooling", "buildings", "uvalue")) %>%
    select("region", "period", "scenario", "variable", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    left_join(betaReg, by = "region") %>%
    mutate(
      # Project corrected AC penetration rate with correction parameters
      acPenetration = 1 / (1 + exp(alpha - .data$betaReg * .data$gdppop^(delta)
                                   * .data$CDD^(gamma))),

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

    estimate <- lm("space_cooling ~ 0 + unscaledDemand", data = regionalFitData)

    projectionData %>%
      filter(.data$region == reg) %>%
      mutate(activityReg = estimate$coef[["unscaledDemand"]]) %>%
      select("region", "period", "activityReg")
  }))

  # Calculate global activity factor (excluding China due to comparitively low values + unreliable data)
  estimate <- lm("space_cooling ~ 0 + unscaledDemand",
                 data = fitData %>%
                   filter(!.data$region %in% c("CHN", "OAS")))


  # Implement convergence of regional to global activity factors
  coolingActivity <- coolingActivity %>%
    mutate(
      activityGlo = estimate$coefficients[["unscaledDemand"]] * coolingActivityGloFactor,

      # Calculate relative distance between regional and global activity
      ratio = .data$activityReg / .data$activityGlo,

      # Get binned tolerance
      tolerance = .getTolerance(.data$ratio, toleranceTable),

      # Calculate adjusted global activity with tolerance scaling
      activityGlo = .data$activityGlo * .data$tolerance
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
      lambdaGDP = pmin(1, pmax(0,
                               (.data$gdppop - .data$gdppop[.data$period == endOfHistory]) /
                                 pmax(incomeThresholdCooling - .data$gdppop[.data$period == endOfHistory], 1))),

      # Combine temporal and income-driven convergence
      lambda = pmin(.data$fullconv, .data$lambdaGDP),

      # Interpolate between regional and global activity based on convergence factor
      activity = .data$activityReg * (1 - .data$lambda) + .data$activityGlo * .data$lambda
    ) %>%
    ungroup() %>%
    select("region", "period", "activity")


  ### Project UE cooling demand ====
  # Calculate final cooling useful energy demand in [EJ/yr]

  projections <- projectionData %>%
    left_join(coolingActivity, by = c("region", "period")) %>%
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
      # For some regions the offset needs to be decreased, otherwise demands drop below zero
      space_cooling = ifelse(is.na(.data[["space_cooling"]]),
                             .data$proj + .data$delta * (1 - .data$fullconv),
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



#' Get Tolerance Value from Tolerance Table
#'
#' Looks up the tolerance value for a given ratio based on the tolerance table
#' which defines tolerance bands for regional to global activity convergence.
#'
#' @param ratio numeric vector of activity ratios (regional/global)
#' @param tolTable data frame with columns ratio_min, ratio_max, and tolerance
#'
#' @returns numeric vector of tolerance values

.getTolerance <- function(ratio, tolTable) {
  # Use findInterval for efficient bin assignment
  breaks <- c(tolTable$ratio_min, Inf)
  indices <- findInterval(ratio, breaks, rightmost.closed = FALSE)

  # Map indices to tolerance values (add 1.0 as default for out-of-bounds)
  tolerances <- c(tolTable$tolerance, 1.0)
  return(tolerances[indices])
}
