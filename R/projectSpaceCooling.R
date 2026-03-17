#' Project Space Cooling Energy Demand
#'
#' This function projects future space cooling useful energy demand and models air
#' conditioner (AC) penetration rates as a function of GDP per capita and cooling degree
#' days (CDD) using a logistic function.
#'
#' Space cooling energy demand is calculated using:
#' \deqn{space\_cooling = baseline + activity \cdot floor space \cdot uvalue \cdot CDD \cdot penetration rate}
#'
#' where \eqn{baseline} is the regional climate-independent baseline demand (intercept)
#' and \eqn{activity} is the regional cooling activity factor.
#'
#' The AC penetration rate component follows the logistic formula:
#' \deqn{penetration = 1 / (1 + \exp(\alpha - \beta \cdot (gdppop - gdppopShift)^\delta \cdot CDD^\gamma))}
#'
#' For cases where GDP per capita falls below the region-specific shift
#' (\eqn{gdppop < gdppopShift}), an extrapolation formula is used. While all
#' historical reference values are above the lower penetration boundary
#' \eqn{1/(1 + \exp(\alpha))}, projected values for regions with very low
#' historical AC adoption can fall below this boundary. The extrapolation formula
#' handles these cases while avoiding numerical issues and maintaining monotonic
#' behavior with GDP growth.
#'
#' The model incorporates regional adjustments through region-specific GDP per capita
#' shifts (\eqn{gdppopShift}) that are calibrated to match historical AC penetration
#' rates. This allows for regional variations in adoption patterns while maintaining
#' the global functional form of the penetration curve with global parameters
#' (\eqn{\alpha}, \eqn{\beta}, \eqn{\gamma}, \eqn{\delta}).
#'
#' Both the regional cooling activity and the climate-independent baseline demand are
#' derived from a linear fit on historical data. The cooling activity factor converges
#' toward a globally-derived activity factor over time, controlled by GDP per capita
#' thresholds and time-based convergence factors. The baseline demand component remains
#' fixed at regional values without convergence.
#'
#' The convergence process uses two different interpolation curves depending on whether
#' regional activity is above or below the global target:
#' \itemize{
#'   \item For regions where \eqn{activity_{regional} < activity_{global}} (ratio < 1):
#'         Uses \code{fullconv} (logistic S-curve) for traditional slow-fast-slow convergence
#'   \item For regions where \eqn{activity_{regional} \ge activity_{global}} (ratio >= 1):
#'         Uses \code{expconv} (exponential approach) with formula
#'         \eqn{f(x) = (1 - \exp(-sharpness \cdot x)) / (1 - \exp(-sharpness))}
#'         to provide quick initial departure from extreme values followed by smooth
#'         asymptotic approach to the global target, avoiding wave-like demand patterns
#' }
#'
#' The global convergence target is adjusted by a continuous tolerance function that
#' scales the deviation based on how far the regional value differs from the global
#' estimate, allowing for more gradual convergence for extreme regional deviations.
#' The tolerance function uses asymmetric bounds: \code{lowerBound = 0.8} (ensuring
#' at most 20% deviation below the global target) and \code{upperBoundFactor = 0.5}
#' (preserving 50% of the relative deviation for regions above the global target).
#' The convergence to this tolerance-adjusted global target then follows the same
#' asymmetric interpolation curves as described above (logistic for below-target regions,
#' exponential for above-target regions).
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
#'
#' @returns A data frame with the same structure as the input \code{data}, but with
#'   projected space cooling energy demand extending the
#'   historical "space_cooling" variable data.
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter select rename left_join group_by slice_max
#'   ungroup mutate right_join anti_join across all_of .data %>%
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stats lm setNames

projectSpaceCooling <- function(data,
                                config,
                                acOwnershipRates,
                                regPars,
                                endOfHistory,
                                lambda) {

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


  # HELPER FUNCTIONS -----------------------------------------------------------

  # Calculate asymmetric tolerance factor to allow gradual convergence when regional
  # activity deviates strongly from global target. Scales the global target based on
  # the ratio of regional to global activity:
  # - ratio < 1: polynomial decay toward lowerBound (stronger convergence below target)
  # - ratio ≥ 1: exponential approach preserving upperBoundFactor of deviation (gentler)
  # - ratio = 1: tolerance = 1.0 (no adjustment needed)
  calculateContinuousTolerance <- function(ratio, lowerBound = 0.8, upperBoundFactor = 0.5) {
    ifelse(
      ratio < 1,
      # Below target: tolerance is the adjusted target itself
      # Formula: tolerance = 1 - (1 - lowerBound) x (1 - ratio)^0.5
      1 - (1 - lowerBound) * (1 - ratio)^0.5,

      # Above target: tolerance preserves a fraction of the relative deviation
      # Formula: tolerance = 1 + smoothed_factor × (ratio - 1)
      # where smoothed_factor approaches upperBoundFactor as ratio increases
      1 + (1 - exp(-0.5 * (ratio - 1))) * upperBoundFactor * (ratio - 1)
    )
  }


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


  ## Regional GDP per Capita Shifts ====
  # Calculate region-specific gdppop shifts to match historical AC penetration
  # rates at the last observed data point, by shifting the global S-curve horizontally

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


  # Calculate region-specific GDP per capita shifts
  gdppopShift <- modelData %>%

    # For each region, use the most recent historical data point
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%

    # Derive gdppopShift by inverting the logistic function to find required shift
    mutate(gdppopShift = .data$gdppop - ((alpha - log(1 / .data$penetration - 1)) /
                                           (beta * .data$CDD^gamma))^(1 / delta)) %>%

    # Ensure all regions are represented
    right_join(data %>%
                 select("region") %>%
                 unique(),
               by = "region") %>%

    # Use zero shift for regions without valid historical data (use global curve as-is)
    mutate(gdppopShift = ifelse(is.finite(.data$gdppopShift), .data$gdppopShift, 0)) %>%
    select("region", "gdppopShift")


  ## Projections ====
  # Project AC penetration rates and calculate unscaled cooling demand

  projectionData <- data %>%
    filter(.data$variable %in% c("CDD", "gdppop", "space_cooling", "buildings", "uvalue")) %>%
    select("region", "period", "scenario", "variable", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    left_join(gdppopShift, by = "region") %>%
    mutate(
      # Project AC penetration rate with global parameters and regional gdppop shift
      acPenetration = ifelse(
        .data$gdppopShift < .data$gdppop,
        1 / (1 + exp(alpha - beta * (.data$gdppop - .data$gdppopShift)^delta * .data$CDD^gamma)),
        # extrapolation towards really low adoption values if GDP shift exceed GDP
        # increases monotonously with GDP and continues the actual formula above
        # otherwise arbitrary, but only affects adoption values close to zero
        1 / (1 + exp(alpha)) / (1 - (.data$gdppop - .data$gdppopShift) / 10000)
      ),

      # Calculate unscaled demand [EJ/yr] to determine regional activity factors
      # Formula: floor space [m²] * U-value [W/m²/K] * CDD [K·days] * conversion to EJ
      unscaledDemand = .data$buildings * .data$uvalue * .data$CDD * day2sec * .data$acPenetration / 1e12
    )

  # Extract historical data points for fitting activity factors
  fitData <- projectionData %>%
    filter(!is.na(.data$space_cooling),
           !is.na(.data$unscaledDemand),
           .data$space_cooling > 0)


  ### Activity factors ====
  # Calculate regional cooling activity factors from historical fit
  # allowing for a baseline demand independent of climate (flexible y-intercept)

  coolingActivity <- do.call(rbind, lapply(unique(projectionData$region), function(reg) {
    # Extract regional historical data
    regionalFitData <- fitData %>%
      filter(.data$region == reg)

    # If no historical data available, use all projection data for that region
    if (nrow(regionalFitData) == 0) {
      regionalFitData <- projectionData %>%
        filter(.data$region == reg)
    }

    # Germany shows a significant jump in the historical data, hence only data beyond this is used
    if (reg == "DEU") {
      regionalFitData <- regionalFitData %>%
        filter(.data$period >= 2015)
    }

    # Fit with flexible intercept (no "0 +" constraint)
    estimate <- lm("space_cooling ~ unscaledDemand", data = regionalFitData)

    if (estimate$coef[["(Intercept)"]] < 0) {
      estimate <- lm("space_cooling ~ 0 + unscaledDemand", data = regionalFitData)
    }

    # Extract coefficients
    activityCoef <- estimate$coef[["unscaledDemand"]]
    interceptCoef <- if ("(Intercept)" %in% names(estimate$coef)) estimate$coef[["(Intercept)"]] else 0

    projectionData %>%
      filter(.data$region == reg) %>%
      mutate(
        activityReg = activityCoef,
        interceptReg = interceptCoef
      ) %>%
      select("region", "period", "activityReg", "interceptReg")
  }))

  # Calculate global activity factor (excluding China due to comparitively low values + unreliable data)
  estimateGlobal <- lm("space_cooling ~ 0 + unscaledDemand",
                       data = fitData %>%
                         filter(!.data$region %in% c("CHN", "OAS")))


  # Convergence of regional to global activity factors
  # Keep regional intercepts fixed (no convergence for baseline demand)
  coolingActivity <- coolingActivity %>%
    mutate(
      activityGlo = estimateGlobal$coefficients[["unscaledDemand"]] * coolingActivityGloFactor,

      # Calculate relative distance between regional and global activity
      ratio = .data$activityReg / .data$activityGlo,

      # Calculate tolerance factor using continuous asymmetric tolerance function
      tolerance = calculateContinuousTolerance(.data$ratio, lowerBound = 0.8, upperBoundFactor = 0.5),

      # Calculate adjusted global activity with tolerance scaling
      activityGlo = .data$activityGlo * .data$tolerance
    ) %>%
    left_join(lambda %>%
                select("region", "period", "fullconv", "expconv"),
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

      # Select convergence type based on ratio of regional to global activity
      # If ratio < 1 (regional below global): use fullconv (S-curve)
      # If ratio >= 1 (regional above global): use expconv (exponential approach)
      lambdaConv = ifelse(.data$ratio < 1,
                          .data$fullconv,
                          .data$expconv),

      # Combine temporal and income-driven convergence
      lambda = pmin(.data$lambdaConv, .data$lambdaGDP),

      # Interpolate between regional and global activity based on convergence factor
      # Note: only activity (slope) converges, intercept remains regional
      activity = .data$activityReg * (1 - .data$lambda) + .data$activityGlo * .data$lambda,
      intercept = .data$interceptReg
    ) %>%
    ungroup() %>%
    select("region", "period", "activity", "intercept")


  ### Project UE cooling demand ====
  # Calculate final cooling useful energy demand in [EJ/yr]
  # Now includes intercept term for baseline demand

  projections <- projectionData %>%
    left_join(coolingActivity, by = c("region", "period")) %>%
    mutate(proj = .data$intercept + .data$activity * .data$unscaledDemand)


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

  # Merge historical and projected data
  mergedData <- projections %>%
    left_join(delta, by = "region") %>%
    left_join(lambda, by = c("region", "period")) %>%
    mutate(
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
