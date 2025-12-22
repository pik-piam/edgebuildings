#' Project Space Cooling Energy Demand
#'
#' This function projects future space cooling useful energy demand and models air
#' conditioner (AC) penetration rates as a function of GDP per capita and cooling degree
#' days (CDD) using a logistic function.
#'
#' Space cooling energy demand is calculated using:
#' \deqn{space\_cooling =  \phi_1 \cdot floor space \cdot uvalue \cdot CDD \cdot penetration rate}
#'
#' The AC penetration rate component follows the logistic formula:
#' \deqn{penetration = 1 / (1 + \exp(\alpha - \beta \cdot (gdppop - gdppopShift)^\delta \cdot CDD^\gamma))}
#'
#' The model incorporates regional adjustments through region-specific GDP per capita
#' shifts (\eqn{gdppopShift}) that are calibrated to match historical AC penetration
#' rates. This allows for regional variations in adoption patterns while maintaining
#' the global functional form of the penetration curve with global parameters
#' (\eqn{\alpha}, \eqn{\beta}, \eqn{\gamma}, \eqn{\delta}).
#'
#' The regional scaling parameter (\eqn{\phi_1}),
#' or cooling activity, is derived from a linear fit on historical data. Regional
#' activity factors converge toward a globally-derived activity factor over time,
#' controlled by GDP per capita thresholds and time-based convergence factors. The
#' global convergence target is adjusted by a continuous tolerance function that
#' scales the deviation based on how far the regional value differs from the global
#' estimate, allowing for more gradual convergence for extreme regional deviations.
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
#' @param toleranceKeyPoints data frame with key points (ratio, tolerance) for continuous
#'   tolerance interpolation in regional to global activity convergence
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
                                lambda,
                                toleranceKeyPoints = NULL) {

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
    mutate(gdppopShift = ifelse(is.na(.data$gdppopShift) | is.infinite(.data$gdppopShift), 0, .data$gdppopShift)) %>%
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

      # Get continuous tolerance using spline interpolation
      tolerance = approx(x = toleranceKeyPoints$ratio,
                         y = toleranceKeyPoints$tolerance,
                         xout = .data$ratio,
                         method = "linear",
                         rule = 2)$y,

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
