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
#' or cooling activity, is derived from a linear fit on historical data and boosted
#' over time for selected regions where an increase in cooling activity is expected.
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
#'   slice_max ungroup mutate right_join anti_join slice_min
#' @importFrom tidyr pivot_wider

projectSpaceCooling <- function(data,
                                config,
                                acOwnershipRates,
                                regPars,
                                endOfHistory,
                                lambda,
                                outliers = NULL) {

  # PARAMETERS -----------------------------------------------------------------

  # current scenario
  scen <- unique(data$scenario[data$scenario != "history"]) %>%
    as.character()

  day2sec <- 24 * 3600 # h/d * s/h

  # assumed future relative growth of activity factor between EOH and 2070
  activityBoost <- config[, "coolingActivityBoost"] %>%
    as.data.frame()


  # PROCESS DATA ---------------------------------------------------------------

  cols <- colnames(data)

  regPars <- setNames(regPars$value, regPars$variable)


  ## Regional beta corrections ====

  # full data set
  modelData <- acOwnershipRates %>%
    filter(!is.na(.data$value),
           .data$value != 0) %>%
    select("region", "period", "value") %>%
    rename("penetration" = "value") %>%

    # join GDP per capita and CDD data
    left_join(data %>%
                filter(.data$variable %in% c("CDD", "gdppop")) %>%
                select("region", "period", "variable", "value") %>%
                pivot_wider(names_from = "variable", values_from = "value"),
              by = c("region", "period"))


  # extract global fit parameters
  alpha <- regPars[["alpha"]]
  beta  <- regPars[["beta"]]
  gamma <- regPars[["gamma"]]
  delta <- regPars[["delta"]]


  betaReg <- modelData %>%
    # match last historical data point
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(betaReg = (alpha - log(1 / .data$penetration - 1)) / (.data$gdppop^delta * .data$CDD^gamma),
           betaReg = ifelse(.data$betaReg < 0, NA, .data$betaReg)) %>%

    # fill missing periods with global beta
    right_join(data %>%
                 select("region") %>%
                 unique(),
               by = "region") %>%
    mutate(betaReg = ifelse((is.na(.data$betaReg) | .data$betaReg < 0),
                            beta,
                            betaReg),
           # use global beta for outlier regions
           betaReg = ifelse(.data$region %in% outliers, beta, betaReg)) %>%
    select("region", "betaReg")


  ## Projections ====

  # project penetration rate
  projectionData <- data %>%
    filter(.data$variable %in% c("CDD", "gdppop", "space_cooling", "buildings", "uvalue")) %>%
    select("region", "period", "variable", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    left_join(betaReg, by = "region") %>%
    mutate(acPenetration = 1 / (1 + exp(alpha - .data$betaReg * .data$gdppop^delta * .data$CDD^gamma))) %>%

    # calculate unscaled demand to determine regional activity factors
    mutate(unscaledDemand = .data$buildings * .data$uvalue * .data$CDD * day2sec * .data$acPenetration / 1e12)


  # fit regional activity factors and project future UE demand
  projections <- do.call(rbind, lapply(unique(projectionData$region), function(reg) {
    # regional data
    regionalData <- projectionData %>%
      filter(.data$region == reg)

    # regional fit data
    regionalFitData <- regionalData %>%
      filter(!is.na(.data$space_cooling),
             .data$space_cooling > 0)

    # check if any data remains after filtering
    if (nrow(regionalFitData) == 0) {
      regionalFitData <- projectionData %>%
        filter(.data$region == reg)
    }

    # linear fit to determine historical activity
    regEstimate <- lm("space_cooling ~ 0 + unscaledDemand",
                      data = regionalFitData)

    # project UE space_cooling demand from historical trends
    regionalData$proj <- predict(regEstimate, newdata = regionalData)

    # temporally increase activity factor if specified in scenario assumptions
    boostFactor <- activityBoost$value[activityBoost$region == reg]

    regEstimate$coefficients[["unscaledDemand"]] <- regEstimate$coefficients[["unscaledDemand"]] * boostFactor

    # transition historical into scenario projections
    regionalData <- regionalData %>%
      left_join(lambda %>%
                  select("region", "period", "boost"),
                by = c("region", "period")) %>%
      mutate(projScen = predict(regEstimate, newdata = regionalData),
             proj = .data$proj * (1 - .data$boost) + .data$projScen * .data$boost) %>%
      select(-"projScen", -"boost")


    return(regionalData)

  }))


  ## Prepare Output ====

  # calculate offset from last historical demand
  delta <- projections %>%
    filter(!is.na(.data$space_cooling),
           .data$period <= endOfHistory) %>%
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(delta = .data$space_cooling - .data$proj) %>%
    select("region", "delta")

  # merge historical and projected data and prepare output format
  mergedData <- projections %>%
    left_join(delta, by = "region") %>%
    left_join(lambda, by = c("region", "period")) %>%
    mutate(space_cooling = ifelse(is.na(.data[["space_cooling"]]),
                                  (.data$proj + delta) * (1 - .data$fullconv) + .data$proj * .data$fullconv,
                                  .data[["space_cooling"]]),
           scenario = ifelse(.data$period <= endOfHistory, "history", scen),
           unit = NA,
           model = NA) %>%
    pivot_longer(cols = c("space_cooling", "acPenetration"), names_to = "variable", values_to = "value") %>%
    select(all_of(cols))

  # bind full dataset
  data <- data %>%
    anti_join(mergedData, by = c("region", "period", "variable")) %>%
    rbind(mergedData)

  return(data)
}
