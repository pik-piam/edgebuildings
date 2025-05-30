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
#' \deqn{penetration = 1 / (1 + \exp(\alpha - \beta \cdot gdppop \cdot CDD))}
#'
#' The model incorporates two key assumptions:
#' \enumerate{
#'   \item All regional penetration curves originate from the same point at
#'         gdppop = 0
#'   \item The curve elongation parameter (\eqn{\phi_3}) decreases monotonically
#'         with rising CDDs to account for dampened/accelerated technology adoption
#'         due to regional climate
#' }
#'
#' For regions with historical data, regional beta coefficients are derived to
#' match the last historical reference point. The scaling parameter (\eqn{\phi_1})
#' is adjusted to match historical useful energy demand and maintain consistency
#' in future projections.
#'
#' The function assumes that the input \code{data} contains a single non-"history"
#' scenario.
#'
#'
#' @param data data frame with historical and scenario data
#' @param acOwnershipRates data frame with historical AC penetration rates
#'
#' @return A data frame with the same structure as the input \code{data}, but with
#'   projected space cooling energy demand extending the
#'   historical "space_cooling" variable data.
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr filter select rename left_join group_by
#'   slice_max ungroup mutate right_join anti_join
#' @importFrom tidyr pivot_wider

projectSpaceCooling <- function(data,
                                acOwnershipRates,
                                endOfHistory) {

  # PARAMETERS -----------------------------------------------------------------

  # current scenario
  scen <- unique(data$scenario[data$scenario != "history"]) %>%
    as.character()


  # PROCESS DATA ---------------------------------------------------------------

  cols <- colnames(data)

  ## Create model estimate ====

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

  # linear estimate
  estimate <- lm("log(1/penetration - 1) ~ gdppop:CDD", data = modelData)


  ## Regional beta corrections ====

  alpha <- estimate$coefficients[["(Intercept)"]]
  beta  <- (-1) * estimate$coefficients[["gdppop:CDD"]]   # * (-1) because we use f(x) = alpha - beta*x

  betaReg <- modelData %>%
    # match last historical data point
    group_by(across(all_of(c("region")))) %>%
    slice_max(order_by = .data$period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(betaReg = (alpha - log(1 / .data$penetration - 1)) / (.data$gdppop * .data$CDD)) %>%

    # fill missing periods with global beta
    right_join(data %>%
                 select("region") %>%
                 unique(),
               by = "region") %>%
    mutate(betaReg = ifelse((is.na(.data$betaReg) | .data$betaReg < 0), beta, .data$betaReg)) %>%
    select("region", "betaReg")

  ## Projections ====

  # project penetration rate
  projectionData <- data %>%
    filter(.data$variable %in% c("CDD", "gdppop", "space_cooling", "buildings", "uvalue")) %>%
    select("region", "period", "variable", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    left_join(betaReg, by = "region") %>%
    mutate(penetrationProj = 1 / (1 + exp(alpha - .data$betaReg * .data$gdppop * .data$CDD)))

  # calculate regional phi1 to match historical data and assume EOH level for future periods
  phi1Reg <- projectionData %>%
    mutate(phi1Reg = .data$space_cooling /
             (.data$buildings * .data$uvalue * .data$CDD * .data$penetrationProj) * 1e6) %>%
    select("region", "period", "phi1Reg") %>%
    interpolate_missing_periods(value = "phi1Reg", expand.values = TRUE)

  # project UE space_cooling demand
  projections <- projectionData %>%
    left_join(phi1Reg, by = c("region", "period")) %>%
    mutate(proj = (.data$buildings * .data$uvalue * .data$CDD * .data$penetrationProj * phi1Reg) / 1e6) %>%
    filter(!is.na(.data$proj))


  ## Prepare Output ====

  # merge historical and projected data and prepare output format
  mergedData <- projections %>%
    mutate(space_cooling = ifelse(is.na(.data[["space_cooling"]]),
                                  .data$proj,
                                  .data[["space_cooling"]])) %>%
    rename("value" = "space_cooling") %>%
    mutate(variable = "space_cooling",
           scenario = ifelse(.data$period <= endOfHistory, "history", scen),
           unit = NA,
           model = NA) %>%
    select(all_of(cols))

  # bind full dataset
  data <- data %>%
    anti_join(mergedData, by = c("region", "period", "variable")) %>%
    rbind(mergedData)

  return(data)
}
