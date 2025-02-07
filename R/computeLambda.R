#' Calculate time series of share parameter describing a linear or logistic
#' transition
#'
#' @param lastIdenticalYear first year of transition
#' @param endYear last year of transition
#' @param lambdaEnd final value of share parameter (0 ... 1)
#' @param type interpolation function c('logit', 'linear')
#' @param scaleLogit scale parameter of logistic function
#' @param startYearVector year from which to prepend yearly steps until
#'        \code{lastIdenticalYear}
#' @returns list with time series of share parameter
#'
#' @author Antoine Levesque
#'
#' @importFrom stats plogis qlogis

calcLambda <- function(lastIdenticalYear,
                       endYear,
                       lambdaEnd,
                       type = "logit",
                       scaleLogit = 0.15,
                       startYearVector = 2005,
                       step = 5) {
  # Internal Functions ---------------------------------------------------------

  computeLogit <- function(x) {
    low    <- plogis(0, location = 0.5, scale = scaleLogit)
    high   <- plogis(1, location = 0.5, scale = scaleLogit)
    gap    <- high - low
    mean   <- 0.5
    middle <- qlogis(mean, location = mean, scale = scaleLogit)

    x <- (plogis(x, location = mean, scale = scaleLogit) - middle) / gap + middle

    return(x)
  }


  # Parameter ------------------------------------------------------------------

  eps <- 1e-8


  # Process Data ---------------------------------------------------------------

  if (lambdaEnd > 1 || lambdaEnd < 0) stop("lambdaEnd must lie between 0 and 1")

  lambdaYears <- seq(from = lastIdenticalYear, to = endYear, by = step)

  if (type == "logit") {
    x <- seq(0, 1, length.out = length(lambdaYears))
    lambda <- c(computeLogit(x) * lambdaEnd,
                rep(lambdaEnd, max(0, (2100 - endYear) / 5)))
    lambda[lambda < eps] <- 0

  } else if (type == "linear") {

    lambda <- c(seq(from = 0, to = lambdaEnd, length.out = length(lambdaYears)),  # start with a non 0 value
                rep(lambdaEnd, max(0, (2100 - endYear) / step)))
  }


  names(lambda) <- seq(lastIdenticalYear, max(2100, endYear), by = step)

  addYears  <- setdiff(seq(startYearVector, lastIdenticalYear, 1), names(lambda))
  lambdaAdd <- rep(0, length(addYears))

  names(lambdaAdd) <- addYears

  lambda <- c(lambdaAdd, lambda)
  lambda <- lambda[sort(names(lambda))]

  return(lambda)
}


#' Compute region-wise share of convergence from regionally differentiated
#' efficiencies towards long-term assumption for a specific scenario.
#'
#' @param scenAssumpSpeed years of full regional convergence for each scenario
#' @param lastIdenticalYear year from which to start convergence
#' @param varySpeed scale speed period of transition from 2015 onward
#'        (e.g. full convergence in 2075 and \code{varySpeed = 2} leads to
#'        full convergence in 2045)
#' @param startYearVector year from which to start time series (yearly steps)
#'
#' @return data.frame with region-wise convergence shares
#'
#' @importFrom dplyr pull filter mutate
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom data.table :=

compLambdaScen <- function(scenAssumpSpeed,
                           lastIdenticalYear,
                           varySpeed = 1,
                           startYearVector = 2005) {
  # Internal Functions ---------------------------------------------------------

  compEvolutionShares <- function(type, reg) {
    # upper temporal convergence boundary
    upperBound <- scenAssumpSpeed %>%
      filter(.data[["region"]] == reg) %>%
      pull(type)

    # adapted upper boundary
    endYear <- round(((upperBound - 2015) / varySpeed + 2015) / 5) * 5

    # calc convergence shares
    tmp <- calcLambda(lastIdenticalYear, endYear, 1,
                      type = ifelse(type == "fullconv", "logit", "linear"),
                      startYearVector = startYearVector)

    tmp <- tmp[names(tmp) <= 2100]

    # build output
    lambda <- data.frame(region = reg,
                         period = names(tmp) %>%
                           as.numeric(),
                         row.names = c()) %>%
      mutate(!!type := unname(tmp))

    return(lambda)
  }


  # Parameter ------------------------------------------------------------------

  # scenario
  scen  <- getScenarios(scenAssumpSpeed)

  # evolution types
  types <- colnames(scenAssumpSpeed)
  types <- types[!(types %in% c("region", "scenario"))]


  # Process Data ---------------------------------------------------------------

  # calculate and merge convergence shares for different types
  lambda <- Reduce(
    function(x, y) merge(x, y, by = c("period", "region"), all = TRUE),
    lapply(
      types, function(t) {
        do.call(
          "rbind",
          lapply(
            getRegs(scenAssumpSpeed), function(r) {
              return(compEvolutionShares(type = t, reg = r))
            }
          )
        )
      }
    )
  )

  # add scenario info
  lambda <- lambda %>%
    mutate(scenario = scen)

  return(lambda)
}
