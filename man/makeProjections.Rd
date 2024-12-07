% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeProjections.R
\name{makeProjections}
\alias{makeProjections}
\title{Project future from historic data with linear and non-linear regressions}
\usage{
makeProjections(
  df,
  formul,
  scenAssumpEcon,
  lambda,
  lambdaDelta,
  scenAssumpCorrect,
  maxReg = NULL,
  outliers = NULL,
  apply0toNeg = TRUE,
  transformVariable = NULL,
  transformVariableScen = NULL,
  applyScenFactor = FALSE,
  convReg = "absolute",
  avoidLowValues = FALSE,
  endOfHistory = 2020,
  scenAssumpRegion = NULL,
  interpolate = TRUE
)
}
\arguments{
\item{df}{data.frame with required variables}

\item{formul}{character, projection formula}

\item{scenAssumpEcon}{data.frame with projection parameters}

\item{lambda}{data.frame with values that go from zero to 1 over time
describing the transition towards the projection scenario}

\item{lambdaDelta}{data.frame with values that go from zero to 1 over time
describing the transition towards the projection scenario for delta targets}

\item{scenAssumpCorrect}{data.frame with scenario corrections}

\item{maxReg}{maximum value}

\item{outliers}{character vector with regions not to be considered in
regression}

\item{apply0toNeg}{logical, if \code{TRUE}, negative values are replaced with
zero}

\item{transformVariable}{variable transformation}

\item{transformVariableScen}{scenario variable transformation}

\item{applyScenFactor}{logical, if \code{TRUE} a given scaling factor is
applied}

\item{convReg}{character, describing what deltas are considered:
\itemize{
\item \code{"absolute"}: absolute deviations from prediction
\item \code{"relative"}: relative deviations from prediction
}}

\item{avoidLowValues}{logical, if \code{TRUE} projection values are clipped
to avoid that they fall below the start value (for increasing projections)
or final value (for decreasing projections)}

\item{endOfHistory}{upper temporal boundary of historical data}

\item{scenAssumpRegion}{regional convergence assumptions}

\item{interpolate}{logical}
}
\value{
data.frame with added projection
}
\description{
Project future from historic data with linear and non-linear regressions
}
\author{
Antoine Levesque
}
