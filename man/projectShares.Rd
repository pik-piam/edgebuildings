% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getShareECprojections.R
\name{projectShares}
\alias{projectShares}
\title{Project energy carrier (EC) shares to pre-set target value}
\usage{
projectShares(df, var, xTar, yTar, phaseOutMaxEnd, phaseOutStart)
}
\arguments{
\item{df}{data.frame with historic EC shares and gdp per capita}

\item{var}{carrier/enduse combination to be phased out}

\item{xTar}{region-wise target values of carrier FE share}

\item{yTar}{year of full convergence}

\item{phaseOutMaxEnd}{upper temporal boundary of phaseout time period}

\item{phaseOutStart}{starting point of phaseOut}
}
\value{
projected EC shares
}
\description{
Project the share of specific demand w.r.t. absolute FE for individual energy
carrier/enduse combinations to a pre-defined value target in a pre-defined
temporal period.
}
