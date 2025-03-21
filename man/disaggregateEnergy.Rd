% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildingsProjections.R
\name{disaggregateEnergy}
\alias{disaggregateEnergy}
\title{Compute carrier-specific useful/final energy}
\usage{
disaggregateEnergy(data, eff, shares, enduses)
}
\arguments{
\item{data}{data.frame containing enduse-specific aggregate FE/UE data}

\item{eff}{data.frame containing final to useful energy conversion efficiencies}

\item{shares}{data.frame containing FE carrier shares w.r.t. specific enduses}

\item{enduses}{list of enduses}
}
\value{
data.frame containing (non-)aggregated FE/UE data for enduse-carrier pairs
}
\description{
Carrier-specific values for useful and final energy are calculated using
efficiencies for FE->UE conversion for specific carrier-enduse combinations
and shares of energy carriers w.r.t. to specific enduses.
}
\details{
Energy-type-specific weights are calculated to disaggregate scenario-wise
projected enduse-specific data stored in \code{data}.
}
