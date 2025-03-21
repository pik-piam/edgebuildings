% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getUvalues.R
\name{makeUValuePrediction}
\alias{makeUValuePrediction}
\title{Make prediction of future U-Values}
\usage{
makeUValuePrediction(dataEst, dataPred, formul)
}
\arguments{
\item{dataEst}{estimate for prediction}

\item{dataPred}{future degree day values for Uvalue prediction}

\item{formul}{correlation formular for variables of interest}
}
\value{
scenario-wise U-value predictions
}
\description{
Future U-values are predicted by assuming an exponential relationship between
the sum of annual degree days (HDD + CDD) and the heat conduction value of the
outer building shell. Further remarks on the assumptions are in the description
of \code{getUvalues}.
}
