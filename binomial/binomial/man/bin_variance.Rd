% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainfunc.R
\name{bin_variance}
\alias{bin_variance}
\title{bin_variance}
\usage{
bin_variance(trials = 5, prob = 0.5)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{probability of success within one trial}
}
\value{
the numerical value of the variance of the given data
}
\description{
calculates the variance of the given binomial distribution
}
\examples{

# default
bin_variance()

# another example
bin_variance(trials = 3, prob = 0.5)

}
