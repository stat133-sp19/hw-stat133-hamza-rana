% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainfunc.R
\name{bin_distribution}
\alias{bin_distribution}
\title{bin_distribution}
\usage{
bin_distribution(trials = 5, prob = 0.5)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{probability of success within one trial}
}
\value{
the probability distribution specified by the parameters
}
\description{
creates a data frame with a distribution of probabilites for the specified number of trials
}
\examples{

# default
bin_distribution()

# another example
bin_distribution(trials = 3, prob = 0.5)

}
