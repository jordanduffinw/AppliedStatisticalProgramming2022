% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardError.R
\name{standardError}
\alias{standardError}
\alias{SE}
\title{Standard Error}
\usage{
standardError(y, SEtype, B = 1000)
}
\arguments{
\item{y}{The vector of observed data}

\item{SEtype}{The type of SE, can choose either `basic` or `bootstrap`}

\item{B}{The number of bootstrap iterations to run if `SEtype` is `bootstrap`. Defaults to 1000.}
}
\value{
A single numeric output:
    \item{SE}{The standard error for the observed data.}
}
\description{
Calculates the standard error for the data using either a basic or bootstrap method
}
\examples{
set.seed(666)
y <- sample(x = 1:50, size = 25, replace = TRUE)
standardError(y, "basic")
standardError(y, "bootstrap", B = 1000)

}
\seealso{
\code{\link{logLik}}, \code{\link{mle}}, \code{\link{estimatePoisson}}
}
\author{
Jordan Duffin Wong: \email{jordan.d.wong@wustl.edu}
}
