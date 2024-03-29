% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_pois.R
\name{check_pois}
\alias{check_pois}
\title{Checks the fit of a Poisson Distribution}
\usage{
check_pois(counts, min_val, max_val, pred, silent = FALSE)
}
\arguments{
\item{counts}{vector of counts, e.g. c(0,5,1,3,4,6)}

\item{min_val}{scaler minimum value to generate the grid of results, e.g. \code{0}}

\item{max_val}{scaler maximum value to generate the grid of results, e.g. \code{max(counts)}}

\item{pred}{can either be a scaler, e.g. \code{mean(counts)}, or a vector (e.g. predicted values from a Poisson regression)}

\item{silent}{boolean, do not print mean/var stat messages, only applies when passing scaler for pred (default \code{FALSE})}
}
\value{
A dataframe with columns
\itemize{
\item \code{Int}, the integer value
\item \code{Freq}, the total observed counts within that Integer value
\item \code{PoisF}, the expected counts according to a Poisson distribution with mean/pred specified
\item \code{ResidF}, the residual from \code{Freq - PoisF}
\item \code{Prop}, the observed proportion of that integer (0-100 scale)
\item \code{PoisD}, the expected proportion of that integer (0-100 scale)
\item \code{ResidD}, the residual from \code{Prop - PoisD}
}
}
\description{
Provides a frequency table to check the fit of a Poisson distribution to empirical data.
}
\details{
Given either a scaler mean to test the fit, or a set of predictions (e.g. varying means predicted from a model), checks whether the data fits a given Poisson distribution over a specified set of integers. That is it builds a table of integer counts, and calculates the observed vs the expected distribution according to Poisson. Useful for checking any obvious deviations.
}
\examples{
# Example use for constant over the whole sample
set.seed(10)
lambda <- 0.2
x <- rpois(10000,lambda)
pfit <- check_pois(x,0,max(x),mean(x))
print(pfit)
# 82\% zeroes is not zero inflated -- expected according to Poisson!

# Example use if you have varying predictions, eg after Poisson regression
n <- 10000
ru <- runif(n,0,10)
x <- rpois(n,lambda=ru)
check_pois(x, 0, 23, ru)

# If you really want to do a statistical test of fit
chi_stat <- sum((pfit$Freq - pfit$PoisF)^2/pfit$PoisF)
df <- length(pfit$Freq) - 2
stats::dchisq(chi_stat, df) #p-value
# I prefer evaluating specific integers though (e.g. zero-inflated, longer-tails, etc.)

}
