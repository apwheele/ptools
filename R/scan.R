#' Scan statistic approximation for counts in moving window
#'
#' Naus scan statistic approximation for Poisson counts in moving window over a particular time period
#'
#' @param L number of time periods in the window
#' @param k window scan time period
#' @param mu Poisson averaged per single time period
#' @param n observed count
#'
#' @details When examining counts of items happening in a specific, discrete set of windows,
#' e.g. counts of crime per week, one can use the Poisson PMF to determine the probability of
#' getting an observation over a particular value. For example, if you have a mean of 1 per week,
#' the probability of observing a single week with a count of 6 or more is `ppois(5,1,FALSE)`
#' , approximately 0.0006. But if you have monitored a series over 5 years, (260 weeks), then the
#' the expected number of seeing at least one 6 count in the time period is `ppois(5,1,FALSE)*260`
#' , over 15%.
#'
#' Now imagine we said "in this particular week span, I observed a count of 6". So it is not in
#' pre-specified week, e.g. Monday through Sunday, but examining over *any* particular moving window. 
#' Naus (1982) provides an approximation to correct for this moving window scan. In this example,
#' it ends up being close to 50% is the probability of seeing a moving window of 6 events.
#' 
#'
#' @returns
#' A single numeric value, the probability of observing that moving window count
#' @export
#' @examples
#' \donttest{
#' # Spiegelhalter example (replicates COOLSerdash's estimates in comments)
#' scanw(208,2,0.6,6)
#'
#' # Example in description
#' scanw(260,1,1,6)
#' }
#' @references
#' Aberdein, J., & Spiegelhalter, D. (2013). Have London's roads become more dangerous for cyclists?. 
#' *Significance*, 10(6), 46-48.
#' 
#' Naus, J.I. (1982). Approximations for distributions of scan statistics. 
#' *Journal of the American Statistical Association*, 77, 177-183.
scanw <- function(L,k,mu,n){
    pn2 <- p2(mu,n)
    pn3 <- p3(mu,n)
    pnr <- pn3/pn2
    res <- pn2*(pnr^(L-k))
    return(1-res)
}

# Scan stat https://understandinguncertainty.org/when-cluster-real-cluster
# note I am able to replicate COOLSerdash's estimates in the comments
fns <- function(mu,n,s){
    if (n < s){
        return(0)
    } else {
        res <- dpois(n-s,mu)
        return(res)
    }
}

Fns <- function(mu,n,s){
    if (n < s){
        return(0)
    } else {
        x <- n - s
        res <- ppois(x,mu)
        return(res)
    }
}


p2 <- function(mu,n){
    Fn_2 <- Fns(mu,n,1)^2
    p1 <- (n - 1)*dpois(n,mu)*fns(mu,n,2)
    p2 <- (n - 1 - mu)*dpois(n,mu)
    Fn_3 <- Fns(mu,n,3)
    fin <- Fn_2 - p1 - p2*Fn_3
    return(fin)
}

p3 <- function(mu,n){
    Fn_3 <- Fns(mu,n,1)^3
    a1_1 <- 2*dpois(n,mu)*Fns(mu,n,1)
    a1_2 <- (n-1)*Fns(mu,n,2) - mu*Fns(mu,n,3)
    A1 <- a1_1*a1_2
    a2_1 <- 0.5*dpois(n,mu)^2
    a2_2 <- (n-1)*(n-2)*Fns(mu,n,3) - 2*(n-2)*mu*Fns(mu,n,4)
    a2_3 <- mu^2*Fns(mu,n,5)
    A2 <- a2_1*(a2_2 + a2_3)
    A3 <- 0
    for (i in 1:(n-1)){
        fl3 <- fns(mu,2*n,i)*Fns(mu,i,1)^2
        A3 <- A3 + fl3
    }
    A4 <- 0
    for (i in 1:(n-2)) {
        fl4_1 <- fns(mu,2*n,i)*dpois(i,mu)
        fl4_2 <- (i-1)*Fns(mu,i,2) - mu*Fns(mu,i,3)
        A4 <- A4 + fl4_1*fl4_2
    }
    fin <- Fn_3 - A1 + A2 + A3 - A4
    return(fin)
}