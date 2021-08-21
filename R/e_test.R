#' Poisson E-test
#'
#' Tests differences in two Poisson means or rates.
#'
#' @param k1 scaler Poisson count
#' @param k2 scaler Poisson count
#' @param n1 scaler divisor for k1 (e.g. rate per unit time or per area), default 1
#' @param n2 scaler divisor for k2 (e.g. rate per unit time or per area), default 1
#' @param d scaler amends the null test by a constant amount, default 0
#' @param eps scaler where to terminate sum in testing larger deviations, default 1e-20
#' @param silent boolean if TRUE, does not print error messages
#'
#' @details This e-test tests the differences in two Poisson counts or rates. The null is more formally:
#' 
#' \deqn{k_1/n_1 = k_2/n_2 + d}{k1/n1 = k2/n2 + d}
#'
#' Note, I would be wary using the test for Poisson counts over 100 (the tail approximation in the sums will have issues, as the PMF is so spread out). (It is also the case with *very* large k's, e.g. `e_test(4000,4000)` my function could run out of memory.) In that case may use the n arguments to make it a rate per some unit time (which can change the p-value, although for smaller counts/rates should be very close).  
#' @returns
#' A scaler p-value. Will return -1 if inputs don't make sense and print an error message, e.g. `e_test(0,0)` is undefined and will return a -1.
#' @export
#' @examples
#' # For small N, changes in rates should result in same p-value minus floating point differences
#' e_test(3,0)
#' e_test(3,0,2,2)
#' 
#' # Not defined
#' e_test(0,0) #returns -1 and prints warning
#' 
#' # The same rates
#' e_test(20,10,4,2)
#' e_test(10,5,2,1) #not quite the same
#' 
#' # Order of counts/rates should not matter
#' e_test(6,2) #second example from Krishnamoorthy article
#' e_test(2,6) #when d=0, can switch arguments and get the same p-value
#' 
#' # These are not the same however, due to how the variance estimates work
#' e_test(3,2)
#' e_test(3,1,d=1)
#' @seealso [wdd()], can use that function for a normal based approximation to the difference in Poisson means as well as pre/post designs
#' @references
#' Krishnamoorthy, K., & Thomson, J. (2004). A more powerful test for comparing two Poisson means. *Journal of Statistical Planning and Inference*, 119(1), 23-35.
e_test <- function(k1,k2,n1=1,n2=1,d=0,eps=1e-20,silent=FALSE){
    #this function finds the integer for poisson
    #PMF that has a density of less than eps
    #where to terminate the sum
    minPMF <- function(r,eps=1e-20){
        p <- 1
        int <- trunc(r) #only evaluate right tail
        while(p > eps){
            int <- int + 1
            p <- stats::dpois(int,r)
        }
        return(int)
    }
    if ( (k1 == 0 & k2 == 0) ){
        if (!silent){ cat('\n\tNot defined for case when both values are zero\n\n') }
        return(-1)
    }
    else if ( (k1 < 0 | k2 < 0 | n1 <= 0 | n2 <= 0) ){
        if (!silent){ cat('\n\tk and n arguments should be positive values\n\n') }
        return(-1)
    }
    r1 <- k1/n1 #rate first process
    r2 <- k2/n2 #rate second process
    lhat <- (k1 + k2)/(n1 + n2) - (d*n1)/(n1 + n2) #mean rate
    Tk <- abs((r1 - r2 - d)/sqrt(lhat)) #test statistic
    d1 <- n2*lhat     #updated so termination of sum did not happen too early
    d2 <- n1*(lhat+d) #problem when using lhat instead of d's
    int1 <- minPMF(r=max(d1,d2),eps=eps) #where to terminate sum
    # Seems unnecessary when taking max for prior
    #if (d==0){int2 <- int1} else {int2 <- minPMF(r=d2,eps=eps)}
    df <- expand.grid(x1 = 0:int1,x2 = 0:int1) #for really large lambdas
    df$p1 <- stats::dpois(df$x1,d2)            #this might not fit in memory 
    df$p2 <- stats::dpois(df$x2,d1)     
    df$rp <- (df$x1 + df$x2)/(n1 + n2)
    df$Tx <- abs((df$x1/n1 - df$x2/n2 - d))/sqrt(df$rp)
    df$I <- 1*(df$Tx >= Tk)
    df$ptot <- df$p1 * df$p2 * df$I
    return(sum(df$ptot, na.rm=TRUE)) #x1 = 0 and x2 = 0 produces nulls for Tx, rp=0
}


######################
#ToDo 
# Power calculations in the same paper?
# Extensions to negative binomial?
######################