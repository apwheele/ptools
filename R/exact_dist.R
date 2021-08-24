###########################
# I don't see much point in 
# exporting the functions besides 
# 
###########################

#functions used in ExactProb - note these do no error checking 
#for observations in zero probability bins - so can return non-sensical results
#Minimalist chi square, default equal probability for each bin
chiStat <- function(v,p=rep(1/length(v),length(v))){
  n <- sum(v)
  e <- n*p
  r <- (v-e)^2
  c <- ifelse(e>0,r/e,0) #this is for zero prob bins for alternate process
  return(sum(c))
}
#Kuipers V - probably copied this from the circular package at some point
KuiperTest <- function(v,p=rep(1/length(v),length(v))){
  n <- sum(v)
  u <- cumsum(p) 
  s <- v/n
  e <- cumsum(s)        #ecdf
  Dp <- max(e - u)
  Dm <- max(s)
  sq_n <- sqrt(n)
  V <- (Dp + Dm)* (sq_n + 0.155 + 0.24/sq_n)
  return(V) 
}
#Kolmogrov smirnov test
KSTest <- function(v,p=rep(1/length(v),length(v))){
  n <- sum(v)
  u <- cumsum(p)
  s <- v/n
  e <- cumsum(s)        #ecdf
  Dp <- max(e - u)
  Dm <- max(s)
  D <- max(c(Dp,Dm))
  return(D) 
}
#likelihood ratio G test
GTest <- function(v,p=rep(1/length(v),length(v))){
  e <- sum(v)*p
  r <- ifelse(v>0&e>0,log(v/e),0)
  g <- 2*sum(v*r)
  return(g)
}
#multinomial prob based on set of probabilities, defaults to equal probabilities
exactMult <- function(v,p=rep(1/length(v),length(v))){
    n <- factorial(sum(v))
    d <- prod(factorial(v))
    p <- prod(p^v)
    return( (n/d)*p )
}

# generating the data frame for the exact probabilities
# Should I export this?
exactProb <- function(n,m,p=rep(1/m,m),type="G"){
  AllDat <- t(partitions::compositions(n,m))
  ExactProb <- apply(AllDat,1,exactMult,p=p)
  if (type == "Chi"){
    Stat <- apply(AllDat,1,chiStat,p=p) }
  else if (type == "V") {
    Stat <- apply(AllDat,1,KuiperTest,p=p) }
  else if (type == "G") {
    Stat <- apply(AllDat,1,GTest,p=p) }
  else if (type == "KS") {
    Stat <- apply(AllDat,1,KSTest,p=p) }
  #order according to stat
  MyData <- data.frame(as.matrix(AllDat),ExactProb, Stat)[order(Stat),]
  #MyData$cumprob <- cumsum(MyData$ExactProb)
  return(MyData)
}



#' Small Sample Exact Test for Counts in Bins
#'
#' Small sample test statistic for counts of N items in bins with particular probability.
#'
#' @param d vector of counts, e.g. c(0,2,1,3,1,4,0) for counts of crimes in days of the week
#' @param p vector of baseline probabilities, defaults to equal probabilities in each bin
#' @param type string specifying "G" for likelihhood ratio G stat (the default), "V" for Kuipers test (for circular data), "KS" for Komolgrov-Smirnov test, and "Chi" for Chi-square test
#' @param cdf if `FALSE` (the default) generates a new permutation vector (using `exactProb`), else pass it a final probability dataset previously created
#'
#' @details
#' This construct a null distribution for small sample statistics for N counts in M bins. Example use cases are to see if a repeat offender have a proclivity to commit crimes on a particular day of the week (see the referenced paper). It can also be used for Benford's analysis of leading/trailing digits for small samples. Referenced paper shows G test tends to have the most power, although with circular data may consider Kuiper's test.
#' @returns
#' A small_sampletest object with slots for:
#'  - `CDF`, a dataframe that contains the exact probabilities and test statistic values for every possible permutation
#'  - `probabilities`, the null probabilities you specified
#'  - `data`, the observed counts you specified
#'  - `test`, the type of test conducted (e.g. G, KS, Chi, etc.)
#'  - `test_stat`, the test statistic for the observed data
#'  - `p_value`, the p-value for the observed stat based on the exact null distribution
#'  - `AggregateStatistics`, here is a reduced form aggregate table for the CDF/p-value calculation
#' 
#' If you wish to save the object, you may want to get rid of the CDF part, it can be quite large. It will have a total of `choose(n+n-1,m-1)` total rows, where m is the number of bins and n is the total counts. So if you have 10 crimes in 7 days of the week, it will result in a dataframe with `choose(7 + 10 - 1,7-1)`, which is 8008 rows.
#' Currently I keep the CDF part though to make it easier to calculate power for a particular test
#' @export
#' @examples
#' # Counts for different days of the week
#' d <- c(3,1,1,0,0,1,1) #format N observations in M bins
#' res <- small_samptest(d=d,type="G")
#' print(res)
#' 
#' # Example for Benfords analysis
#' f <- 1:9
#' p_fd <- log10(1 + (1/f)) #first digit probabilities
#' #check data from Nigrini page 84
#' checks <- c(1927.48,27902.31,86241.90,72117.46,81321.75,97473.96,
#'            93249.11,89658.17,87776.89,92105.83,79949.16,87602.93,
#'            96879.27,91806.47,84991.67,90831.83,93766.67,88338.72,
#'            94639.49,83709.28,96412.21,88432.86,71552.16)
#' # To make example run a bit faster
#' c1 <- checks[1:10]
#' #extracting the first digits
#' fd <- substr(format(c1,trim=TRUE),1,1)
#' tot <- table(factor(fd, levels=paste(f)))
#' resG <- small_samptest(d=tot,p=p_fd,type="Chi")
#' resG
#' 
#' #Can reuse the cdf table if you have the same number of observations
#' c2 <- checks[11:20]
#' fd2 <- substr(format(c2,trim=TRUE),1,1)
#' t2 <- table(factor(fd2, levels=paste(f)))
#' resG2 <- small_samptest(d=t2,p=p_fd,type="Chi",cdf=resG$CDF)
#' 
#' @references
#' Nigrini, M. J. (2012). *Benford's Law: Applications for forensic accounting, auditing, and fraud detection*. John Wiley & Sons.
#' 
#' Wheeler, A. P. (2016). Testing Serial Crime Events for Randomness in Day-of-Week Patterns with Small Samples. *Journal of Investigative Psychology and Offender Profiling*, 13(2), 148-165.
#' @seealso 
#' [powalt()] for calculating power of a test under alternative
small_samptest <- function(d,p=rep(1/length(d),length(d)),type="G",cdf=FALSE){
  n <- sum(d)
  m <- length(d)
  if (((cdf == FALSE)[1])){
      cdf <- exactProb(n=n,m=m,p=p,type=type)  #generate exact probability
  }
  if (type == "Chi"){
    Samp <- chiStat(d,p) }
  else if (type == "V") {
    Samp <- KuiperTest(d,p)  }
  else if (type == "G") {
    Samp <- GTest(d,p)  }
  else if (type == "KS") {
    Samp <- KSTest(d,p)  }  
  #p-value to the right of the test statistic, aggregating first
  Agg <- stats::aggregate(x=cdf[,'ExactProb'],by=list(cdf$Stat),FUN=sum)
  names(Agg) <- c("Stat","ExactProb")
  Agg$cumprob <- cumsum(Agg$ExactProb)
  pvalue <- sum(Agg[Agg[,'Stat'] >= Samp,'ExactProb'])
  #return object
  res <- list(cdf,p,d,type,Samp,pvalue,Agg)
  names(res) <- c("CDF","probabilities","data","test","test_stat","p_value","AggregateStatistics")
  class(res) <- "small_samptest"
  return(res)
}
methods::setClass("small_samptest",slots = c(CDF="data.frame", probabilities="numeric",data="numeric",
         test="character",test_stat="numeric",p_value="numeric",AggregateStatistics="data.frame"))

#' Power for Small Sample Exact Test
#'
#' A helper function to calculate power for different alternative distributions
#'
#' @param SST a small_samptest object created with the small_samptest function
#' @param p_alt vector of alternative probabilities to calculate power for
#' @param a scaler, alpha level for power estimate, default 0.05
#'
#' @details
#' This construct a null distribution for small sample statistics for N counts in M bins. Example use cases are to see if a repeat offender have a proclivity to commit crimes on a particular day of the week (see the referenced paper). It can also be used for Benford's analysis of leading/trailing digits for small samples.
#' @returns
#' A PowerSmallSamp object with slots for:
#'  - `permutations`, a dataframe that contains the exact probabilities and test statistic values for every possible permutation
#'  - `power`, the estimated power of the scenario
#'  - `alternative`, the alternative distribution of probabilities specified
#'  - `null`, the null distribution (taken from the SST object)
#'  - `alpha`, the specified alpha level
#' @export
#' @examples
#' @examples
#' # Counts for different days of the week
#' d <- c(3,1,2,0,0,0,0) #format N observations in M bins
#' res <- small_samptest(d=d,type="G")
#' # Power if someone only commits crime on 4 days of the week
#' alt_p <- c(1/4,1/4,1/4,1/4,0,0,0)
#' rp <- powalt(res,alt_p) #need to use previously created SST object
#' print(rp)
#' 
#' # Example for Benfords analysis
#' f <- 1:9
#' p_fd <- log10(1 + (1/f)) #first digit probabilities
#' #check data from Nigrini page 84
#' checks <- c(1927.48,27902.31,86241.90,72117.46,81321.75,97473.96,
#'            93249.11,89658.17,87776.89,92105.83,79949.16,87602.93,
#'            96879.27,91806.47,84991.67,90831.83,93766.67,88338.72,
#'            94639.49,83709.28,96412.21,88432.86,71552.16)
#' # To make example run a bit faster
#' checks <- checks[1:10]
#' # extracting the first digits
#' fd <- substr(format(checks,trim=TRUE),1,1)
#' tot <- table(factor(fd, levels=paste(f)))
#' resG <- small_samptest(d=tot,p=p_fd,type="Chi")
#' # Lets look at alt under equal probabilities (very conservative)
#' alt_equal <- rep(1/length(p_fd),length(p_fd))
#' powalt(resG,alt_equal)
#' @seealso 
#' [small_samptest()] for generating the SST object needed to estimate the power
powalt <- function(SST,p_alt,a=.05){
  x <- merge(SST$CDF,SST$`AggregateStatistics`,by="Stat")
  x$AltProb <- apply(as.matrix(x[,2:(1+length(p_alt))]),1,exactMult,p=p_alt)
  # For ties, need to get 'over' that value
  over_prob <- 1 - (x[,'cumprob'] - x[,'ExactProb.y'])
  pow <- sum(x[over_prob < a,'AltProb']) #power of alt
  r <- list(x,pow,p_alt,SST$probabilities,a,SST$test)
  names(r) <- c("permutations","pow","alt","null","alpha","test")
  names(r$permutations)[1] <- names(SST[4])
  class(r) <- "powalt"
  return(r)
}
methods::setClass("powalt",slots = c(permutations="data.frame", pow="numeric", alt="numeric",
                  null="numeric",alpha="numeric",test="character"))

#' @method print small_samptest
#' @export
print.small_samptest <- function(x, ...){
  cat("\n\tSmall Sample Test Object \n")
  cat(paste0("\tTest Type is ",x$test," \n"))
  cat(paste0("\tStatistic is: ",x$test_stat," \n"))
  cat("\tp-value is: ",x$'p_value'," \n")
  cat("\tData are: ",paste(x$data),"\n")
  cat("\tNull probabilities are: ",formatC(x$probabilities,digits=2),"\n")
  cat("\tTotal permutations are: ",length(x$CDF[,1]),"\n\n")
}
#methods::setMethod("print","small_samptest",print.small_samptest)

#' @method print powalt
#' @export
print.powalt <- function(x, ...){
  cat("\n\tPower for Small Sample Test \n")
  cat("\tTest type is:",x$test," \n")
  cat("\tPower is:",x$pow," \n")
  cat("\tNull is:",formatC(x$null,digits=2),"\n")
  cat("\tAlt is:",formatC(x$alt,digits=2),"\n")
  cat("\tAlpha is:",x$alpha," \n")
  b <- length(names(x$permutations))-5
  cat("\tNumber of Bins:",b," \n")
  s <- sum(x$permutations[1,2:(1+b)])
  cat("\tNumber of Observations:",s,"\n\n")
}
#methods::setMethod("print","powalt",print.powalt)