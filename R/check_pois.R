#' Checks the fit of a Poisson Distribution
#'
#' Provides a frequency table to check the fit of a Poisson distribution to empirical data.
#'
#' @param counts vector of counts, e.g. c(0,5,1,3,4,6)
#' @param min_val scaler minimum value to generate the grid of results, e.g. `0`
#' @param max_val scaler maximum value to generate the grid of results, e.g. `max(counts)`
#' @param pred can either be a scaler, e.g. `mean(counts)`, or a vector (e.g. predicted values from a Poisson regression)
#' @param silent boolean, do not print mean/var stat messages, only applies when passing scaler for pred (default `FALSE`)
#'
#' @details Given either a scaler mean to test the fit, or a set of predictions (e.g. varying means predicted from a model), checks whether the data fits a given Poisson distribution over a specified set of integers. That is it builds a table of integer counts, and calculates the observed vs the expected distribution according to Poisson. Useful for checking any obvious deviations.
#' @returns
#' A dataframe with columns
#'  - `Int`, the integer value
#'  - `Freq`, the total observed counts within that Integer value
#'  - `PoisF`, the expected counts according to a Poisson distribution with mean/pred specified
#'  - `ResidF`, the residual from `Freq - PoisF`
#'  - `Prop`, the observed proportion of that integer (0-100 scale)
#'  - `PoisD`, the expected proportion of that integer (0-100 scale)
#'  - `ResidD`, the residual from `Prop - PoisD`
#' @export
#' @examples
#' # Example use for constant over the whole sample
#' set.seed(10)
#' lambda <- 0.2
#' x <- rpois(10000,lambda)
#' pfit <- check_pois(x,0,max(x),mean(x))
#' print(pfit)
#' # 82% zeroes is not zero inflated -- expected according to Poisson!
#'
#' # Example use if you have varying predictions, eg after Poisson regression
#' n <- 10000
#' ru <- runif(n,0,10)
#' x <- rpois(n,lambda=ru)
#' check_pois(x, 0, 23, ru)
#' 
#' # If you really want to do a statistical test of fit
#' chi_stat <- sum((pfit$Freq - pfit$PoisF)^2/pfit$PoisF)
#' df <- length(pfit$Freq) - 2
#' stats::dchisq(chi_stat, df) #p-value
#' # I prefer evaluating specific integers though (e.g. zero-inflated, longer-tails, etc.)
#' 
#' # If you want an example with real data, see the WaPo fatal officer involved shootings
#' w1 <- 'https://raw.githubusercontent.com/washingtonpost/' #too long url!
#' w2 <- 'data-police-shootings/master/fatal-police-shootings-data.csv'
#' wapo_url <- paste0(w1,w2)
#' oid <- read.csv(wapo_url, stringsAsFactors = FALSE)
#' # Now aggregating to count per day
#' oid$date_val <- as.Date(oid$date)
#' #may be biased low if several recent days with 0
#' date_range <- paste0(seq(as.Date('2015-01-01'),max(oid$date_val),by='days'))
#' day_counts <- as.data.frame(table(factor(oid$date,levels=date_range)))
#' check_pois(day_counts$Freq, 0, max(day_counts$Freq)+1, mean(day_counts$Freq))
#'
#' # Example with varying predictions from a model
#' day_counts$wd <- weekdays(as.Date(day_counts$Var1))
#' mod <- stats::glm(Freq ~ as.factor(wd) - 1, family='poisson', data=day_counts)
#' lin_pred <- exp(predict(mod))
#' pfit_wd <- check_pois(day_counts$Freq, 0, 11, lin_pred)
#' print(pfit_wd)

check_pois <- function(counts,min_val,max_val,pred,silent=FALSE){
   freqN <- as.data.frame(table(factor(counts,levels=min_val:max_val)))
   mu <- pred #mean(counts)
   if(length(mu) == 1){
       if (!silent){
           cat( paste0('\n\tmean: ', mean(counts)) )
           cat( paste0('\tvariance: ',stats::var(counts),'\n') )
       }
       PoisD <- stats::dpois(min_val:max_val,mu)
   }
   else{
      PoisD <- min_val:max_val
      n <- length(counts)
      for (i in 1:length(PoisD)){
          PoisD[i] <- sum(stats::dpois(PoisD[i],mu))/n
      }
   } 
   freqN$PoisF <- PoisD*length(counts)
   freqN$ResidF <- (freqN$Freq  - freqN$PoisF)
   freqN$Prop <- (freqN$Freq/sum(freqN$Freq))*100
   freqN$PoisD <- PoisD*100
   freqN$ResidD <- (freqN$Prop - freqN$PoisD)
   freqN$Var1 <- as.numeric(as.character(freqN$Var1))
   names(freqN)[1] <- 'Int'
   return(freqN)
}

######################
#ToDo, this for negative binomial fit
######################