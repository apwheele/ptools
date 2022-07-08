#' Predictive Accuracy Index
#'
#' Given a set of predictions and observed counts, returns the PAI (predictive accuracy index),
#' PEI (predictive efficiency index), and the RRI (recovery rate index)
#'
#' @param dat data frame with the predictions, observed counts, and area sizes (can be a vector of ones)
#' @param count character specifying the column name for the observed counts (e.g. the out of sample crime counts)
#' @param pred character specifying the column name for the predicted counts (e.g. predictions based on a model)
#' @param area character specifying the column name for the area sizes (could also be street segment distances, see Drawve & Wooditch, 2019)
#' @param other vector of strings for any other column name you want to keep (e.g. an ID variable), defaults to empty `c()`
#'
#' @details
#' Given predictions over an entire sample, this returns a dataframe with the sorted best PAI (sorted by density of predicted counts per area). 
#' PAI is defined as:
#' 
#' \deqn{PAI = \frac{c_t/C}{a_t/A}}{PAI = (ct/C)/(at/A)}
#' 
#' Where the numerator is the percent of crimes in cumulative t areas, and the denominator is the percent of the area encompassed.
#' PEI is the observed PAI divided by the best possible PAI if you were a perfect oracle, so is scaled between 0 and 1. 
#' RRI is `predicted/observed`, so if you have very bad predictions can return Inf or undefined!
#' See Wheeler & Steenbeek (2019) for the definitions of the different metrics. 
#' User note, PEI may behave funny with different sized areas.
#'
#' @returns
#' A dataframe with the columns:
#'  - `Order`, The order of the resulting rankings
#'  - `Count`, the counts for the original crimes you specified
#'  - `Pred`, the original predictions 
#'  - `Area`, the area for the units of analysis
#'  - `Cum*`, the cumulative totals for Count/Pred/Area
#'  - `PCum*`, the proportion cumulative totals, e.g. `CumCount/sum(Count)`
#'  - `PAI`, the PAI stat
#'  - `PEI`, the PEI stat
#'  - `RRI`, the RRI stat (probably should analyze/graph the `log(RRI)`)!
#'
#' Plus any additional variables specified by `other` at the end of the dataframe.
#' @export
#' @examples
#' 
#' # Making some very simple fake data
#' crime_dat <- data.frame(id=1:6,
#'                         obs=c(6,7,3,2,1,0),
#'                         pred=c(8,4,4,2,1,0))
#' crime_dat$const <- 1
#' p1 <- pai(crime_dat,'obs','pred','const')
#' print(p1)
#' 
#' # Combining multiple predictions, making
#' # A nice table
#' crime_dat$rand <- sample(crime_dat$obs,nrow(crime_dat),FALSE)
#' p2 <- pai(crime_dat,'obs','rand','const')
#' pai_summary(list(p1,p2),c(1,3,5),c('one','two'))
#' 
#' @references
#' Drawve, G., & Wooditch, A. (2019). A research note on the methodological and theoretical considerations for assessing crime forecasting accuracy with the predictive accuracy index. *Journal of Criminal Justice*, 64, 101625.
#' 
#' Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. *Journal of Quantitative Criminology*, 37(2), 445-480.
#' @seealso 
#' [pai_summary()] for a summary table of metrics for multiple pai tables given fixed N thresholds
pai <- function(dat,count,pred,area,other=c()){
    LimData <- dat[,c(count,pred,area,other)]
    p <- LimData[,pred]/LimData[,area] #sorts by the highest density
    cr <- LimData[,count]
    LimData <- LimData[order(p,-1*cr, decreasing = TRUE),] #sorting asc for cr gives min potential PAI
    LimData$Order <- 1:nrow(LimData)
    LimData$Count <- LimData[,count]
    LimData$CumCount <- cumsum(LimData[,count])
    tot_area <- sum(LimData[,area])
    tot_crime <- sum(LimData[,count])
    LimData$Area <- LimData[,area]
    LimData$CumArea <- cumsum(LimData$Area)
    LimData$PCumArea <- LimData$CumArea/tot_area
    LimData$PCumCount <- (LimData$CumCount/tot_crime)
    LimData$PAI <- LimData$PCumCount / LimData$PCumArea
    # Calculating PEI, note may not be apples to apples with diffe
    cr_dens <- LimData$Count/LimData$Area
    pei_dat <- LimData[order(cr_dens, decreasing = TRUE),]
    cum_perfect <- cumsum(pei_dat$Count)
    pcum_perfect <- cum_perfect/tot_crime
    pcum_area <- cumsum(pei_dat$Area)/tot_area
    perf_pai <- pcum_perfect/pcum_area
    LimData$PEI <- LimData$PAI/perf_pai
    # Calculating RRI
    LimData$Pred <- LimData[,pred]
    LimData$CumPred <- cumsum(LimData[,pred])
    LimData$RRI <- LimData$CumPred/LimData$CumCount #?Handle 0's?
    # Variables I want
    keep_vars <- c('Order','Count','CumCount','PCumCount','Pred','CumPred',
                   'Area','CumArea','PCumArea','PAI','PEI','RRI',other)
    return( LimData[,keep_vars] )
}

#' Summary Table for Multiple PAI Stats
#'
#' Takes a list of multiple PAI summary tables (for different predictions) and returns summaries at fixed area thresholds
#'
#' @param pai_list, list of data frames that have the PAI stats from the `pai` function
#' @param thresh, vector of area numbers to select, e.g. 10 would select the top 10 areas, c(10,100) would select the top 10 and the top 100 areas
#' @param labs vector of characters that specifies the labels for each PAI dataframe, should be the same length as `pai_list`
#' @param wide boolean, if TRUE (default), returns data frame in wide format. Else returns summaries in long format
#'
#' @details
#' Given predictions over an entire sample, this returns a dataframe with the sorted best PAI (sorted by density of predicted counts per area). 
#' PAI is defined as:
#' 
#' \deqn{PAI = \frac{c_t/C}{a_t/A}}{PAI = (ct/C)/(at/A)}
#' 
#' Where the numerator is the percent of crimes in cumulative t areas, and the denominator is the percent of the area encompassed.
#' PEI is the observed PAI divided by the best possible PAI if you were a perfect oracle, so is scaled between 0 and 1. 
#' RRI is `predicted/observed`, so if you have very bad predictions can return Inf or undefined!
#' See Wheeler & Steenbeek (2019) for the definitions of the different metrics. 
#' User note, PEI may behave funny with different sized areas.
#'
#' @returns
#' A dataframe with the PAI/PEI/RRI, and cumulative crime/predicted counts, for each original table
#'
#' @export
#' @examples
#' 
#' # Making some very simple fake data
#' crime_dat <- data.frame(id=1:6,
#'                         obs=c(6,7,3,2,1,0),
#'                         pred=c(8,4,4,2,1,0))
#' crime_dat$const <- 1
#' p1 <- pai(crime_dat,'obs','pred','const')
#' print(p1)
#' 
#' # Combining multiple predictions, making
#' # A nice table
#' crime_dat$rand <- sample(crime_dat$obs,nrow(crime_dat),FALSE)
#' p2 <- pai(crime_dat,'obs','rand','const')
#' pai_summary(list(p1,p2),c(1,3,5),c('one','two'))
#' 
#' @references
#' Drawve, G., & Wooditch, A. (2019). A research note on the methodological and theoretical considerations for assessing crime forecasting accuracy with the predictive accuracy index. *Journal of Criminal Justice*, 64, 101625.
#' 
#' Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. *Journal of Quantitative Criminology*, 37(2), 445-480.
#' @seealso 
#' [pai()] for a summary table of metrics for multiple pai tables given fixed N thresholds
pai_summary <- function(pai_list,thresh,labs,wide=TRUE){
    keep_vars <- c('lab','Order','CumCount','CumPred','PAI','PEI','RRI')
    names(pai_list) <- labs
    combo <- do.call(rbind,pai_list)
    combo$lab <- do.call(rbind,strsplit(row.names(combo),"[.]"))[,1]
    combo_sub <- combo[combo$Order %in% thresh,keep_vars]
    # If wide return in wide format
    if (wide){
        cs <- stats::reshape(combo_sub,idvar='Order',timevar = 'lab',direction='wide')
    }
    else { cs <- combo_sub } #Else return in long format
    row.names(cs) <- 1:nrow(cs)
    return(cs)
}

# ToDo, thesh could be PCumArea instead or Order