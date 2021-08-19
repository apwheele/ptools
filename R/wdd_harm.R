#' Combines Multiple WDD Tests
#'
#' Combines multiple weighted displacement difference tests into one final weighted harm metric. 
#'
#' @param est vector with WDD estimates (e.g. difference in crime counts for treated vs controls)
#' @param se vector with standard errors for WDD estimates
#' @param weight vector with weights to aggregate results
#' @param alpha scaler alpha level for confidence interval (default `0.1`)
#'
#' @details This test combines multiple wdd estimates with different weights. Created to [combine tests for crime harm weights](https://andrewpwheeler.com/2020/11/19/amending-the-wdd-test-to-incorporate-harm-weights/).
#' @returns
#' A length 5 vector with names: 
#'  - `HarmEst`, the combined harm estimate
#'  - `SE_HarmEst` its standard error
#'  - `Z`, the Z-score
#'  - and the lower and upper confidence intervals, `LowCI` and `HighCI`, for whatever alpha level you specified.
#' @export
#' @examples
#' # Creating wdd tests for three different crimes and combining
#' rob <- wdd(c(20,20),c(20,10))
#' burg <- wdd(c(30,30),c(25,20))
#' theft <- wdd(c(80,60),c(70,20))
#' dat = data.frame(rbind(rob,burg,theft))
#' # passing those columns now to the wdd_harm function
#' harm_weights <- c(10,5,1)
#' wdd_harm(dat$Est_Local,dat$SE_Local,harm_weights)
#' @seealso [wdd()] for estimating the individual wdd outcomes
#\code{\link{wdd}}

wdd_harm <- function(est,se,weight,alpha=0.1){
    # Harm estimates 
    harm_est <- est*weight
    harm_var <- (se^2)*(weight^2)
    tot_harm <- sum(harm_est)
    tot_harm_var <- sum(harm_var)
    tot_harm_se <- sqrt(tot_harm_var)
    # Inference stats
    level <- stats::qnorm(1 - alpha/2)
    z_score <- tot_harm/tot_harm_se
    low_ci <-  tot_harm - level*tot_harm_se
    high_ci <- tot_harm + level*tot_harm_se
    print(paste0('The total WDD harm estimate is ',round(tot_harm,1),' (',round(tot_harm_se,1),')'))
    print(paste0('The ',round(100*(1-alpha)),'% confidence interval is ',round(low_ci,1),' to ',round(high_ci,1)))
    res <- c(tot_harm,tot_harm_se,z_score,low_ci,high_ci)
    names(res) <- c('HarmEst','SE_HarmEst','Z','LowCI','HighCI')
    return(res)
}