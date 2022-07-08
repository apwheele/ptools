#' Estimates the WDD Test
#'
#' Estimates the weighted displacement difference test from [Wheeler & Ratcliffe, *A simple weighted displacement difference test to evaluate place based crime interventions*, Crime Science](https://crimesciencejournal.biomedcentral.com/articles/10.1186/s40163-018-0085-5)
#'
#' @param control vector with counts in pre,post for control area
#' @param treated vector with counts in pre,post for treated area
#' @param disp_control vector with counts in pre,post for displacement control area (default `c(0,0)`)
#' @param disp_treated vector with counts in pre,post for displacement treated area (default `c(0,0)`)
#' @param time_weights vector with weights for time periods for pre/post (default `c(1,1)`)
#' @param area_weights vector with weights for different sized areas, order is c(control,treated,disp_control,disp_treated) (default `c(1,1,1,1)`)
#' @param alpha scaler alpha level for confidence interval (default `0.1`)
#' @param silent boolean set to TRUE if you do not want anything printed out (default FALSE)
#'
#' @details The wdd (weighted displacement difference) test is an extensions to differences-in-differences when observed count data pre/post in treated control areas. The test statistic (ignoring displacement areas and weights) is:
#' 
#' \deqn{WDD = \Delta T - \Delta Ct}{WDD = \Delta T - \Delta Ct}
#' where \eqn{\Delta T = T_1 - T_0}{\Delta T = T1 - T0}, the post time period count minus the pre time period count for the treated areas. And ditto for the control areas, Ct. The variance is calculated as:
#'
#' \deqn{T_1 + T_0 + Ct_1 + Ct_0}{T1 + T0 + Ct1 + Ct0}
#'
#' that is this test uses the normal approximation to the Poisson distribution to calculate the standard error for the WDD. So beware if using very tiny counts -- this approximation is less likely to be applicable (or count data that is Poisson, e.g. very overdispersed). 
#'
#' This function also incorporates weights for different examples, such as differing [pre/post time periods](https://andrewpwheeler.com/2021/01/09/the-wdd-test-with-different-pre-post-time-periods/) (e.g. 2 years in pre and 1 year in post), or [different area sizes](https://andrewpwheeler.com/2021/02/23/the-wdd-test-with-different-area-sizes/) (e.g. a one square mile area vs a two square mile area). The subsequent test statistic can then be interpreted as changes per unit time or changes per unit area (e.g. density) or both per time and density.
#' @returns
#' A length 9 vector with names: 
#'  - `Est_Local` and `SE_Local`, the WDD and its standard error for the local estimates
#'  - `Est_Displace` and `SE_Displace`, the WDD and its standard error for the displacement areas
#'  - `Est_Total` and `SE_Total`, the WDD and its standard error for the combined local/displacement areas
#'  - `Z`, the Z-score for the total estimate
#'  - and the lower and upper confidence intervals, `LowCI` and `HighCI`, for whatever alpha level you specified for the total estimate.
#' @export
#' @examples
#' # No weights and no displacement
#' cont <- c(20,20); treat <- c(20,10)
#' wdd(cont,treat)
#'
#' # With Displacement stats
#' disptreat <- c(30,20); dispcont <- c(30,30)
#' wdd(cont,treat,dispcont,disptreat)
#'
#' # With different time periods for pre/post
#' wdd(cont,treat,time_weights=c(2,1))
#'
#' # With different area sizes
#' wdd(cont,treat,dispcont,disptreat,area_weights=c(2,1.5,3,3.2))
#' 
#' # You can technically use this even without pre (so just normal based approximation)
#' # just put in 0's for the pre data (so does not factor into variance)
#' res_test <- wdd(c(0,20),c(0,10))
#' twotail_p <- pnorm(res_test['Z'])*2
#' print(twotail_p) #~0.068
#' # e-test is very similar
#' e_test(20,10) #~0.069
#' @references
#' Wheeler, A. P., & Ratcliffe, J. H. (2018). A simple weighted displacement difference test to evaluate place based crime interventions. *Crime Science*, 7(1), 1-9.
#' @seealso 
#' [wdd_harm()] for aggregating multiple WDD tests into one metric (e.g. based on crime harm weights)
#' [e_test()] for checking the difference in two Poisson means

wdd <- function(control,treated,disp_control=c(0,0),
                disp_treated=c(0,0),
                time_weights=c(1,1),area_weights=c(1,1,1,1),
                alpha=0.1, silent=FALSE){
    # Generating the weights
    cpre_w  <- time_weights[1]*area_weights[1]
    cpost_w <- time_weights[2]*area_weights[1]
    tpre_w  <- time_weights[1]*area_weights[2]
    tpost_w  <- time_weights[2]*area_weights[2]
    dcpre_w <- time_weights[1]*area_weights[3]
    dcpos_w <- time_weights[2]*area_weights[3]
    dpre_w <-  time_weights[1]*area_weights[4]
    dpost_w <- time_weights[2]*area_weights[4]
    # Generating the stats
    est_local <- (treated[2]/tpost_w - treated[1]/tpre_w) - (control[2]/cpost_w - control[1]/cpre_w)
    var_local <- treated[2]*((1/tpost_w)^2) + treated[1]*((1/tpre_w)^2) + control[2]*((1/cpost_w)^2) + control[1]*((1/cpre_w)^2)
    est_disp <- (disp_treated[2]/dpost_w - disp_treated[1]/dpre_w) - (disp_control[2]/dcpos_w - disp_control[1]/dcpre_w)
    var_disp <- disp_treated[2]*((1/dpost_w)^2) + disp_treated[1]*((1/dpre_w)^2) + disp_control[2]*((1/dcpos_w)^2) + disp_control[1]*((1/dcpre_w)^2)
    tot_est <- est_local + est_disp
    var_tot <- var_local + var_disp
    # Inference stats
    level <- stats::qnorm(1 - alpha/2)
    z_score <- tot_est/sqrt(var_tot)
    low_ci  <- tot_est - level*sqrt(var_tot)
    high_ci <- tot_est + level*sqrt(var_tot)
    # Printing results
    if (!silent){
        cat(paste0('\n\tThe local WDD estimate is ',round(est_local,1),' (',round(sqrt(var_local),1),')'))
        cat(paste0('\n\tThe displacement WDD estimate is ',round(est_disp,1),' (',round(sqrt(var_disp),1),')'))
        cat(paste0('\n\tThe total WDD estimate is ',round(tot_est,1),' (',round(sqrt(var_tot),1),')'))
        cat(paste0('\n\tThe ',round(100*(1-alpha)),'% confidence interval is ',round(low_ci,1),' to ',round(high_ci,1),'\n\n'))
    }
    res <- c(est_local,sqrt(var_local),est_disp,sqrt(var_disp),tot_est,sqrt(var_tot),z_score,low_ci,high_ci)
    names(res) <- c('Est_Local','SE_Local','Est_Displace','SE_Displace','Est_Total','SE_Total','Z','LowCI','HighCI')
    return(res)
}