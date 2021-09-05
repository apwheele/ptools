#' Checks the fit of a Poisson Distribution
#'
#' Provides contours (for use in graphs) to show changes in Poisson counts in a pre vs post period.
#' 
#' @param pre_crime, vector of crime counts in the pre period
#' @param post_crime, vector of crime counts in the post period 
#' @param lev, vector of Poisson Z-scores to draw the contours at, defaults to `c(-3,0,3)`
#' @param lr, scaler lower limit for where to draw the contour lines, defaults to `5`
#' @param hr, scaler upper limit for where to draw the contour lines, defaults to `max(pre_crime)*1.05`
#' @param steps, scaler how dense to make the lines, defaults to 1000 steps
#'
#' @details Provides a set of contour lines to show whether increases/decreases in Poisson counts between two periods
#' are outside of those expected by chance according to the Poisson distribution based on the normal approximation.
#' Meant to be used in subsequent graphs. Note the approximation breaks down at smaller N values, so below 5 is not
#' typically recommended. 
#' @returns
#' A dataframe with columns
#'  - `x`, the integer value
#'  - `y`, the y-value in the graph for expected changes (will not be below 0)
#'  - `levels`, the associated Z-score level
#' @export
#' @examples
#' # Example use with NYC Shooting Data pre/post Covid lockdowns
#' # Prepping the NYC shooting data
#' data(nyc_shoot)
#' begin_date <- as.Date('03/01/2020', format="%m/%d/%Y")
#' nyc_shoot$Pre <- ifelse(nyc_shoot$OCCUR_DATE < begin_date,1,0)
#' nyc_shoot$Post <- nyc_shoot$Pre*-1 + 1
#' # Note being lazy, some of these PCTs have changed over time
#' pct_tot <- aggregate(cbind(Pre,Post) ~ PRECINCT, data=nyc_shoot@data, FUN=sum)
#' cont_lines <- pois_contour(pct_tot$Pre,pct_tot$Post)
#' # Now making an ugly graph
#' sp <- split(cont_lines,cont_lines$levels)
#' plot(pct_tot$Pre,pct_tot$Post)
#' for (s in sp){
#'   lines(s$x,s$y,lty=2)
#' }
#' # Can see it is slightly overdispersed, but pretty close!
#' # See https://andrewpwheeler.com/2021/02/02/the-spatial-dispersion-of-nyc-shootings-in-2020/
#' # For a nicer example using ggplot
#' 
#' @references
#' Drake, G., Wheeler, A., Kim, D.-Y., Phillips, S. W., & Mendolera, K. (2021). *The Impact of COVID-19 on the Spatial Distribution of Shooting Violence in Buffalo, NY*. CrimRxiv. https://doi.org/10.21428/cb6ab371.e187aede
pois_contour <- function(pre_crime,post_crime,lev=c(-3,0,3),
                         lr=5,hr=max(pre_crime)*1.05,steps=1000){
    #calculating the overall crime increase
    ov_inc <- sum(post_crime)/sum(pre_crime)
    #Making the sequence on the square root scale
    gr <- seq(sqrt(lr),sqrt(hr),length.out=steps)^2
    cont_data <- expand.grid(gr,lev)
    names(cont_data) <- c('x','levels')
    inc <- cont_data$x*ov_inc
    vari <- cont_data$x * ov_inc^2
    cont_data$y <- inc + cont_data$levels*sqrt(vari)
    cont_df <- as.data.frame(cont_data)
    # Should never be below 0
    cont_df$y <- ifelse(cont_data$y < 0,0,cont_data$y)
    return(cont_df[,c("x","y","levels")])
}