#docstrings!!!

# Increase contours, see https://andrewpwheeler.com/2020/02/21/some-additional-plots-to-go-with-crime-increase-dispersion/
pois_contour <- function(pre_crime,post_crime,levels=c(-3,0,3),
                         lr=5,hr=max(pre_crime)*1.05,steps=1000){
    #calculating the overall crime increase
    ov_inc <- sum(post_crime)/sum(pre_crime)
    #Making the sequence on the square root scale
    gr <- seq(sqrt(lr),sqrt(hr),length.out=steps)^2
    cont_data <- expand.grid(gr,levels)
    names(cont_data) <- c('x','levels')
    cont_data$inc <- cont_data$x*ov_inc
    vari <- cont_data$x * ov_inc^2
    cont_data$lines <- cont_data$inc + cont_data$levels*sqrt(vari)
    cont_df <- as.data.frame(cont_data)
    cont_df$lines <- ifelse(cont_data$lines < 0,0,cont_data$lines)
    return(cont_df)
}


#https://data.cityofnewyork.us/Business/Sidewalk-Caf-Licenses-and-Applications/qcdj-rwhu