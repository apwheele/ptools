#' Creates vector grid cells over study area
#'
#' Creates grid cells of given size over particular study area.
#'
#' @param outline SpatialPolygon or SpatialPolygonDataFrame that defines the area to draw grid cells over
#' @param size scaler for the size of the grid cells (one side), in whatever units the outline is in
#' @param clip_level , you can clip grid cells if they are not entirely inside the outlined area, defaults to `0`
#' so any cells at least touching are included
#' @param point_over default `NULL`, but can pass in SpatialPoints and will only include grid cells that have at least one point
#' @param point_n default 0, only used if passing in `point_over`. Will return only grid cells with greater than `point_n` points
#'
#' @details This generates a vector grid over the study area of interest. Intentionally working with vector data for use with 
#' other feature engineering helper functions (that can pass in X/Y). 
#' @returns
#' A SpatialPolygonDataFrame object with columns
#'  - `id`, integer id value (not the same as row.names!)
#'  - `x`, x centroid of grid cell
#'  - `y`, y centroid of grid cell
#'  - `cover`, proportion that grid cell is covered by `outline`
#'  - `count`, optional (only if you pass in `point_over`)
#' @export
#' @examples
#' library(sp) #for sp plot methods
#' # large grid cells
#' data(nyc_bor)
#' res <- prep_grid(nyc_bor,5000)
#' plot(nyc_bor)
#' plot(res,border='BLUE',add=TRUE)
#' 
#' # clipping so majority of grid is inside outline
#' res <- prep_grid(nyc_bor,2000,clip_level=0.5)
#' plot(nyc_bor)
#' plot(res,border='BLUE',add=TRUE)
#' 
#' # only grid cells that have at least one shooting
#' data(nyc_shoot)
#' res <- prep_grid(nyc_bor,2000,clip_level=0,nyc_shoot)
#' plot(nyc_bor)
#' plot(res,border='RED',add=TRUE)
#'
#' @references
#' Wheeler, A. P. (2018). The effect of 311 calls for service on crime in DC at microplaces. *Crime & Delinquency*, 64(14), 1882-1903.
#' 
#' Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. *Journal of Quantitative Criminology*, 37(2), 445-480.
prep_grid <- function(outline, size, clip_level=0, point_over=NULL, point_n=0){
    # Creating the initial raster full grid
    base_raster <- raster::raster(ext=raster::extent(outline), res=size)
    raster::projection(base_raster) <- raster::crs(outline)
    # Getting grid cells inside out the outline
    mask_raster <- raster::rasterize(outline, base_raster, getCover=TRUE)
    base_poly <- raster::rasterToPolygons(mask_raster,dissolve=FALSE)
    sel_poly <- base_poly[base_poly$layer > clip_level,]
    # Adding in XY coordinates
    xy_df <- sp::coordinates(sel_poly)
    sel_poly$x <- xy_df[,1]
    sel_poly$y <- xy_df[,2]
    sel_poly$id <- 1:nrow(sel_poly)
    sel_poly <- sel_poly[,c('id','x','y','layer')]
    names(sel_poly)[4] <- 'cover'
    # If you pass in spatial points, also extract those covered
    # By at least one point (and get counts of points)
    if (!is.null(point_over)){
        ch <- sp::over(sel_poly,point_over[,1],fn=length)
        names(ch) <- 'count'
        ch[is.na(ch)] <- 0
        sel_poly$count <- ch
        # selecing out minimal point number
        sel_poly <- sel_poly[c(sel_poly$count > point_n),]
        sel_poly$id <- 1:nrow(sel_poly)
        names(sel_poly)[5] <- 'count'
    }
    return(sel_poly)
}


#' Distance to nearest based on centroid
#'
#' Given a base X/Y dataset, calculates distance to nearest for another feature X/Y dataset
#'
#' @param base base dataset (eg gridcells)
#' @param feat feature dataset (eg another crime generator)
#' @param bxy vector of strings that define what the base xy fields are defined as, defaults `c('x','y')`
#' @param fxy vector of strings that define what the base xy fields are defined as, defaults `c('x','y')`
#'
#' @details This generates a distance to nearest, based on the provided x/y coordinates (so if using polygons pass the centroid).
#' This uses kd-trees from RANN, so should be reasonably fast. But I do no projection checking, that is on you. You should not
#' use this with spherical coordinates. Useful for feature engineering for crime generators.
#' @returns
#' A vector of distances from base dataset xy to the nearest feature xy
#' @export
#' @examples
#' data(nyc_bor); data(nyc_cafe)
#' gr_nyc <- prep_grid(nyc_bor,3000,clip_level=0.3)
#' gr_nyc$dist_cafe <- dist_xy(gr_nyc,nyc_cafe)
#' head(gr_nyc)
#' sp::spplot(gr_nyc,zcol='dist_cafe')
#' @seealso 
#' [count_xy()] for counting points inside of base polygon
#'
#' [dcount_xy()] for counting points within distance of base polygon
#'
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#'
#' [bisq_xy()] for estimate bi-square kernel of points for features at base polygon xy coords
#'
#' [idw_xy()] for estimate inverese distance weighted of points for features at base polygon xy coords 
#' 
#' @references
#' Caplan, J. M., Kennedy, L. W., & Miller, J. (2011). Risk terrain modeling: Brokering criminological theory and GIS methods for crime forecasting. *Justice Quarterly*, 28(2), 360-381.
#'
#' Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. *Journal of Quantitative Criminology*, 37(2), 445-480.
#'
dist_xy <- function(base,feat,bxy=c('x','y'),fxy=c('x','y')){
    # I do not check to make sure projection is same
    # To allow passing in data frames
    bclass <- substring(class(base)[1],1,2)
    fclass <- substring(class(feat)[1],1,2)
    if (bclass == 'Sp'){ bdat <- base@data[,bxy]
    } else { bdat <- base[,bxy] }
    if (fclass == 'Sp'){ fdat <- feat@data[,fxy]
    } else { fdat <- feat[,bxy] }
    resnn <- RANN::nn2(data=fdat,query=bdat,k=1)
    return(c(resnn$nn.dists))
}


#' Count of points in polygon
#'
#' Given a base X/Y dataset, calculates number of feature points that fall inside
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param weight if 1 (default), does not use weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates a count (or weighted count) of features inside of the base areas. Both should be projected in the same units.
#' Uses `sp::over()` methods in the function.
#' @returns
#' A vector of counts (or weighted sums)
#' @export
#' @examples
#' data(nyc_liq); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,3000)
#' gr_nyc$liq_cnt <- count_xy(gr_nyc,nyc_liq)
#' gr_nyc$table_cnt <- count_xy(gr_nyc,nyc_cafe,'SWC_TABLES')
#' head(gr_nyc)
#' sp::spplot(gr_nyc,zcol='liq_cnt')
#' 
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [dcount_xy()] for counting points within distance of base polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords
#' 
#' @references
#' Wheeler, A. P. (2019). Quantifying the local and spatial effects of alcohol outlets on crime. *Crime & Delinquency*, 65(6), 845-871.
#' 
count_xy <- function(base,feat,weight=1){
    if (weight == 1){
        ch <- sp::over(base,feat[,1],fn=length)
    } else {
        ch <- sp::over(base,feat[,weight],fn=sum)
    }
    ch[is.na(ch)] <- 0
    return( ch[,1])
}


#' Count of points within distance of polygon
#'
#' Given a base X/Y dataset, calculates number of feature points that are within particular distance
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param d scaler distance to count (based on polygon boundary for base, not centroid)
#' @param weight if 1 (default), does not use weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates a count (or weighted count) of features within specified distance of the `base` *polygon* border. 
#' Both should be projected in the same units. Uses `raster::buffer()` on `feat` dataset (which calls `rgeos`) and `sp::over` functions.
#' @returns
#' A vector of counts (or weighted sums)
#' @export
#' @examples
#' data(nyc_cafe); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,3000)
#' gr_nyc$dcafe_5k <- dcount_xy(gr_nyc,nyc_cafe,5000)
#' #gr_nyc$dtabl_5k <- dcount_xy(gr_nyc,nyc_cafe,5000,'SWC_TABLES')
#' head(gr_nyc)
#' sp::spplot(gr_nyc,zcol='dcafe_5k') #total tables within 5k feet of grid cell
#' 
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [count_xy()] for counting points inside polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords
#'
#' @references
#' Groff, E. R. (2014). Quantifying the exposure of street segments to drinking places nearby. *Journal of Quantitative Criminology*, 30(3), 527-548.
#' 
dcount_xy <- function(base,feat,d,weight=1){
   # Calculate buffers
   buff <- raster::buffer(feat,d,dissolve=FALSE)
   # Use over to count or sum
   if (weight == 1){
       ov_buff <- sp::over(base,buff[,1],fn=length)
   } else {
       ov_buff <- sp::over(base,buff[,weight],fn=sum)
   }
   ov_buff[is.na(ov_buff)] <- 0
   return(as.numeric(ov_buff[,1]))
}

# Gaussian Kernel density, no norm
kern_fun <- function(d,b,w=1){
    rd <- stats::dnorm(d,0,b)
    if (w == 1){
        return( sum(rd) )
    }
    else { 
        return( sum(rd*w) )
    }
}

#' Kernel density of nearby areas 
#'
#' Given a base X/Y dataset, calculates guassian kernel density for nearby points in feat dataset
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame or SpatialPointsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param bandwidth scaler bandwidth for the normal KDE
#' @param weight if 1 (default), does not use weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates a density of nearby features at particular control points (specified by `base`). Useful for risk terrain
#' style feature engineering given nearby crime generators. Loops through all pairwise distances (and uses `dnorm()`). So will be slow
#' for large base + feature datasets (although should be OK memory wise). Consider aggregating/weighting data if `feat` is very large.
#' @returns
#' A vector of densities (or weighted densities)
#' @export
#' @examples
#' data(nyc_cafe); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,3000)
#' gr_nyc$kdecafe_5k <- kern_xy(gr_nyc,nyc_cafe,5000)
#' head(gr_nyc)
#' sp::spplot(gr_nyc,zcol='kdecafe_5k')
#' 
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [count_xy()] for counting points inside polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords
#' 
#' @references
#' Caplan, J. M., Kennedy, L. W., & Miller, J. (2011). Risk terrain modeling: Brokering criminological theory and GIS methods for crime forecasting. *Justice Quarterly*, 28(2), 360-381.
#'
#' Wheeler, A. P., & Steenbeek, W. (2021). Mapping the risk terrain for crime using machine learning. *Journal of Quantitative Criminology*, 37(2), 445-480.
#' 
kern_xy <- function(base,feat,bandwidth,weight=1){
    nr <- nrow(base)
    res_weight <- vector('numeric',nr)
    fco <- sp::coordinates(feat)
    fx <- fco[,1]
    fy <- fco[,2]
    bco <- sp::coordinates(base)
    if (weight == 1){ fw <- 1
    } else { fw <- feat@data[,weight] }
    for (i in 1:nr){
        l <- t(bco[i,])
        di <- sqrt( (l[1]-fx)^2 + (l[2]-fy)^2 )
        res_weight[i] <- kern_fun(di,bandwidth,fw) 
    }
    return(res_weight)
}

bisq_fun <- function(d,b){
    ifelse(d < b, ( 1 - (d/b)^2 )^2, 0)
}

#' Bisquare weighted sum
#'
#' Given a base X/Y dataset, calculates bisquare weighted sums of points from feature dataset
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param bandwidth distances above this value do not contribute to the bi-square weight
#' @param weight if 1 (default), does not use attribute weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates bi-square distance weighted sums of features within specified distance of the `base` centroid. 
#' Bisquare weights are calculated as:
#' \deqn{w_{ij} = [ 1 - (d_{ij}/b)^2 ]^2 }
#' where d_ij is the Euclidean distance between the base point and and the feature point. If d < b, then w_ij equals 0. These are then multiplied
#' and summed so each base point gets a cumulative weighted sum. See the GWR book for a reference. 
#' Uses loops and calculates all pairwise distances, so can be slow for large base and feature datasets. Consider
#' aggregating/weighting feature dataset if it is too slow. Useful for quantifying features nearby (Groff, 2014), or for egohoods
#' (e.g. spatial smoothing of demographic info, Hipp & Boessen, 2013). 
#' @returns
#' A vector of bi-square weighted sums
#' @export
#' @examples
#' data(nyc_cafe); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,3000)
#' gr_nyc$bscafe <- bisq_xy(gr_nyc,nyc_cafe,5000)
#' gr_nyc$bstabl <- bisq_xy(gr_nyc,nyc_cafe,5000,'SWC_TABLES')
#' head(gr_nyc)
#' sp::spplot(gr_nyc,zcol='bscafe') #bisquare weights for cafes
#' 
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [count_xy()] for counting points inside polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords 
#' 
#' @references
#' Fotheringham, A. S., Brunsdon, C., & Charlton, M. (2003). G*eographically weighted regression: the analysis of spatially varying relationships*. John Wiley & Sons.
#' 
#' Groff, E. R. (2014). Quantifying the exposure of street segments to drinking places nearby. *Journal of Quantitative Criminology*, 30(3), 527-548.
#' 
#' Hipp, J. R., & Boessen, A. (2013). Egohoods as waves washing across the city: A new measure of “neighborhoods”. Criminology, 51(2), 287-327.
#' 
bisq_xy <- function(base,feat,bandwidth,weight=1){
    nr <- nrow(base)
    res_weight <- vector('numeric',nr)
    fco <- sp::coordinates(feat)
    fx <- fco[,1]
    fy <- fco[,2]
    bco <- sp::coordinates(base)
    if (weight == 1){ fw <- 1
    } else { fw <- feat@data[,weight] }
    for (i in 1:nr){
        l <- t(bco[i,])
        di <- sqrt( (l[1]-fx)^2 + (l[2]-fy)^2 )
        bis <- bisq_fun(di,bandwidth)
        res_weight[i] <- sum(bis*fw)
    }
    return(res_weight)
}

# Inverse Distance weight
idw_fun <- function(d,clip){
    dc <- ifelse(d > clip, 1/d, 1/clip) 
    return(dc)
}

#' Inverse distance weighted sums
#'
#' Given a base X/Y dataset, calculates clipped inverse distance weighted sums of points from feature dataset
#'
#' @param base base dataset (eg gridcells), needs to be SpatialPolygonsDataFrame
#' @param feat feature dataset (eg another crime generator), needs to be SpatialPointsDataFrame
#' @param clip scaler minimum value for weight, default `1` (so weights cannot be below 0)
#' @param weight if 1 (default), does not use weights, else pass in string that is the variable name for weights in `feat`
#'
#' @details This generates a inverse distance weighted sum of features within specified distance of the `base` centroid. 
#' Weights are clipped to never be below `clip` value, which prevents division by 0 (or division by a very small distance number)
#' Uses loops and calculates all pairwise distances, so can be slow for large base and feature datasets. Consider
#' aggregating/weighting feature dataset if it is too slow. Useful for quantifying features nearby (Groff, 2014), or for egohoods
#' (e.g. spatial smoothing of demographic info, Hipp & Boessen, 2013). 
#' @returns
#' A vector of IDW weighted sums
#' @export
#' @examples
#' data(nyc_cafe); data(nyc_bor)
#' gr_nyc <- prep_grid(nyc_bor,3000)
#' gr_nyc$idwcafe <- idw_xy(gr_nyc,nyc_cafe)
#' gr_nyc$idwtabl <- idw_xy(gr_nyc,nyc_cafe,weight='SWC_TABLES')
#' head(gr_nyc)
#' sp::spplot(gr_nyc,zcol='idwtabl') #inverse distance weighted tables
#' 
#' @seealso 
#' [dist_xy()] for calculating distance to nearest
#' 
#' [count_xy()] for counting points inside polygon
#' 
#' [kern_xy()] for estimating gaussian density of points for features at base polygon xy coords
#' 
#' [bisq_xy()] to estimate bi-square kernel weights of points for features at base polygon xy coords
#' 
#' [idw_xy()] to estimate inverse distance weights of points for features at base polygon xy coords 
#' 
#' @references
#' Groff, E. R. (2014). Quantifying the exposure of street segments to drinking places nearby. *Journal of Quantitative Criminology*, 30(3), 527-548.
#' 
#' Hipp, J. R., & Boessen, A. (2013). Egohoods as waves washing across the city: A new measure of “neighborhoods”. Criminology, 51(2), 287-327.
#' 
idw_xy <- function(base,feat,clip=1,weight=1){
    nr <- nrow(base)
    res_weight <- vector('numeric',nr)
    fco <- sp::coordinates(feat)
    fx <- fco[,1]
    fy <- fco[,2]
    bco <- sp::coordinates(base)
    if (weight == 1){ fw <- 1
    } else { fw <- feat@data[,weight] }
    for (i in 1:nr){
        l <- t(bco[i,])
        di <- sqrt( (l[1]-fx)^2 + (l[2]-fy)^2 )
        id <- idw_fun(di,clip)
        res_weight[i] <- sum(id*fw)
    }
    return(res_weight)
}


#' Voronoi tesselation from input points
#'
#' Given an outline and feature points, calculates Voronoi areas
#'
#' @param outline object that can be coerced to a spatstat window via `as.owin` (so SpatialPolygonDataFrame, SpatialPolygon, owin)
#' @param feat A SpatialPointsDataFrame object (if duplicate X/Y coordinates will get errors)
#'
#' @details Outline should be a single polygon area. Uses spatstats `dirichlet` and window to compute the Voronoi tesselation.
#' Will generate errors if feat has duplicate X/Y points. Useful to create areas for other functions, 
#' such as `dcount_xy()` or `count_xy()`. Common spatial unit of analysis used in crime research when using points (e.g. intersections
#' and street midpoints). 
#' 
#' @returns
#' A SpatialPolygonsDataFrame object, including the dataframe for all the info in the orignal `feat@data` dataframe.
#' @export
#' @examples
#' library(sp) #for plot methods
#' library(maptools) #for conversion
#' data(nyc_bor)
#' nyc_buff <- raster::buffer(nyc_bor,50000)
#' po <- sp::spsample(nyc_buff,50,'hexagonal')
#' po$id <- 1:dim(coordinates(po))[1] #turns into SpatialDataFrame
#' vo <- vor_sp(nyc_buff,po)
#' plot(vo)
#' plot(nyc_buff,border='RED',lwd=3, add=TRUE)
#' 
#' @references
#' Wheeler, A. P. (2018). The effect of 311 calls for service on crime in DC at microplaces. *Crime & Delinquency*, 64(14), 1882-1903.
#' 
#' Wheeler, A. P. (2019). Quantifying the local and spatial effects of alcohol outlets on crime. *Crime & Delinquency*, 65(6), 845-871.
#' 
vor_sp <- function(outline,feat){
    # Create the window
    outline_win <- spatstat.geom::as.owin(outline) 
    # Create the spatstat ppp
    fxy <- sp::coordinates(feat)
    pp <- spatstat.geom::ppp(fxy[,1], fxy[,2], window=outline_win)
    # Create the tesselation
    tess <- spatstat.geom::dirichlet(pp)
    # Convert into spatial polygon (needs ?maptools?)
    sp_object <- methods::as(tess, "SpatialPolygons")
    spdf <- sp::SpatialPolygonsDataFrame(sp_object, feat@data)
    return(sp_object)
}



#ToDo CPP functions for loops