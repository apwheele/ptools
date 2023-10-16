## code to prepare datasets

# NYC projection 
#pr <- "+proj=lcc +lat_0=40.1666666666667 +lon_0=-74 +lat_1=40.6666666666667 +lat_2=41.0333333333333 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=us-ft +no_defs"
pr <- "+proj=lcc +lat_0=40.1666666666667 +lon_0=-74 +lat_1=40.6666666666667 +lat_2=41.0333333333333 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"
#pr <- sp::CRS("+init=epsg:2908")


prep_nyc_shootings <- function(){
    curr_url <- 'https://data.cityofnewyork.us/api/views/5ucz-vwe8/rows.csv?accessType=DOWNLOAD'
    hist_url <- 'https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD'
    old <- read.csv(hist_url,stringsAsFactors=FALSE)
    new <- read.csv(curr_url,stringsAsFactors=FALSE)
    # Final column I know is off
    names(new)[ncol(new)] <- "Lon_Lat"
    # Making sure names are the same
    print('These are columns that are not the same between the two and are being dropped')
    print( setdiff(names(old),names(new)) )
    combo_names = intersect(names(old),names(new))
    shooting <- rbind(old[,combo_names],new[,combo_names])
    # The geo coordinate fields need to be converted to numbers
    coord_fields <- c('X_COORD_CD','Y_COORD_CD')
    for (c in coord_fields){
        #replacing commas in 2018 data
        shooting[,c] <- as.numeric(gsub(",","",shooting[,c])) #replacing commas in 2018 data
    }
    # Dates (should maybe concat date and time
    shooting$OCCUR_DATE <- as.Date(shooting$OCCUR_DATE, format = "%m/%d/%Y", tz = "America/New_York")
    # Converting characters to factors for storage, leave OCCUR_TIME as string
    types <- sapply(shooting, class)
    for (n in names(types)){
        if (types[n] == "character" & n != 'OCCUR_TIME'){
            shooting[,n] <- as.factor(shooting[,n])
        }
    }
    # dropping fields
    drop_fields <- c('Lon_Lat')
    keep_fields <- names(shooting)[!(names(shooting) %in% drop_fields)]
    # Maybe do a nice table/map to make sure no funny business
    #MoYr <- format(shooting$OCCUR_DATE, "%Y-%m")
    Year <- format(shooting$OCCUR_DATE,"%Y")
    print("Shootings per Year as a quick data check")
    print(as.data.frame(table(Year)))
    #print(as.data.frame(table(MoYr)))
    # Creating a spatial points data frame
    res <- shooting[,keep_fields]
    sp_res <- sp::SpatialPointsDataFrame(res[,coord_fields],data=res,proj4string=sp::CRS(pr))
    return(sp_res)
}

nyc_shoot <- prep_nyc_shootings()
usethis::use_data(nyc_shoot, overwrite = TRUE)

# NYC Boroughs outline
nyc_borough <- function(){
    bor_url <- 'https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nybb_21c.zip'
    temp <- tempfile("nybb_21c",fileext = ".zip")
    download.file(bor_url,temp)
    unzip(temp,exdir=path.expand('~/temp'))
    shp <- path.expand('~/temp/nybb_21c/nybb.shp')
    res_bor <- terra::vect(shp)
    nyc_sf <- sf::st_read(dsn=shp)
    nyc_sf <- sf::st_simplify(nyc_sf, preserveTopology = TRUE,dTolerance = 500)
    # Clean up the files
    unlink(temp)
    unlink(path.expand('~/temp/nybb_21c'),recursive=TRUE,force=TRUE)
    # Simplifying the boundary a bit to make file smaller
    nyc_sp <- sf::as_Spatial(nyc_sf)
    sp::proj4string(nyc_sp) <- pr
    return(nyc_sp)
}

nyc_bor <- nyc_borough()
usethis::use_data(nyc_bor, overwrite = TRUE)

# Liquor Stores in NYC
liquor_bor <- function(outline){
    li_url <- "https://data.ny.gov/api/views/hrvs-fxs2/rows.csv?accessType=DOWNLOAD"
    liq <- read.csv(li_url,stringsAsFactors=FALSE)
    # Get rid of those without geo
    liq <- liq[liq$Georeference != "",]
    # Creating numeric Lon/Lat coordinates
    s1 <- gsub("POINT (","",liq$Georeference,fixed=TRUE)
    s2 <- gsub(")","",s1,fixed=TRUE)
    s3 <- do.call(rbind,strsplit(s2," "))
    liq$lon <- as.numeric(s3[,1])
    liq$lat <- as.numeric(s3[,2])
    # Projectiong to local
    proj_nyc <- proj4::project(as.matrix(liq[,c('lon','lat')]), proj=pr)
    liq$x <- proj_nyc[,1]
    liq$y <- proj_nyc[,2]
    # I don't need a bunch of these columns
    # "Method.of.Operation","Premise.Name" (need to clean up unicode)
    keep_cols <- c("Serial.Number","lon","lat","x","y")
    liq <- liq[,keep_cols]
    #liq$Method.of.Operation <- as.factor(liq$Method.of.Operation)
    # Turning into a spatial data frame
    liq_sp <- sp::SpatialPointsDataFrame(liq[,c("x","y")],data=liq,proj4string=sp::CRS(pr))
    # Note some of these are headquarters, so not the actual selling premise
    #plot(liq_sp)
    # Getting those over NYC boroughs (outline)
    liq_sp_nyc <- liq_sp[outline,]
    return(liq_sp_nyc)
}

#nyc_bor@proj4string@projargs

nyc_liq <- liquor_bor(nyc_bor)
usethis::use_data(nyc_liq, overwrite = TRUE)

# Sidewalk Cafes in nyc
side_cafe <- function(outline){
    url <- "https://data.cityofnewyork.us/api/views/qcdj-rwhu/rows.csv?accessType=DOWNLOAD"
    side_cafe <- read.csv(url,stringsAsFactors=FALSE)
    # only worrying about active
    side_cafe <- side_cafe[side_cafe$LIC_STATUS == 'Active',]
    side_cafe$x <- side_cafe$FINAL_X
    side_cafe$y <- side_cafe$FINAL_Y
    side_cafe <- side_cafe[,c('LICENSE_NBR','BUSINESS_NAME','SWC_SQ_FT','SWC_TABLES','SWC_CHAIRS',
                              'x','y')]
    #side_cafe$LIC_STATUS <- as.factor(side_cafe$LIC_STATUS)
    #proj_nyc <- proj4::project(as.matrix(side_cafe[,c('LONGITUDE','LATITUDE')]), proj=pr)
    #side_cafe$x <- proj_nyc[,1]
    #side_cafe$y <- proj_nyc[,2]
    # Getting rid of points duplicated at same location
    side_cafe <- side_cafe[!duplicated(side_cafe[,c('x','y')]),]
    cafe_sp <- sp::SpatialPointsDataFrame(side_cafe[,c("x","y")],data=side_cafe,proj4string=sp::CRS(pr))
    return(cafe_sp[outline,])
}

nyc_cafe <- side_cafe(nyc_bor)
usethis::use_data(nyc_cafe, overwrite = TRUE)

#plot(nyc_bor) # To check everything is good
#plot(nyc_shoot,add=TRUE,pch='.')

# This should just show the outline pretty well
#plot(nyc_liq)
#plot(nyc_cafe) #currently none in Staten Island