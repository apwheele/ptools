## code to prepare datasets

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
    keep_fields <- names(nyc_shoot)[!(names(nyc_shoot) %in% drop_fields)]
    # Maybe do a nice table/map to make sure no funny business
    #MoYr <- format(shooting$OCCUR_DATE, "%Y-%m")
    Year <- format(shooting$OCCUR_DATE,"%Y")
    print("Shootings per Year as a quick data check")
    print(as.data.frame(table(Year)))
    #print(as.data.frame(table(MoYr)))
    return(shooting[,keep_fields])
}

nyc_shoot <- prep_nyc_shootings()
usethis::use_data(nyc_shoot, overwrite = TRUE)

# NYC Boroughs outline
nyc_borough <- function(){
    bor_url <- 'https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nybb_21c.zip'
    temp <- tempfile("nybb_21c",fileext = ".zip")
    download.file(bor_url,temp)
    unzip(temp,exdir=path.expand('~/temp'))
    res_bor <- readOGR(dsn=path.expand('~/temp/nybb_21c/nybb.shp'),layer='nybb')
    # Clean up the files
    unlink(temp)
    unlink(path.expand('~/temp/nybb_21c'),recursive=TRUE,force=TRUE)
    # Simplifying the boundary a bit
    nyc_simpler <- rgeos::gSimplify(res_bor, 500, topologyPreserve=TRUE)
    return(nyc_simpler)
}

nyc_bor <- nyc_borough()
plot(nyc_bor) # To check
points(nyc_shoot$X_COORD_CD,nyc_shoot$Y_COORD_CD,pch='.')
usethis::use_data(nyc_bor, overwrite = TRUE)
