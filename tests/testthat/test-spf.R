# Testing the spatial feature functions


test_that("prep_grid check", {
  # simple outline polygon
  pr <- "+proj=lcc +lat_0=40.1666666666667 +lon_0=-74 +lat_1=40.6666666666667 +lat_2=41.0333333333333 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=us-ft +no_defs" #NYC projection
  r1 <- cbind( c(0,1,1,0,0),c(0,0,1,1,0) )
  sr1 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(r1)),"r1")), proj4string=sp::CRS(pr))
  #plot(sr1,lwd=3) #simple plot to check
  #plot(resg,border='blue',add=TRUE)
  # grid cells should all be the same
  resg <- prep_grid(sr1,0.2)
  nr <- nrow(resg)
  ch_area <- rep(0.2^2,nrow(resg))
  expect_equal(raster::area(resg),ch_area)
  # single point, only should have 1 grid cell
  x1 <- c(0.5); y1 <- c(0.5)
  op <- sp::SpatialPointsDataFrame(cbind(x1,y1), data=data.frame(id=1), proj4string = sp::CRS(pr))
  res1 <- prep_grid(sr1,0.2,point_over=op)
  expect_equal(nrow(res1),1)
  # no grid outside, need a different shape
  cr1 <- raster::buffer(op,1)
  resc <- prep_grid(cr1,0.2,0.5)
  #plot(cr1,lwd=3)
  #plot(resc,add=TRUE)
  expect_gte(min(resc$cover),0.5)
})

# test hexgrid
 # all same size
 # area outside
 # only 1 point over
 # only 2 points over
 

# test voronoi
  # input number same as output
  # cumulative area same as outline area

# test weighting functions

# test distance functions