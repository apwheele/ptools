# Functions for working with hexagons in ggplot
# For maps, I want them to be a specific size

#get width given height
wd_hex <- function(height){
  tri_side <- height/2
  sma_side <- height/4
  width <- 2*sqrt(tri_side^2 - sma_side^2)
  return(width)
}

#now to figure out the area if you want
#side is simply height/2 in geom_hex
hex_area <- function(side){
  area <- 6 * (  (sqrt(3)*side^2)/4 )
  return(area)
}

#So if you want your hexagon to have a regular area need the inverse function
#Gives height and width if you want a specific area
hex_dim <- function(area){
  num <- 4*area
  den <- 6*sqrt(3)
  vert <- 2*sqrt(num/den)
  horz <- wd_hex(height)
  return(c(vert,horz))
}