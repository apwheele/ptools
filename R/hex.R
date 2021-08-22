# Functions for working with hexagons in ggplot
# For maps, I want them to be a specific size

#' Get width of hexagon given height
#'
#' @param height scaler 
#'
#' @details For use with ggplot and `geom_hex` binwidth arguments, which expects arguments in width/height. I want hexagons in maps to be a specific area.
#' @returns A scaler for the width
#' @export
#' @examples
#' area_check <- 1000
#' wh <- hex_dim(area_check^2)   #e.g. a square kilometer if spatial units are in meters
#' area <- hex_area(wh[1]/2)       #inverse operation
#' all.equal(area_check,sqrt(area))
#' wi <- hex_wd(wh[1])
#' all.equal(wh[2],wi)
#' 
#' @seealso 
#' [hex_area()] for estimating the area given side length
#' [hex_dim()] for estimating width/height given area
hex_wd <- function(height){
  tri_side <- height/2
  sma_side <- height/4
  width <- 2*sqrt(tri_side^2 - sma_side^2)
  return(width)
}

#' Get area of hexagon given length of side
#'
#' The length of the side is half of the length from vertex to vertex (so height in `geom_hex`).  
#'
#' @param side scaler 
#'
#' @details For use with ggplot and `geom_hex` binwidth arguments, which expects arguments in width/height. I want hexagons in maps to be a specific area.
#' @returns A scaler for the width
#' @export
#' @examples
#' area_check <- 1000
#' wh <- hex_dim(area_check^2)   #e.g. a square kilometer if spatial units are in meters
#' area <- hex_area(wh[1]/2)       #inverse operation
#' all.equal(area_check,sqrt(area))
#' wi <- hex_wd(wh[1])
#' all.equal(wh[2],wi)
#' 
#' @seealso 
#' [hex_wd()] for estimating the width given the height
#' [hex_dim()] for estimating width/height given area
hex_area <- function(side){
  area <- 6 * (  (sqrt(3)*side^2)/4 )
  return(area)
}

#' Get dimensions of hexagon given area
#'
#' @param area scaler 
#'
#' @details For use with ggplot and `geom_hex` binwidth arguments, which expects arguments in width/height. I want hexagons in maps to be a specific area.
#' @returns a vector with two elements, first element is the height (vertex to vertex), the second element is the width (side to side)
#' @export
#' @examples
#' area_check <- 1000
#' wh <- hex_dim(area_check^2)   #e.g. a square kilometer if spatial units are in meters
#' area <- hex_area(wh[1]/2)       #inverse operation
#' all.equal(area_check,sqrt(area))
#' wi <- hex_wd(wh[1])
#' all.equal(wh[2],wi)
#' 
#' @seealso 
#' [hex_wd()] for estimating the width given the height
#' [hex_area()] for estimating the area given side length
hex_dim <- function(area){
  num <- 4*area
  den <- 6*sqrt(3)
  height <- 2*sqrt(num/den)
  width <- hex_wd(height)
  return(c(height,width))
}