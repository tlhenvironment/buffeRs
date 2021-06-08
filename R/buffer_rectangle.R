#' A rectangle Function
#'
#' Creates a rectangular polygon 
#' @param point Centre point of the buffer, must equal to true in: \code{sf::st_is(point, "POINT")}
#' @param x_length Length of the x-side (horizontal side), should be in the unit of projection (numeric)
#' @param y_length Length of the y-side (vertical side), should be in the unit of projection (numeric)
#' @param degree The angle at which the rectangle is centred (clockwise). Must be between 0 and 360 (numeric) 
#' @return An object of class \code{sfc_POLYGON}
#' @keywords rectangle
#' @export
#' @references Rotation function taken from Edzer Pebesma sf package vignette \url{https://r-spatial.github.io/sf/articles/sf3.html}
#' @examples
#' example_point = sf::st_point(c(1,2))
#' example_point = sf::st_sfc(example_point)
#' example_point = sf::st_sf(example_point)
#' buffer_rectangle(example_point, 200, 90, 22) -> rectangular_shaped_buffer
#' plot(rectangular_shaped_buffer)
#' @export

buffer_rectangle <- function(point, x_length, y_length, degree = 0){

  #Error handlers for input type
  if(!("sf" %in% class(point))) stop("Input one point of class sf")
  if(nrow(point) != 1) stop("Input one point of class sf")
  if(!sf::st_is(point, "POINT")) stop("Input one point of class sf")

  if(!is.numeric(degree) || degree < 0 || degree > 360){
    stop("Input degree as numeric between 0 and 360")
  }

  #function starts
  point_coordinates <- sf::st_coordinates(point)

  radians <- degree * 0.0174532925
  
  #create an empty matrix
  pts_df = as.data.frame(matrix(nrow = 4, ncol = 2))
  colnames(pts_df) <- c("x", "y")

  #set the points
  pts_df[1,1] <- (point_coordinates[1] - x_length / 2) 
  pts_df[1,2] <- (point_coordinates[2] + y_length / 2)

  pts_df[2,1] <- (point_coordinates[1] + x_length / 2) 
  pts_df[2,2] <- (point_coordinates[2] + y_length / 2)

  pts_df[3,1] <- (point_coordinates[1] - x_length / 2) 
  pts_df[3,2] <- (point_coordinates[2] - y_length / 2) 

  pts_df[4,1] <- (point_coordinates[1] + x_length / 2) 
  pts_df[4,2] <- (point_coordinates[2] - y_length / 2) 
    
  #convert to sf
  pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"), crs = sf::st_crs(point))
  
  ##create the convex hull
  rectangular_sf <- sf::st_convex_hull(sf::st_union(pts_sf))

  (rectangular_sf - point_coordinates) * rotation_f(radians) + point_coordinates -> rectangular_sf

  #return
  return(rectangular_sf)  
}
