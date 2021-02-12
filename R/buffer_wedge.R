#' A wedge Function
#'
#' Creates a wedge polygon 
#' @param point Centre point of the buffer, must equal to true in: \code{sf::st_is(point, "POINT")}
#' @param radius Radius of the buffer (numeric)
#' @param degree The angle at which the wedge is centred (clockwise). Must be between 0 and 360 (numeric) 
#' @param degree_width Width of the wedge. Must be between 0 and 360 (numeric) 
#' @return An object of class \code{sfc_POLYGON}
#' @keywords wedge
#' @export
#' @examples
#' example_point = sf::st_point(c(1,2))
#' example_point = sf::st_sfc(example_point)
#' example_point = sf::st_sf(example_point)

#' buffer_wedge(example_point, 200, 90, 45) -> wedge_shaped_buffer
#' plot(wedge_shaped_buffer)
#' @export

buffer_wedge <- function(point, radius, degree, degree_width){

  #Error handlers for input type
  if(!("sf" %in% class(point))) stop("Input one point of class sf")
  if(nrow(point) != 1) stop("Input one point of class sf")
  if(!sf::st_is(point, "POINT")) stop("Input one point of class sf")

  if(!is.numeric(degree) || degree < 0 || degree > 360){
    stop("Input degree as numeric between 0 and 360")
  }

  if(!is.numeric(degree_width) || degree_width < 0 || degree_width > 360){
    stop("Input degree_width as numeric between 0 and 360")
  }
  #error handler if degree_width is very small
  if(degree_width < 1){
    warning("Degree is smaller than 1, set degree to 1")
    degree_width <- 1
    }
  
  #function starts
  point_coordinates <- sf::st_coordinates(point)
  
  angles <- c(degree-degree_width - 90, degree+degree_width - 90)
  radians <- angles * 0.0174532925
  
  #create an empty matrix
  pts_df = as.data.frame(matrix(nrow = 1 + degree_width * 20, ncol = 2))

  colnames(pts_df) <- c("x", "y")
  
  #set the starting points
  pts_df[1,1] <- point_coordinates[1]
  pts_df[1,2] <- point_coordinates[2]
  
  #set the first points
  pts_df[2,1] <- radius*cos(-radians[1]) + point_coordinates[1]
  pts_df[2,2] <- radius*sin(-radians[1]) + point_coordinates[2]
  
  #set the last points
  pts_df[nrow(pts_df),1] <- radius*cos(-radians[2]) + point_coordinates[1]
  pts_df[nrow(pts_df),2] <- radius*sin(-radians[2]) + point_coordinates[2]
  
  #set all intermediate points
  for(i in 3:nrow(pts_df)-1){
    pts_df[i,1] <- radius*cos(-radians[1] * (nrow(pts_df) - i)/nrow(pts_df) - radians[2] * (i / nrow(pts_df))) + point_coordinates[1]
    pts_df[i,2] <- radius*sin(-radians[1] * (nrow(pts_df) - i)/nrow(pts_df) - radians[2] * (i / nrow(pts_df))) + point_coordinates[2]
  }
    
  #convert to sf
  pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"), crs = sf::st_crs(point))
  
  ##create the convex hull
  wedge_sf <- sf::st_convex_hull(sf::st_union(pts_sf))
  
  #return
  return(wedge_sf)  
}
