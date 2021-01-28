#' A wedge Function
#'
#' Creates a wedge polygon 
#' @param point An sf variable, \code{\link[sf]{st_is(point, "POINT")}} must be TRUE.
#' @param radius numeric
#' @param degree numeric; must be between 0 and 360. The angle at which the wedge is centred (clockwise).
#' @param degree_width numeric; must be between 0 and 360. The width of the wedge.
#' @keywords wedge
#' @export
#' @examples buffer_wedge()
#' @export

buffer_wedge <- function(point, radius, degree, degree_width){

  #Error handlers for input type
  if(nrow(point) != 1){
    stop("Input one point of class sf")
  }

  if(!sf::st_is(point, "POINT")){
    stop("Input one point of class sf")
  }

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
  pts_df = as.data.frame(matrix(nrow = 1+degree_width*2, ncol = 2))
  colnames(pts_df) <- c("x", "y")
  
  #set the starting points
  pts_df[1,1] <- point_coordinates[1]
  pts_df[1,2] <- point_coordinates[2]
  
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
