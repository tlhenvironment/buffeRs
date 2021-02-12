#' A Semicircle Function
#'
#' Creates a semicircular polygon, wrapper around \code{buffer_wedge(point, radius, degree, degree_width = 45)}
#' @param point Centre point of the buffer, must equal to true in: \code{sf::st_is(point, "POINT")}
#' @param radius Radius of the buffer (numeric)
#' @param degree The angle at which the wedge is centred (clockwise). Must be between 0 and 360 (numeric) 
#' @keywords wedge semicircle
#' @return An object of class \code{sfc_POLYGON}
#' @export
#' @examples 
#' example_point = sf::st_point(c(1,2))
#' example_point = sf::st_sfc(example_point)
#' example_point = sf::st_sf(example_point)
#' 
#' buffer_semicircle(example_point, 200, 90) -> semicircular_buffer
#' plot(semicircular_buffer)
#' @export

buffer_semicircle <- function(point, radius, degree){
  buffer_wedge(point = point, radius = radius, degree = degree, degree_width = 90) -> semicircle_sf

  #return
  return(semicircle_sf)  
}
