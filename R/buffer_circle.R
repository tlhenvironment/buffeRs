#' A circule function
#'
#' Creates a circular buffer. Wrapper around sf::st_buffer()
#' @param point Cente point of the buffer, must equal to true in: \code{sf::st_is(point, "POINT")}
#' @param radius Radius of the buffer (numeric)
#' @param ... Further arguments to give to \code{sf::st_buffer()}
#' @return An object of class \code{sfc_POLYGON}
#' @keywords circle
#' @export
#' @examples 
#' example_point = sf::st_point(c(1,2))
#' example_point = sf::st_sfc(example_point)
#' example_point = sf::st_sf(example_point)
#' 
#' buffer_circle(example_point, 200) -> circular_buffer
#' plot(circular_buffer)
#' @export

buffer_circle <- function(point, radius, ...){

  #Error handlers for input type
  if(!("sf" %in% class(point))) stop("Input one point of class sf")
  if(nrow(point) != 1) stop("Input one point of class sf")
  if(!sf::st_is(point, "POINT")) stop("Input one point of class sf")

  #function starts
  circular_buffer <- sf::st_buffer(x = point, dist = radius, ...)
  return(circular_buffer)

}
