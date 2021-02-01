#' A circule function
#'
#' Creates a circular buffer. Wrapper around sf::st_buffer()
#' @param point Cente point of the buffer, must equal to true in: sf::st_is(point, "POINT")
#' @param radius Radius of the buffer (numeric)
#' @param ... Further arguments to give to sf::st_buffer()
#' @keywords cats
#' @export
#' @examples 
#' example_point = sf::st_point(c(1,2))
#' example_point = sf::st_sfc(example_point)
#' 
#' plot(buffer_circle(example_point, 200))
#' @export

buffer_circle <- function(point, radius, ...){

  #Error handlers for input type
  if(length(point) != 1){
    stop("Input one point of class sf")
  }

  if(!sf::st_is(point, "POINT")){
    stop("Input one point of class sf")
  }


  #function starts
  circular_buffer <- sf::st_buffer(x = point, dist = radius, ...)
  return(circular_buffer)

}
