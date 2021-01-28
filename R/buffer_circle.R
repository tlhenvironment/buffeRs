#' A circule function
#'
#' Creates a circular buffer. Wrapper around sf::st_buffer()
#' @param point Centre point of the buffer
#' @param radius numeric
#' @keywords cats
#' @export
#' @examples buffer_circle()
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
  circular_buffer <- sf::st_buffer(x = point, dist = readius, ...)
  return(circular_buffer)

}
