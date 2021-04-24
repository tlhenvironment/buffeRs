#' A square Function
#'
#' Creates a square polygon, wrapper around \code{buffer_rectangle(point, x_length = length, y_length = length, degree)}
#' @param point Centre point of the buffer, must equal to true in: \code{sf::st_is(point, "POINT")}
#' @param length Length of the square sides, should be in the unit of projection (numeric)
#' @param degree The angle at which the square  is centred (clockwise). Must be between 0 and 360 (numeric) 
#' @return An object of class \code{sfc_POLYGON}
#' @keywords square
#' @export
#' @examples
#' example_point = sf::st_point(c(1,2))
#' example_point = sf::st_sfc(example_point)
#' example_point = sf::st_sf(example_point)
#' buffer_square(example_point, 90, 22) -> square_shaped_buffer
#' plot(square_shaped_buffer)
#' @export

buffer_square <- function(point, length, degree = 0){
  return(buffer_rectangle(point, length, length, degree))  
}
