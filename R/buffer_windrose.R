#' A wind-rose shaped buffer function
#'
#' @description
#' `buffer_windrose` creates a wind-rose based buffer shape.
#' 
#' @param point Centre point of the buffer, must equal to true in: \code{sf::st_is(point, "POINT")}
#' @param wind_frequency_df A wind frequency table, in the format provided by \code{"openair::windRose(wind_sample)$data"}
#' @param radius radius of the buffer (numeric). The radius of the largest sub-wedge of the wind-rose shaped buffer
#' @param width_factor Scaling factor of the width of sub-wedges (numeric). Smaller number (<1) emphasize less-dominant wind-directions, (>1) emphasize dominant wind-directions. 
#' @return An object of class \code{sfc_POLYGON}
#' @keywords Wind-rose wind
#' @export
#' @examples
#' example_point = sf::st_point(c(1,2))
#' example_point = sf::st_sfc(example_point)
#' example_point = sf::st_sf(example_point)
#' 
#' openair::windRose(wind_sample) -> wind_sample_wind_rose
#' wind_sample_wind_rose$data -> wind_frequency_df
#' 
#' buffer_windrose(example_point, wind_frequency_df, 100, 0.5) -> windrose_shaped_buffer
#' plot(windrose_shaped_buffer)
#' @export

buffer_windrose <- function(point, wind_frequency_df, radius = 100, width_factor = 2){
  
  #Error handlers for input type
  if(!("sf" %in% class(point))) stop("Input one point of class sf")
  if(nrow(point) != 1) stop("Input one point of class sf")
  if(!sf::st_is(point, "POINT")) stop("Input one point of class sf")


  #function starts
  mean_ws <- as.numeric(unique(wind_frequency_df$panel.fun))
  
  for (i in 1:nrow(wind_frequency_df)){
  #reconstruct the values dynamic wedge needs to from windrose
    ##degree
    wind_frequency_df$wd[i] -> degree
    
    ##skip if degee is error
    if(degree == -999){
      next()}
    
    ##degree_width, fix so that wedges are connected is at 16, at 15 they are slightly not touching
    round(wind_frequency_df$Interval3[i] * width_factor) -> degree_width
    if (degree_width < 16){degree_width <- 16}
    
    (wind_frequency_df$freqs[i] / max(wind_frequency_df$freqs)) * (radius) ->  wedge_radius #in m

  #create the wedge
    buffer_wedge(point, wedge_radius, degree, degree_width) -> wedge_sf

  #union the wedges  
    if (!exists("wedge_all_sf")){wedge_all_sf <- wedge_sf; next()}
    
    sf::st_union(wedge_all_sf, wedge_sf) -> wedge_all_sf
  }

  return(wedge_all_sf)
}
