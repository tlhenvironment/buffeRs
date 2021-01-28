#' A wedge Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @param radius Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples wedge()
#' @export

buffer_windrose <- function(point, wind_frequency_df, radius = 100, 
  m_s_scaling_normalization = 1, width_factor = 2){
  
  #Error handlers for input type
  if(nrow(point) != 1){
    stop("Input one point of class sf")
  }

  if(!sf::st_is(point, "POINT")){
    stop("Input one point of class sf")
  }    

  mean_ws <- as.numeric(unique(wind_frequency_df$panel.fun))

  #function starts
  if(m_s_scaling_normalization == "auto"){
    m_s_scaling_normalization <- mean_ws
  }

  ##calculate percentage of mean_ws to scaling normalization (m/s)
  mean_ws / m_s_scaling_normalization -> scaling_by_strength

  #make sure can't be higher than original buffer size
  # if(m_s_scaling_normalization < mean_ws) scaling_by_strength <- 1
  
  ##scaling_by_strength_smoothing
  scaling_by_strength_smoothing(scaling_by_strength) -> scaling_by_strength_smoothed
  
  ##calculating scaling factor
  scaling_by_strength_smoothed * radius -> scaling_to_normalization
  message(scaling_to_normalization)
  
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
    
    (wind_frequency_df$freqs[i] / max(wind_frequency_df$freqs)) * (scaling_to_normalization) ->  wedge_radius #in m

  #create the wedge
    buffer_wedge(point, wedge_radius, degree, degree_width) -> wedge_sf

  #union the wedges  
    if (!exists("wedge_all_sf")){wedge_all_sf <- wedge_sf; next()}
    
    sf::st_union(wedge_all_sf, wedge_sf) -> wedge_all_sf
  }

  return(wedge_all_sf)
}