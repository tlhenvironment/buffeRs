scaling_by_strength_smoothing <- function(scaling_by_strength, lower_bound = 0.5, higher_bound = 1.5){
  if (scaling_by_strength < lower_bound){scaling_by_strength <- lower_bound}
  if (scaling_by_strength > higher_bound){scaling_by_strength <- higher_bound}
  return(scaling_by_strength)
}