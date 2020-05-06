#' Forest Growth Model 
#' Model output is in units of kgC to represent forest growth
#' @param r exponential growth rate of the forest
#' @param g linear growth rate of the forest
#' @param K carrying capacity


forest<- function(initial_kgC, early_growth, later_growth, K, canopy_thresh, temp, time){

  # Set the initial size of forest
  kgC = initial_kgC
  
  # Calculate the growth rate at each time step in a for loop
  for (i in 1:time) {
    
    kgC = kgC * early_growth
    kgC = ifelse(temp < 0, 0, kgC)
    kgC = ifelse(kgC > canopy_thresh, kgC*later_growth, kgC)
    kgC = ifelse(kgC >= K, 0, kgC)
    
  }
  return(kgC)
    }