atkinson <- function (x, epsilon = 0.5, na.rm = TRUE) {
  
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  
  x_mean <- mean2(x, na.rm = na.rm)

  if (epsilon != 1) {
    
    x_eps <- (x/x_mean)^(1-epsilon)
    x_eps_mean <- mean2(x_eps)
    AI <- 1-(x_eps_mean^(1/(1-epsilon)))
    
  }
  
  else if (epsilon == 0) 
  {
    AI <- 1
  }
  
  else {

    if (any((x) <= 0)) { return (NA) }

    x_gmean <- mean2(x, output = "geom")
    
    AI <- 1-(x_gmean/x_mean)

  }
  
  return(AI)
  
}