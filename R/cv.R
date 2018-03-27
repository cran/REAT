cv <-
function (x, is.sample = TRUE, coefnorm = FALSE, weighting = NULL, wmean = FALSE, na.rm = FALSE)
{ 
  if (is.sample == TRUE)

  {
    n <- length(x)-1

  }
  else

  {
    n <- length(x)

  }
  
  mean_x <- mean2 (x, weighting = weighting, output = "mean", na.rm = FALSE)
  
  sd_x <- sd2(x, is.sample = is.sample, weighting = weighting, wmean = wmean, na.rm = na.rm)
  
  v <- sd_x/abs(mean_x)

  if (coefnorm == FALSE) {
    return (v)

  }   
  
  if (coefnorm == TRUE) {
    v.norm <- v/sqrt(n)

    return(v.norm)
  }
  
}
