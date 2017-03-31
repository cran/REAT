cv <- 
function (x, is.sample = TRUE, coefnorm = FALSE, weighting = NULL, wmean = FALSE, na.rm = FALSE)
  
{ 
  
  if (na.rm == TRUE)
  {
    x <- x[!is.na(x)]
  }
  
  if (is.sample == TRUE)
  {
    n <- length(x)-1
  }
  else
  {
    n <- length(x)
  }
  
  
  if (!is.null(weighting)) 
  {
    
    if (wmean == TRUE)
    {
      mean_x <- (sum(x*weighting)/sum(weighting))
    }
    else
    {
      mean_x <- mean(x)
    }
    
    sd_x <- sd2(x, is.sample = is.sample, weighting = weighting, wmean = wmean, na.rm = na.rm)
  }
  
  else
    
  {
    mean_x <- mean(x)
    
    sd_x <- sd2(x, is.sample = is.sample, weighting = NULL, wmean = FALSE, na.rm = na.rm)
  }
  
  v <- sd_x/abs(mean_x)
  
  if (coefnorm == FALSE) {
    return (v)
  }   
  
  if (coefnorm == TRUE) {
    v.norm <- v/sqrt(n)
    return(v.norm)
  }
  
}