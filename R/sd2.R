sd2 <- 
function (x, is.sample = TRUE, weighting = NULL, wmean = FALSE, na.rm = FALSE)
    
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
    
    if (wmean == FALSE)
    {
      mean_x <- mean(x)
    }
    else
    {
      mean_x <- (sum(x*weighting)/sum(weighting))
    }
    
    if (!is.null(weighting))
    {
      dev_x_sq <- weighting*(x-mean_x)^2
      sd_x <- sqrt(sum(dev_x_sq)/sum(weighting))
    }
    else
    {
      dev_x_sq <- (x-mean_x)^2
      sd_x <- sqrt(sum(dev_x_sq)/n)
    }
    
    return(sd_x)
    
}