sd2 <-
function (x, is.sample = TRUE, weighting = NULL, wmean = FALSE, na.rm = FALSE)
{
  
  if (!is.null(weighting))
  {
    if (length(x) != length(weighting))
    {
      stop("Frequency and weighting differ in length", call. = FALSE)
    }
  }
  
  
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
    mean_x <- mean2 (x, weighting = weighting, output = "mean")
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
