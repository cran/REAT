mean2 <-
function (x, weighting = NULL, output = "mean", na.rm = FALSE)
{
  
  if (!is.null(weighting))
  {
    if (length(x) != length(weighting))
    {
      stop("Frequency and weighting differ in length", call. = FALSE)
    }
  }
  
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  
  if (output == "geom") {
    geom_x <- exp(mean(log(x)))
    return (geom_x)
  }
  
  else {
  
  if (!is.null(weighting)) {
    mean_x <- sum(x*weighting)/sum(weighting)
  }
  else {
    mean_x <- sum(x)/length(x)
  }
    
  return (mean_x)
    
  }
  
}
