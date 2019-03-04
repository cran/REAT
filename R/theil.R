theil <- function (x, weighting = NULL, na.rm = TRUE)
{
  
  if (!is.null(weighting))
  {
    
    if (length(x) != length(weighting))
    {
      stop("Frequency and weighting differ in length", call. = FALSE)
    }
  }
  
  if (na.rm == TRUE) {
    
    if (!is.null(weighting)) {
      x_w <- as.matrix(cbind(x, weighting))

      x_w <- x_w[complete.cases(x_w),]

      x <- x_w[,1]
      weighting <- x_w[,2]
    }
    
    else {
      x <- x[!is.na(x)]
    }
    
  }
  
  
  if (any((x) <= 0)) { return (NA) }

  x_mean <- mean2(x, na.rm = na.rm)

 
  if (!is.null(weighting)) {
    
    w_share <- weighting/sum(weighting)
    
    ln_mean <- w_share*log (x_mean/x)
  } 
  
  else {
    ln_mean <- log (x_mean/x)
  }
  
  TE <- mean2 (ln_mean, na.rm = na.rm)

  return (TE)
}