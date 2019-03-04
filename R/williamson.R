williamson <- function (x, weighting, coefnorm = FALSE, wmean = FALSE, na.rm = TRUE) {
  
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
  
  w_share <- weighting/sum(weighting)
  
  WI <- cv(x, is.sample = FALSE, coefnorm = coefnorm, weighting = w_share, wmean = wmean, na.rm = na.rm)

  return(WI)

  
}