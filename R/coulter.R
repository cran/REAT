coulter <- function (x, weighting = NULL, na.rm = TRUE)
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
  
  x_share <- x/sum(x)

  if (is.null(weighting)) {
    w_share <- 1/length(x)
  }
  else { 
    w_share <- weighting/sum(weighting) 
  }

  x_comp <- (x_share-w_share)^2
  x_comp_sum <- sum(x_comp)

  CC <- sqrt(x_comp_sum/2)

  return(CC)
}