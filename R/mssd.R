mssd <-
function (x) {
  
  samplesize <- length(x)
  
  i <- 0
  j <- 1
  
  diffscores <- vector()
  
  for (i in 2:samplesize)
  {
    diffscores[j] <- x[i]-x[i-1]
    j <- j+1
  }

  diffscores_sq <- diffscores^2

  mssd_val <- sum(diffscores_sq)/(samplesize-1)

  return(mssd_val)
  
}
