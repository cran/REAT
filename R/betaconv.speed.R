betaconv.speed <-
function (beta, tinterval, output.results = TRUE) 
{
  if (beta < 0)
  {
    lambda <- -log(1+beta)/tinterval  

    halflife <- log(2)/lambda
  }
  else
  {
    lambda <- NA
    halflife <- NA
  }
  
  results <- matrix(nrow = 2, ncol = 1)
  results[1,1] <- lambda
  results[2,1] <- halflife
  rownames(results) <- c("Lambda", "Half-Life")
  colnames(results) <- c("Estimates")
  
  if (output.results == TRUE) {
    cat ("Beta Convergence: Speed and Half-Life", "\n", "\n")
    print(as.data.frame(results))
  }
  
  invisible(results)
}
