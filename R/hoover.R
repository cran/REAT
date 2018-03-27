hoover <-
function (x, weighting = NULL)
{
  
  if (!is.null(weighting))

  {
    if (length(x) != length(weighting))

    {
      stop("Frequency and weighting differ in length", call. = FALSE)

    }
  }
  
  x_share <- x/sum(x)

  
  if (is.null(weighting)) {
    w_share <- 1/length(x)
  }
  else { 
    w_share <- weighting/sum(weighting) 
  }

  x_comp <- abs(x_share-w_share)

  x_comp_sum <- sum(x_comp)

  CI <- x_comp_sum/2

  return(CI)
}
