herf <-
function (x, coefnorm = FALSE, output = "HHI", na.rm = TRUE) 
{
  
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  
  n <- length (x)
  a_i <- x/sum(x)
  a_i_2 <- a_i^2
  H <- sum(a_i_2)   

  if (output == "eq")
  {
    eq <- 1/H

    return(eq)
  }
  
  else
  {
    if (coefnorm == FALSE) 
    {   
      return(H)   
    } 
    else 
    {
      H.norm <- (H-1/n)/(1-1/n)

      return (H.norm)   
    }
  }
  
}
