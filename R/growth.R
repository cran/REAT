growth <- function (val1, val2, growth.type = "growth", output = "rate", log.rate = FALSE, factor.mean = "mean") {
  
  if (nrow(as.matrix(val2)) != nrow(as.matrix(val1))) {
    stop (paste("Datasets", as.character(substitute(val1)), "and", 
                as.character(substitute(val2)), "differ in length"), call. = FALSE)
  }
  
  if ((ncol(as.data.frame(val2)) > 1)) { 
    
    val_all <- cbind (val1, val2)
    
    no_years <- ncol(val2)
    
    i <- 0
    
    growth_annual <- matrix(ncol = no_years, nrow = nrow(val_all))
    
    if (growth.type == "abs")
    {
      for (i in 1:no_years) {
        growth_annual[,i] <- val_all[,(i+1)]-val_all[,i]
      }
      
      growth <- apply(growth_annual, 1, sum) 
    }
    else {
      for (i in 1:no_years) {
        growth_annual[,i] <- val_all[,(i+1)]/val_all[,i]
      }
      
      if (log.rate == TRUE) {
        growth_annual <- log10(growth_annual)
      }
      
      if (factor.mean == "geom") {
        growth <- apply(growth_annual, 1, mean2, output = "geom") 
      }
      else {
        growth <- apply(growth_annual, 1, mean2) 
      }
      
    }
    
    if (output == "annual") {
      return(growth_annual)
    }
  } 
  
  else {
    if (growth.type == "abs") {
      growth <- val2-val1
    }
    else {
      growth <- val2/val1
    }
    
  }
  
  if (growth.type == "rate")
  {
    growth <- growth-1
  }
  
  return(growth)  
  
  
}