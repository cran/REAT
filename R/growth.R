growth <-
function (val1, val2, growth.type = "growth") {
  
  if (nrow(as.matrix(val2)) != nrow(as.matrix(val1))) {
    stop (paste("Datasets", as.character(substitute(val1)), "and", 
         as.character(substitute(val2)), "differ in length"), call. = FALSE)
  }
  
  if ((ncol(as.data.frame(val2)) > 1)) { 

    val_all <- cbind (val1, val2)

    no_years <- ncol(val2)-1

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
      
      growth <- apply(growth_annual, 1, mean) 
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