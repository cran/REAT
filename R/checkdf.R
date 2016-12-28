checkdf <-
function (dataset, ...) 
{
  
  vars <- unlist(list(...))
  vars_count <- length(vars)
  
  i <- 0
  
  for (i in 1:vars_count)
  {
    var_name <- vars[i]
    
    if (!exists(var_name, dataset))
    { 
      stop(paste("Variable", var_name, "not found"), call. = FALSE)
    }  
  }
}
