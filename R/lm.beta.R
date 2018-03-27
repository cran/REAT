lm.beta <-
function (linmod, dummy.na = TRUE) 
{

  linmod_data <- linmod$model
  var_dep <- linmod_data[,1]

  extractvar <- 1
  
  if (linmod$assign[1] == 0)
  { 
    var_ind_count <- length(linmod$assign)-1
    var_dep_col <- 1
  }
  
  else {
    var_ind_count <- length(linmod$assign)
    var_dep_col <- 0
  }
  
  var_ind_names <- colnames(linmod_data[,(extractvar+1):ncol(linmod_data)])

  i <- 0
  coeff <- vector()

  sd_varind <- vector()
  coeff_stand <- vector()

  sd_vardep <- sd(var_dep) 

  
  for (i in 1:var_ind_count)
  {
    coeff[i] <- linmod$coefficients[i+var_dep_col]

    if ((dummy.na == TRUE) && (all(linmod_data[,i+1] %in% c(1,0)))) { coeff_stand[i] <- NA }

    else
    {
      sd_varind[i] <- sd(linmod_data[,i+1])

      coeff_stand[i] <- coeff[i] * (sd_varind[i]/sd_vardep)
    }
    
  }
  
  coeff_stand_list <- as.list(coeff_stand)
  names(coeff_stand_list) <- var_ind_names

  return(coeff_stand_list)

}
