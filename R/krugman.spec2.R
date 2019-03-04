krugman.spec2 <- function (e_ij, e_il) {
  
  compreg <- length(e_il)
  
  industries <- nrow(as.matrix(e_il))
  
  s_ij <- vector()
  s_il <- vector()
  
  e_j <- sum (e_ij)
  
  i <- 0
  
  for (i in 1:industries) {
    s_ij[i] <- e_ij[i]/e_j
  }  
  
  j <- 0
  i <- 0
  
  s_il <- matrix(nrow = compreg, ncol = industries)
  e_l <- vector()
  
  for (j in 1:compreg)
  {
    
    for (i in 1:industries) {
      
      e_l[j] <- sum (e_il[,j]) 
      s_il[j,i] <- e_il[i,j]/e_l[j]

    }
    
  }
  
  s_il_mean <- apply(s_il, MARGIN = 2, FUN = mean)

  s_ij_minus_mean <- abs(s_ij-s_il_mean)
  
  K_j <- sum(s_ij_minus_mean)
  
  return(K_j)
  
}