krugman.conc2 <-
function (e_ij, e_uj) {
  
  compind <- length(e_uj)
  
  regions <- nrow(e_uj)

  s_ij <- vector()
  s_uj <- vector()
  
  e_j <- sum (e_ij)
  
  i <- 0
  
  for (i in 1:regions) {
    s_ij[i] <- e_ij[i]/e_j
  }  
  
  j <- 0
  i <- 0
  
  s_uj <- matrix(nrow = compind, ncol = regions)
  e_l <- vector()

  for (j in 1:compind)
  {
    
    for (i in 1:regions) {
      
      e_l[j] <- sum (e_uj[,j]) 
      s_uj[j,i] <- e_uj[i,j]/e_l[j]

    }
    
  }
  
  s_uj_mean <- apply(s_uj, MARGIN = 2, FUN = mean)

  s_ij_minus_mean <- abs(s_ij-s_uj_mean)
  
  K_i <- sum(s_ij_minus_mean)
  
  return(K_i)
  
}
