krugman.conc <- function (e_ij, e_uj) {

  regions <- nrow(as.matrix(e_ij))

  s_ij <- vector()
  s_il <- vector()
  
  e_j <- sum (e_ij)
  e_l <- sum (e_uj)

  for (i in 1:regions) {
    s_ij[i] <- e_ij[i]/e_j
    s_il[i] <- e_uj[i]/e_l
  }
  
  K_jl <- sum(abs(s_ij-s_il))

  return(K_jl)
  
}