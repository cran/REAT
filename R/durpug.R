durpug <- function (e_ij, e_i) {
  
  e_j <- sum(e_ij)
  e <- sum (e_i)
  
  s_ij <- e_ij/e_j
  s_i <- e_i/e
  
  RDI <- 1/(sum(abs(s_ij-s_i)))
  
  return(RDI)
  
}