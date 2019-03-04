litzenberger <- function (e_ij, e_i, a_j, a, p_j, p, b_ij, b_i) {

  conc_i <- (e_ij/a_j)/(e_i/a)
  spec_j <- (e_ij/p_j)/(e_i/p)
  size_i <- (e_ij/b_ij)/(e_i/b_i)
  
  CI_ij <- conc_i*spec_j/size_i

  return(CI_ij)
  
}