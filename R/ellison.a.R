ellison.a <- function (e_ik, e_j, regions, print.results = TRUE) {

  
  k <- 1:nrow(as.matrix((e_ik)))

  K <- nrow(as.matrix((e_ik)))

  J <- length(levels(as.factor(regions)))

  j_names <- levels(as.factor(regions))

  
  j_names <- factor(regions, levels = unique(regions))
  j_names <- levels(j_names)

  ellisonworkfile <- data.frame (k, regions, e_ik)

  
  i <- 0
  
  e_ij <- vector()
  
  for (i in 1:J)
  {
    e_ij[i] <- sum (ellisonworkfile[ellisonworkfile$regions == j_names[i],]$e_ik)

  }
  
  e_i <- sum (e_ik)

  e <- sum (e_j)

  s_ij <- e_ij/e_i

  s_j <- e_j/e

  G_i <- sum ((s_ij-s_j)^2)

  
  H_i <- herf(e_ik)

  sum_s_j2 <- sum(s_j^2)
  
  gamma_i <- (G_i-(1-sum_s_j2)*H_i)/((1-sum_s_j2)*(1-H_i))

  
  H_i_sq <- H_i^2


  sum_sj3 <- sum(s_j^3)

  sum_s_j2_sq <- sum_s_j2^2

  sum_zik4 <- sum((e_ik/e_i)^4)

  var_Gi <- 2*(H_i_sq*(sum_s_j2-2*sum_sj3+sum_s_j2_sq)-(sum_zik4*(sum_s_j2-4*sum_sj3+3*sum_s_j2_sq)))

  z_Gi <- (G_i-(1-sum_s_j2)*H_i)/sqrt(var_Gi)

  
  results <- matrix(ncol = 5, nrow = 1)
  
  results[,1] <- gamma_i
  results[,2] <- G_i
  results[,3] <- z_Gi
  results[,4] <- K
  results[,5] <- H_i
  colnames(results) <- c("Gamma i", "G i", "z Gi", "K i", "HHI i")
  rownames(results) <- 1
  
  if (print.results == TRUE) {
    
    print(gamma_i)
    
  }
  
  invisible(results)
  
  
}