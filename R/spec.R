spec <- function (e_ij, industry.id, region.id, na.rm = TRUE) {
  
  conc_workfile <- data.frame(as.character(region.id), as.character(industry.id), e_ij)
  colnames(conc_workfile) <- c("j_region", "i_industry", "e_ij")
  
  cat("Specialization of regions", "\n")
  
  if (na.rm == TRUE) {
    conc_workfile <- conc_workfile[complete.cases(conc_workfile),]
  }
  
  I <- nlevels(as.factor(as.character(conc_workfile$i_industry)))
  i_names <- as.character(levels(as.factor(conc_workfile$i_industry)))
  J = nlevels(as.factor(as.character(conc_workfile$j_region)))
  j_names <- as.character(levels(as.factor(conc_workfile$j_region)))

  cat(paste0("I = ", I, " industries, J = ", J, " regions"), "\n")
  cat("\n")
  
  
  conc_coefs <- matrix (nrow = J, ncol = 3)
  
  
  e_i <- aggregate(conc_workfile$e_ij, by = list(conc_workfile$i_industry), FUN = sum)
  colnames(e_i) <- c("i_industry", "e_i")

  conc_workfile <- merge (conc_workfile, e_i, by.x = "i_industry", by.y = "i_industry")

  

  i <- 0
  
  for (i in 1:J) {
    conc_coefs[i,1] <-  hoover(conc_workfile[conc_workfile$j_region == j_names[i],]$e_ij,
                               ref = conc_workfile[conc_workfile$j_region == j_names[i],]$e_i)

  }
  
  

  i <- 0
  
  for (i in 1:J) {
    conc_coefs[i,2] <-  gini.conc(conc_workfile[conc_workfile$j_region == j_names[i],]$e_ij,
                                  conc_workfile[conc_workfile$j_region == j_names[i],]$e_i)

  }
  
  

  i <- 0
  
  krugman_mat <- matrix (ncol = J, nrow = I)
  
  for (i in 1:J) {
    krugman_mat[,i] <- conc_workfile[conc_workfile$j_region == j_names[i],]$e_ij
  }
  
  colnames(krugman_mat) <- j_names
  
  krugman_mat_df <- as.data.frame(krugman_mat)
  rownames(krugman_mat_df) <- i_names
  
  i <- 0
  
  for (i in 1:J) {
    col_i <- which(colnames(krugman_mat_df) == j_names[i])
    conc_coefs[i,3] <- krugman.spec2(krugman_mat_df[,col_i], krugman_mat_df[-col_i])

  }
  
  rownames(conc_coefs) <- j_names
  colnames (conc_coefs) <- c("H j", "G j", "K j")
  
  
  print (conc_coefs)
  
  invisible(conc_coefs)    

}