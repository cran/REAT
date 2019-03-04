conc <- function (e_ij, industry.id, region.id, na.rm = TRUE) {
  
  conc_workfile <- data.frame(as.character(region.id), as.character(industry.id), e_ij)
  colnames(conc_workfile) <- c("j_region", "i_industry", "e_ij")
  
  cat("Spatial concentration of industries", "\n")
  
  if (na.rm == TRUE) {
    conc_workfile <- conc_workfile[complete.cases(conc_workfile),]
  }
  
  I <- nlevels(as.factor(as.character(conc_workfile$i_industry)))
  i_names <- as.character(levels(as.factor(conc_workfile$i_industry)))
  J = nlevels(as.factor(as.character(conc_workfile$j_region)))
  j_names <- as.character(levels(as.factor(conc_workfile$j_region)))

  cat(paste0("I = ", I, " industries, J = ", J, " regions"), "\n")
  cat("\n")
  
  
  conc_coefs <- matrix (nrow = I, ncol = 3)
  
  
  e_j <- aggregate(conc_workfile$e_ij, by = list(conc_workfile$j_region), FUN = sum)
  colnames(e_j) <- c("j_region", "e_j")

  conc_workfile <- merge (conc_workfile, e_j, by.x = "j_region", by.y = "j_region")

  

  i <- 0
  
  for (i in 1:I) {
    conc_coefs[i,1] <-  hoover(conc_workfile[conc_workfile$i_industry == i_names[i],]$e_ij,
                               ref = conc_workfile[conc_workfile$i_industry == i_names[i],]$e_j)

  }
  
  

  i <- 0
  
  for (i in 1:I) {
    conc_coefs[i,2] <-  gini.conc(conc_workfile[conc_workfile$i_industry == i_names[i],]$e_ij,
                                  conc_workfile[conc_workfile$i_industry == i_names[i],]$e_j)

  }
  
  

  i <- 0
  
  krugman_mat <- matrix (ncol = I, nrow = J)
  
  for (i in 1:I) {
    krugman_mat[,i] <- conc_workfile[conc_workfile$i_industry == i_names[i],]$e_ij
  }
  
  colnames(krugman_mat) <- i_names
  
  krugman_mat_df <- as.data.frame(krugman_mat)
  rownames(krugman_mat_df) <- j_names
  
  i <- 0
  
  for (i in 1:I) {
    col_i <- which(colnames(krugman_mat_df) == i_names[i])
    conc_coefs[i,3] <- krugman.conc2(krugman_mat_df[,col_i], krugman_mat_df[-col_i])

  }
  
  rownames(conc_coefs) <- i_names
  colnames (conc_coefs) <- c("H i", "G i", "K i")
  
  
  print (conc_coefs)
  
  invisible(conc_coefs)    

}