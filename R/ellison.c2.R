ellison.c2 <- function (e_ik, industry, region, e_j = NULL, print.results = TRUE) {

  industry1 <- data.frame(as.character(levels(as.factor(industry))))
  colnames(industry1) <- "industry1"
  industry2 <- data.frame(as.character(levels(as.factor(industry))))
  colnames(industry2) <- "industry2"
  
  industry_comb <- merge (industry1, industry2, all=TRUE)
  industry_comb <- industry_comb[industry_comb$industry1 != industry_comb$industry2,]
  industry_comb$industries <- paste0(as.character(industry_comb$industry1), "-", as.character(industry_comb$industry2))
  
  i <- 0
  
  
  ellison.c_i <- matrix(ncol = 1, nrow = nrow(as.matrix(industry_comb)))
  
  for (i in 1:nrow(as.matrix(industry_comb))) {
    
    i_comb <-  c(as.character(industry_comb[i,]$industry1), as.character(industry_comb[i,]$industry2))
    
    ellison.c_i[i,] <- ellison.c (e_ik, industry, region, e_j, c.industries = i_comb)
    
  }
  
  colnames(ellison.c_i) <- "Gamma c"
  rownames (ellison.c_i) <- industry_comb$industries
  
  if (print.results == TRUE) {
    
    cat("Ellison-Glaeser Co-Agglomeration Index", "\n")
    cat(paste0("K = ", nrow(as.matrix(e_ik)), " firms, I = ", nlevels(as.factor(industry)), " industries, J = ", nlevels(as.factor(region)), " regions"), "\n")
    cat("\n")
    
    gamma_c_df <- as.data.frame(ellison.c_i)
    colnames(gamma_c_df) <- c("Gamma c")
    
    print (gamma_c_df)
  }
  
  invisible(ellison.c_i)
  
}