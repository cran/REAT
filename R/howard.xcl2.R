howard.xcl2 <- function (k, industry, region, print.results = TRUE) {

  industry1 <- data.frame(as.character(levels(as.factor(industry))))
  colnames(industry1) <- "industry1"
  industry2 <- data.frame(as.character(levels(as.factor(industry))))
  colnames(industry2) <- "industry2"
  
  industry_comb <- merge (industry1, industry2, all = TRUE)
  industry_comb <- industry_comb[industry_comb$industry1 != industry_comb$industry2,]
  industry_comb$industries <- paste0(as.character(industry_comb$industry1), "-", as.character(industry_comb$industry2))
  
  i <- 0
  
  howard.xcl_i <- matrix(ncol = 1, nrow = nrow(as.matrix(industry_comb)))
  
  for (i in 1:nrow(as.matrix(industry_comb))) {
    
    howard.xcl_i[i,] <- howard.xcl (k, industry, region, industry1 = industry_comb$industry1[i], industry2 = industry_comb$industry2[i])
    
  }
  
  colnames(howard.xcl_i) <- "XCL"
  rownames (howard.xcl_i) <- industry_comb$industries
  
  if (print.results == TRUE) {
    
    cat("Howard-Newman-Tarp Excess Colocation Index", "\n")
    cat(paste0("K = ", nrow(as.matrix(k)), " firms, I = ", nlevels(as.factor(industry)), " industries, J = ", nlevels(as.factor(region)), " regions"), "\n")
    cat("\n")
    
    xcl_df <- as.data.frame(howard.xcl_i)
    colnames(xcl_df) <- c("XCL")
    
    print (xcl_df)
  }
  
  invisible(howard.xcl_i)
  
}