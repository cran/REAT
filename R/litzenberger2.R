litzenberger2 <- function (e_ij, industry.id, region.id, a_j, p_j, b_ij, CI.output = "mat", na.rm = TRUE) {
  

  litz_workfile <- data.frame(as.character(region.id), as.character(industry.id), b_ij, e_ij, p_j, a_j)
  colnames(litz_workfile) <- c("j_region", "i_industry", "b_ij", "e_ij", "p_j", "a_j")

  if (na.rm == TRUE) {
    
    litz_workfile <- litz_workfile[complete.cases(litz_workfile),]
  }
  
  
  e_i <- aggregate(litz_workfile$e_ij, by = list(litz_workfile$i_industry), FUN = sum)
  colnames(e_i) <- c("i_industry", "e_i")

  b_i <- aggregate(litz_workfile$b_ij, by = list(litz_workfile$i_industry), FUN = sum)
  colnames(b_i) <- c("i_industry", "b_i")

  
  a_j <- aggregate(litz_workfile$a_j, by = list(litz_workfile$j_region), FUN = mean)
  colnames(a_j) <- c("j_region", "a_j")
  a <- sum(a_j[,2])

  p_j <- aggregate(litz_workfile$p_j, by = list(litz_workfile$j_region), FUN = mean)
  colnames(p_j) <- c("j_region", "p_j")
  p <- sum(p_j[,2])

  
  cat("Litzenberger-Sternberg cluster indices", "\n")
  
  
  I <- nlevels(as.factor(as.character(litz_workfile$i_industry)))
  i_names <- as.character(levels(as.factor(litz_workfile$i_industry)))
  J = nlevels(as.factor(as.character(litz_workfile$j_region)))
  j_names <- as.character(levels(as.factor(litz_workfile$j_region)))

  cat(paste0("I = ", I, " industries, J = ", J, " regions"), "\n")
  cat("\n")
  
  i <- 0
  
  litzs <- matrix(ncol = J, nrow = I)
  
  for (i in 1:J) {

    litz_workfile_j <- litz_workfile[litz_workfile$j_region == j_names[i],]
    litz_workfile_j <- merge (litz_workfile_j, e_i, by.x = "i_industry", by.y = "i_industry", all.x = TRUE)
    litz_workfile_j <- merge (litz_workfile_j, b_i, by.x = "i_industry", by.y = "i_industry", all.x = TRUE)

    
    litzs[,i] <- litzenberger (litz_workfile_j$e_ij, litz_workfile_j$e_i, 
                               a_j[a_j$j_region == j_names[i],][,2], a,
                               p_j[p_j$j_region == j_names[i],][,2], p,
                               litz_workfile_j$b_ij, litz_workfile_j$b_i)

    
  }
  
  colnames(litzs) <- j_names
  rownames(litzs) <- i_names
  
  
  if (CI.output == "df") {
    i <- 0
    
    litzs_j <- data.frame (matrix(nrow = I, ncol = 3))
    litzs_df <- data.frame (matrix(ncol = 3))
    
    litzs_df[1:I,1] <- colnames(litzs)[1] # oder rep(colnames(locqs[,i]), I) ??
    litzs_df[1:I,2] <- rownames(litzs)
    litzs_df[1:I,3] <- litzs[,1]
    
    for (i in 2:J) {
      
      litzs_j[1:I,1] <- colnames(litzs)[i] 
      litzs_j[1:I,2] <- rownames(litzs)
      litzs_j[1:I,3] <- litzs[,i]
      
      
      litzs_df <- rbind(litzs_df, litzs_j)
      
    }
    colnames(litzs_df) <- c("j_region", "i_industry", "CI")
    
    print(litzs_df)
    
    invisible(litzs_df) 
    
  } else {
    
    print(litzs)
    
    invisible(litzs)
  }
  
}