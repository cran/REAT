locq2 <- function (e_ij, industry.id, region.id, LQ.norm = "none", LQ.output = "mat", na.rm = TRUE) {
  

  locq_workfile <- data.frame(as.character(region.id), as.character(industry.id), e_ij)
  colnames(locq_workfile) <- c("j_region", "i_industry", "e_ij")
  
  if (na.rm == TRUE) {
    
    locq_workfile <- locq_workfile[complete.cases(locq_workfile),]
  }
  
  e_i <- aggregate(locq_workfile$e_ij, by = list(locq_workfile$i_industry), FUN = sum)
  colnames(e_i) <- c("i_industry", "e_i")
  e <- sum(locq_workfile$e_ij)

  cat("Location quotients", "\n")
  
  
  I <- nlevels(as.factor(as.character(locq_workfile$i_industry)))
  i_names <- as.character(levels(as.factor(locq_workfile$i_industry)))
  J = nlevels(as.factor(as.character(locq_workfile$j_region)))
  j_names <- as.character(levels(as.factor(locq_workfile$j_region)))

  cat(paste0("I = ", I, " industries, J = ", J, " regions"), "\n")
  cat("\n")
  
  i <- 0
  
  locqs <- matrix(ncol = J, nrow = I)
  
  for (i in 1:J) {

    locq_workfile_j <- locq_workfile[locq_workfile$j_region == j_names[i],]
    locq_workfile_j <- merge (locq_workfile_j, e_i, by.x = "i_industry", by.y = "i_industry", all.x = TRUE)
    locqs[,i] <- locq (locq_workfile_j$e_ij, sum(locq_workfile_j$e_ij), locq_workfile_j$e_i, e)
  }
  
  colnames(locqs) <- j_names
  rownames(locqs) <- i_names
  
  
  if (LQ.norm == "OG") {
    m_LQ_i <- colMeans(locqs)
    sd_LQ_i <- apply(locqs, 1, sd2)
    SLQ <- (locqs-m_LQ_i)/sd_LQ_i
    colnames(SLQ) <- j_names
    rownames(SLQ) <- i_names
    
    locqs <- SLQ
    
  }
  
  if (LQ.norm == "T") {
    locqs_log <- log10(locqs)
    m_LQ_i <- colMeans(locqs_log)
    sd_LQ_i <- apply(locqs_log, 1, sd2)
    SLLQ <- (locqs_log-m_LQ_i)/sd_LQ_i
    colnames(SLLQ) <- j_names
    rownames(SLLQ) <- i_names
    
    locqs <- SLLQ
  }
  
  
  if (LQ.output == "df") {
    i <- 0
    
    locqs_j <- data.frame (matrix(nrow = I, ncol = 3))
    locqs_df <- data.frame (matrix(ncol = 3))
    
    locqs_df[1:I,1] <- colnames(locqs)[1] # oder rep(colnames(locqs[,i]), I) ??
    locqs_df[1:I,2] <- rownames(locqs)
    locqs_df[1:I,3] <- locqs[,1]
    
    for (i in 2:J) {
      

      locqs_j[1:I,1] <- colnames(locqs)[i] # oder rep(colnames(locqs[,i]), I) ??
      locqs_j[1:I,2] <- rownames(locqs)
      locqs_j[1:I,3] <- locqs[,i]
      
      
      locqs_df <- rbind(locqs_df, locqs_j)
      
    }
    colnames(locqs_df) <- c("j_region", "i_industry", "LQ")
    
    print(locqs_df)
    
    invisible(locqs_df) # klappt
    
  } else {
    
    print(locqs)
    
    invisible(locqs)
  }
  
  
}