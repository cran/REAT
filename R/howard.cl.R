howard.cl <- function(k, industry, region, industry1, industry2, e_k = NULL) {
  
  if (!is.null(e_k)) {
    howardworkfile <- data.frame(k, industry, region, e_k)
  }
  else {
    howardworkfile <- data.frame(k, industry, region)
  }
  
  
  howardworkfile <- howardworkfile[(howardworkfile$industry == industry1) | (howardworkfile$industry == industry2),]
  
  i_names <- as.character(levels(as.factor(howardworkfile$industry)))
  
  K_i <- nrow(howardworkfile[howardworkfile$industry == industry1,])
  K_q <- nrow(howardworkfile[howardworkfile$industry == industry2,])
  
  if (!is.null(e_k)) {
    KixKq <- sum(howardworkfile$e_k)
  }
  else {
    KixKq <- K_i*K_q
    
  }
  
  k_i <- howardworkfile[howardworkfile$industry == industry1,]
  k_q <- howardworkfile[howardworkfile$industry == industry2,]
  k_mat <- merge (k_i$k, k_q$k, all = TRUE)
  
  colnames(k_mat) <- c("k1", "k2")
  
  k_mat <- merge (k_mat, howardworkfile, by.x = "k1", by.y = "k")
  
  colnames(k_mat) <- c("k1", "k2", "k1_i", "k1_r")
  
  k_mat <- merge (k_mat, howardworkfile, by.x = "k2", by.y = "k")
  colnames(k_mat) <- c("k1", "k2", "k1_i", "k1_r", "k2_i", "k2_r")
  
  k_mat$C_ij <- 0
  
  i <- 0
  
  for (i in 1:nrow(k_mat)) {
    
    if (k_mat[i,]$k1_r == k_mat[i,]$k2_r) { 
      k_mat[i,]$C_ij <- 1 
    }
    else {
      k_mat[i,]$C_ij <- 0
    }
    
  }
  
  if (!is.null(e_k)) {
    sum_Cij_i <- aggregate(k_mat[k_mat$C_ij == 1,]$e_k, by = list(k_mat[k_mat$C_ij == 1,]$k1_i), FUN = sum)
  }
  else {
    sum_Cij_i <- aggregate(k_mat$C_ij, by = list(k_mat$k1_i), FUN = sum)
  }
  
  CL <- sum_Cij_i[1,2]/KixKq
  
  return(CL)
  
}