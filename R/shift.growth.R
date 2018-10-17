shift.growth <-
function (e_ij1, e_ij2, e_i1, e_i2, industry.names = NULL) 
{
  
  if ((ncol(as.data.frame(e_ij1)) > 1) | (ncol(as.data.frame(e_i1)) > 1))
  {
    stop (paste("Datasets for initial time period must consist of 1 column (= 1 time period)"), call. = FALSE)
  }
  
  industries <- length(e_ij1)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  e_j1 <- sum(e_ij1)
  e_j2 <- sum(e_ij2)
  e1 <- sum(e_i1)
  e2 <- sum(e_i2)

  growthir.abs <- growth (e_ij1, e_ij2, growth.type = "abs") 
  growthin.abs <- growth (e_i1, e_i2, growth.type = "abs") 

  growthir.rel <- growth (e_ij1, e_ij2, growth.type = "rate")
  growthin.rel <- growth (e_i1, e_i2, growth.type = "rate")

  growth <- matrix (ncol = 8, nrow = industries)
  
  growth[,1] <- e_ij1
  
  if ((ncol(as.data.frame(e_ij2)) > 1) | (ncol(as.data.frame(e_i2)) > 1)) {
    growth[,2] <- e_ij2[,ncol(e_ij2)]
  }
  else { growth[,2] <- e_ij2 }
  
  growth[,3] <- growthir.abs
  growth[,4] <- growthir.rel
  growth[,5] <- e_i1
  
  if ((ncol(as.data.frame(e_ij2)) > 1) | (ncol(as.data.frame(e_i2)) > 1)) {
    growth[,6] <- e_i2[,ncol(e_i2)]
  }
  else {  growth[,6] <- e_i2 }
  
  growth[,7] <- growthin.abs
  growth[,8] <- growthin.rel
  
  rownames(growth) <- industry.names
  
  colnames(growth) <- c("e_ij", "e_ij_t1", "e_ij_growth_abs", "e_ij_growth_rel", "e_i", "e_i_t1", "e_i_growth_abs", "e_i_growth_rel")
  
  return(growth)
}
