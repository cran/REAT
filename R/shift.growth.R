shift.growth <-
function (region1, region2, nation1, nation2, industry.names = NULL) 
{
  
  if ((ncol(as.data.frame(region1)) > 1) | (ncol(as.data.frame(nation1)) > 1))
  {
    stop (paste("Datasets for initial time period must consist of 1 column (= 1 time period)"), call. = FALSE)
  }
  
  industries <- length(region1)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  sum.region1 <- sum(region1)
  sum.region2 <- sum(region2)
  sum.nation1 <- sum(nation1)
  sum.nation2 <- sum(nation2)

  growthir.abs <- growth (region1, region2, growth.type = "abs") 
  growthin.abs <- growth (nation1, nation2, growth.type = "abs") 

  growthir.rel <- growth (region1, region2, growth.type = "rate")
  growthin.rel <- growth (nation1, nation2, growth.type = "rate")

  if ((ncol(as.data.frame(region2)) > 1) | (ncol(as.data.frame(nation2)) > 1)) {
    growth <- data.frame(industry.names, region1, region2[,ncol(region2)], growthir.abs, growthir.rel, nation1, nation2[,ncol(nation2)], growthin.abs, growthin.rel)
  }
  else {
    growth <- data.frame(industry.names, region1, region2, growthir.abs, growthir.rel, nation1, nation2, growthin.abs, growthin.rel)  
  }
  
  colnames(growth) <- c("Industry", "Region t", "Region t+1", "R growth", "R growth %", "Nation t", "Nation t+1", "N growth", "N growth %")
  
  return(growth)
}
