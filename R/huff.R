huff <-
function (huffdataset, origins, locations, attrac, dist, gamma = 1, lambda = -2, 
                  atype= "pow", dtype= "pow", gamma2 = NULL, lambda2 = NULL, 
                  output_total = FALSE, origindataset, od_localmarket) 
{   
  sort_i_j <- order(huffdataset[[origins]], huffdataset[[locations]])   
  huffworkfile <- huffdataset[sort_i_j,]   

  origins_single <- levels(as.factor(huffworkfile[[origins]]))   
  origins_count <- nlevels(as.factor(huffworkfile[[origins]]))   
  
  locations_single <- levels(as.factor(huffworkfile[[locations]]))   
  locations_count <- nlevels(as.factor(huffworkfile[[locations]]))   
  
  if (atype=="pow") { attrac_w <- huffworkfile[[attrac]]^gamma }
  if (atype=="exp") { attrac_w <- exp(gamma*huffworkfile[[attrac]]) }
  if (atype=="logistic") { attrac_w <- (max(huffworkfile[[attrac]]))/(1+exp(gamma2+gamma*huffworkfile[[attrac]])) }
  
  if (dtype=="pow") { dist_w <- huffworkfile[[dist]]^lambda } 
  if (dtype=="exp") { dist_w <- exp(lambda*huffworkfile[[dist]]) }
  if (dtype=="logistic") { dist_w <- (max(huffworkfile[[dist]]))/(1+exp(lambda2+lambda*huffworkfile[[dist]])) }
  
  U_ij <- attrac_w * dist_w 
  huffworkfile$U_ij <- U_ij
  
  sum_U_ij <- vector()
  sum_U_ij_all <- vector()
  
  for (i in 1:origins_count) {   
    origin_i <- subset (huffworkfile, huffworkfile[[origins]] == origins_single[i])   
    sum_U_ij_i <- sum(origin_i$U_ij)   
  
    for (j in 1:locations_count) {   
      sum_U_ij_all <- rbind(sum_U_ij_all, list(sum_U_ij_i)) 
    }
  }
  
  huffworkfile$sum_U_ij <- sum_U_ij_all   
  
  huffworkfile$p_ij <- as.numeric(huffworkfile$U_ij)/as.numeric(huffworkfile$sum_U_ij)   
  
  if (output_total == TRUE)
  {
    huffworkfile2 <- merge (huffworkfile, origindataset)
    
    huffworkfile2$C_i <- huffworkfile2[[od_localmarket]]
    huffworkfile2$E_ij <- huffworkfile2$p_ij*huffworkfile2$C_i   
  
    sum_E_j <- numeric()
    
    for (j in 1:locations_count) {   
      location_j <- subset (huffworkfile2, huffworkfile2[[locations]] == locations_single[j])   
      sum_E_j[j] <- sum(location_j$E_ij)   
    }
    
    E_j_output <- data.frame(locations_single, sum_E_j)   
    E_j_output$share_j <- E_j_output$sum_E_j/sum(E_j_output$sum_E_j)   
    
    return(E_j_output)
}
  
  else {
  return(huffworkfile)   
  }
}
