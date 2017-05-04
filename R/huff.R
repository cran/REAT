huff <-
function (huffdataset, origins, locations, attrac, dist,    
                         gamma = 1, lambda = -2, atype= "pow", dtype = "pow", 
                         gamma2 = NULL, lambda2 = NULL, output = "shares",
                         localmarket_dataset = NULL, origin_id = NULL, localmarket = NULL, 
                         check_df = TRUE)
{   
  
  if (check_df == TRUE)
  {
    if (exists(as.character(substitute(huffdataset)))) { 
      checkdf(huffdataset, origins, locations, attrac, dist)
    }
    else {
      stop(paste("Dataset", as.character(substitute(huffdataset))), " not found", call. = FALSE)
    }

    if (output == "total")
    {
      if (exists(as.character(substitute(localmarket_dataset)))) { 
        checkdf(localmarket_dataset, origin_id, localmarket)
      }
      else {
        stop(paste("Dataset", as.character(substitute(localmarket_dataset))), " not found", call. = FALSE)
      }
      
    }
   
  }
  
  sort_i_j <- order(huffdataset[[origins]], huffdataset[[locations]])   

  huffworkfile <- huffdataset[sort_i_j,]   

  huffworkfile[[origins]] <- as.factor(as.character(huffworkfile[[origins]]))
  huffworkfile[[locations]] <- as.factor(as.character(huffworkfile[[locations]]))
  
  origins_single <- levels(as.factor(as.character(huffworkfile[[origins]])))

  origins_count <- nlevels(as.factor(as.character(huffworkfile[[origins]])))

  locations_single <- levels(as.factor(as.character(huffworkfile[[locations]])))

  locations_count <- nlevels(as.factor(as.character(huffworkfile[[locations]])))

  if (atype=="pow") { attrac_w <- huffworkfile[[attrac]]^gamma }
  if (atype=="exp") { attrac_w <- exp(gamma*huffworkfile[[attrac]]) }
  if (atype=="logistic") { attrac_w <- (1/(1+exp(gamma2+gamma*huffworkfile[[attrac]]))) }
  
  if (dtype=="pow") { dist_w <- huffworkfile[[dist]]^lambda } 
  if (dtype=="exp") { dist_w <- exp(lambda*huffworkfile[[dist]]) }
  if (dtype=="logistic") { dist_w <- (1/(1+exp(lambda2+lambda*huffworkfile[[dist]]))) }
  
  U_ij <- attrac_w * dist_w 
 
  huffworkfile$U_ij <- as.numeric(U_ij)

  sum_U_ij <- vector()
  sum_U_ij_all <- vector()
  
  for (i in 1:origins_count) {   

    origin_i <- subset (huffworkfile, as.factor(as.character(huffworkfile[[origins]])) == origins_single[i])   

    sum_U_ij_i <- sum(as.numeric(origin_i$U_ij))

    for (j in 1:locations_count) {   
 
      sum_U_ij_all <- rbind(sum_U_ij_all, list(sum_U_ij_i)) 
    }
  }
  
  huffworkfile$sum_U_ij <- sum_U_ij_all   
  huffworkfile$sum_U_ij <- as.numeric(unlist(huffworkfile$sum_U_ij))   
  
  huffworkfile$p_ij <- as.numeric(huffworkfile$U_ij)/as.numeric(unlist(huffworkfile$sum_U_ij))

  if (output == "total")
  {
    huffworkfile <- merge (huffworkfile, localmarket_dataset, by.x = colnames(huffworkfile[origins]), by.y = colnames(localmarket_dataset[origin_id]))

    huffworkfile$E_ij <- as.numeric(huffworkfile$p_ij) * as.numeric(huffworkfile[[localmarket]])

    sum_E_j <- vector()

    for (j in 1:locations_count) {   
      location_j <- subset (huffworkfile, huffworkfile[[locations]] == locations_single[j])   
      sum_E_j[j] <- sum(as.numeric(location_j$E_ij), na.rm = TRUE)   
    }
    
    E_j_output <- data.frame(locations_single, sum_E_j)   
    E_j_output$share_j <- E_j_output$sum_E_j/sum(E_j_output$sum_E_j, na.rm = TRUE)   

    return(E_j_output)
  }
  
  else
  {
    return(huffworkfile)   
  }
  
}
