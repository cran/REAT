huff <-
function (huffdataset, origins, locations, attrac, dist,    
          gamma = 1, lambda = -2, atype = "pow", dtype = "pow", 
          gamma2 = NULL, lambda2 = NULL,
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
  }
  
  sort_i_j <- order(huffdataset[[origins]], huffdataset[[locations]])   

  huffworkfile <- huffdataset[sort_i_j,]   

  huffworkfile[[origins]] <- as.factor(as.character(huffworkfile[[origins]]))
  huffworkfile[[locations]] <- as.factor(as.character(huffworkfile[[locations]]))
  
  origins_single <- levels(as.factor(as.character(huffworkfile[[origins]])))

  origins_count <- nlevels(as.factor(as.character(huffworkfile[[origins]])))

  locations_single <- levels(as.factor(as.character(huffworkfile[[locations]])))

  locations_count <- nlevels(as.factor(as.character(huffworkfile[[locations]])))

  if (atype == "exp") { attrac_w <- exp(gamma*huffworkfile[[attrac]]) }
  else if (atype == "logistic") { attrac_w <- (1/(1+exp(gamma2+gamma*huffworkfile[[attrac]]))) }
  else { 
    atype <- "pow"
    attrac_w <- huffworkfile[[attrac]]^gamma 
  }

  if (dtype == "exp") { dist_w <- exp(lambda*huffworkfile[[dist]]) }
  else if (dtype == "logistic") { dist_w <- (1/(1+exp(lambda2+lambda*huffworkfile[[dist]]))) }
  else { 
    dtype <- "pow"
    dist_w <- huffworkfile[[dist]]^lambda 
  }

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

  if (!is.null(localmarket_dataset)) {

    if (check_df == TRUE)
    {
      if (exists(as.character(substitute(localmarket_dataset)))) { 
        checkdf(localmarket_dataset, origin_id, localmarket)
      }
      else {
        stop(paste("Dataset", as.character(substitute(localmarket_dataset))), " not found", call. = FALSE)
      }
      
    }
    
    huffworkfile <- merge (huffworkfile, localmarket_dataset, by.x = colnames(huffworkfile[origins]), by.y = colnames(localmarket_dataset[origin_id]))

    huffworkfile$E_ij <- as.numeric(huffworkfile$p_ij) * as.numeric(huffworkfile[[localmarket]])

    totals <- aggregate (huffworkfile$E_ij, by = list(huffworkfile[[locations]]), FUN = sum, na.rm = TRUE)
    
    colnames (totals) <- c(colnames(huffworkfile[locations]), "T_j")
    
    totals$T_j_share <- totals$T_j/sum(totals$T_j, na.rm = TRUE)   

    results <- list (huffmat = huffworkfile, totals = totals)
  }
  
  else 
  {
    results <- list (ijmatrix = huffworkfile)
  }
  

  cat ("Huff Model", "\n")
  cat ("\n")
  cat ("Summary:", "\n")
  cat (locations_count, "locations with mean attractivity =", mean(huffdataset[[attrac]]), "\n")
  cat (origins_count, "origins with mean transport costs =", mean(huffdataset[[dist]]), "\n")
  
  if (atype == "logistic") {
    cat (paste0("Attractivity weighting (", atype, ") with Gamma1 = ", gamma, " and Gamma2 = ", gamma2), "\n")  
  }
  else { 
    cat (paste0("Attractivity weighting (", atype, ") with Gamma = ", gamma), "\n")
  }
  
  if (dtype == "logistic") {
    cat (paste0("Distance weighting (", dtype, ") with Lambda1 = ", lambda, " and Lambda2 = ", lambda2), "\n")  
  }
  else {
    cat (paste0("Distance weighting (", dtype, ") with Lambda = ", lambda), "\n")  
  }
  
  
  if (!is.null(localmarket_dataset)) { 
    cat ("Mean of total market areas =", mean(totals$T_j), "\n")  
    
  }
  
  cat ("\n")
  cat ("Interaction matrix", "\n")
  print (as.data.frame(huffworkfile))
  
  
  if (!is.null(localmarket_dataset)) {
    cat ("\n")
    cat ("Total market areas", "\n")
    print (as.data.frame(totals))
  } 
  
  
  invisible(results)
  
}