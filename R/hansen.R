hansen <- function (od_dataset, origins, destinations, attrac, dist, 
                    gamma = 1, lambda = -2, atype = "pow", dtype = "pow", 
                    gamma2 = NULL, lambda2 = NULL, dist_const = 0, dist_max = NULL,
                    extract_local = FALSE, accnorm = FALSE, check_df = TRUE,
                    print.results = TRUE)
{
  
  if (check_df == TRUE) 
  {
    if (exists(as.character(substitute(od_dataset)))) { 

      if (attrac != 1) { 
        checkdf(od_dataset, origins, destinations, attrac, dist)
      }
      else
      {
        checkdf(od_dataset, origins, destinations, dist)
      }
    }
    else {
      stop(paste("Dataset", as.character(substitute(od_dataset))), " not found", call. = FALSE)
    }
  }
  
  if (!is.null(dist_max))
  {
    od_dataset <- od_dataset[od_dataset[[dist]]+dist_const <= dist_max+dist_const,]
  }
  
  if (extract_local == TRUE)
  {
    od_dataset <- od_dataset[od_dataset[[origins]] != od_dataset[[destinations]],]  
  }
  
  sort_i_j <- order(od_dataset[[origins]], od_dataset[[destinations]])   

  hansenworkfile <- od_dataset[sort_i_j,]   

  origins_single <- levels(as.factor(hansenworkfile[[origins]]))   

  origins_count <- nlevels(as.factor(hansenworkfile[[origins]]))   

  locations_single <- levels(as.factor(as.character(hansenworkfile[[destinations]])))

  locations_count <- nlevels(as.factor(as.character(hansenworkfile[[destinations]])))

  
  if (attrac == 1) {
    attractivity <- 1
  }
  else { 
    attractivity <- hansenworkfile[[attrac]] 
  }
  
  distance <- dist_const+hansenworkfile[[dist]]
  
  if (atype == "exp") { attrac_w <- exp(gamma*attractivity) }
  else if (atype == "logistic") { attrac_w <- (1/(1+exp(gamma2+gamma*attractivity))) }
  else { 
    atype <- "pow"
    attrac_w <- attractivity^gamma 
  }

  if (dtype == "exp") { dist_w <- exp(lambda*distance) }
  else if (dtype == "logistic") { dist_w <- (1/(1+exp(lambda2+lambda*distance))) }
  else { 
    dtype <- "pow"
    dist_w <- distance^lambda 
  }

  
  U_ij <- attrac_w * dist_w 

  hansenworkfile$U_ij <- U_ij

  accessibility <- vector()
  
  for (i in 1:origins_count) {   

    origin_i <- subset (hansenworkfile, hansenworkfile[[origins]] == origins_single[i])   

    accessibility[i] <- sum(origin_i$U_ij)   
  }
  
  if (accnorm == TRUE)
  {
    if (dtype=="pow") { 
      if (dist_const == 0) { dist_const <- 1 }
      dist_w_c <- dist_const^lambda 
    } 
    if (dtype=="exp") { dist_w_c <- exp(lambda*dist_const) }
    if (dtype=="logistic") { dist_w_c <- 1/(1+exp(lambda2+lambda*dist_const)) }
    
    U_ij_c <- attrac_w * dist_w_c 

    hansenworkfile$U_ij_c <- U_ij_c

    accessibility_c <- vector()
    
    for (i in 1:origins_count) {   

      origin_i <- subset (hansenworkfile, hansenworkfile[[origins]] == origins_single[i])   

      accessibility_c[i] <- sum(origin_i$U_ij_c)   
    }
    accessibility_n <- accessibility/accessibility_c
    accessibility <- data.frame(accessibility, accessibility_c, accessibility_n)
  }
  
  results <- data.frame(origins=origins_single, accessibility)
  colnames(results)[1] <- names(od_dataset[origins])

  
  cat ("Hansen Accessibility", "\n")
  cat ("\n")
  cat ("J =", locations_count, "locations with mean attractivity =", mean(attractivity), "\n")
  cat ("I =", origins_count, "origins with mean transport costs =", mean(distance), "\n")
  
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
  
  if (print.results == TRUE)
  {
    cat ("\n")
    print(as.data.frame(results))
  }
  
  invisible(results)

}