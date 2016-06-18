hansen <-
function (od_dataset, origins, destinations, attrac, dist, gamma = 1, 
                    lambda = -2, atype= "pow", dtype= "pow", gamma2 = NULL, lambda2 = NULL)
{
  sort_i_j <- order(od_dataset[[origins]], od_dataset[[destinations]])   

  hansenworkfile <- od_dataset[sort_i_j,]   

  origins_single <- levels(as.factor(hansenworkfile[[origins]]))   
  origins_count <- nlevels(as.factor(hansenworkfile[[origins]]))   

  if (attrac == 1) {
    attractivity <- 1
  }
  
  attractivity <- hansenworkfile[[attrac]]
  distance <- hansenworkfile[[dist]]
  
  if (atype=="pow") { attrac_w <- attractivity^gamma }
  if (atype=="exp") { attrac_w <- exp(gamma*attractivity) }
  if (atype=="logistic") { attrac_w <- (max(attractivity))/(1+exp(gamma2+gamma*attractivity)) }
  
  if (dtype=="pow") { dist_w <- distance^lambda } 
  if (dtype=="exp") { dist_w <- exp(lambda*distance) }
  if (dtype=="logistic") { dist_w <- (max(distance))/(1+exp(lambda2+lambda*distance)) }
  
  U_ij <- attrac_w * dist_w 

  hansenworkfile$U_ij <- U_ij

  accessibility <- vector()
  
  for (i in 1:origins_count) {   
    origin_i <- subset (hansenworkfile, hansenworkfile[[origins]] == origins_single[i])   
    accessibility[i] <- sum(origin_i$U_ij)   
  }
  
  results <- data.frame(origins=origins_single, accessibility)
  
  return(results)
}
