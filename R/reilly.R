reilly <-
function (P_a, P_b, D_a, D_b, gamma = 1, lambda = 2, relation = NULL) 
{
  
  cat ("Reilly's Law of Retail Gravitation", "\n")
  cat ("\n")
  
  if (is.null(lambda))
  {
    if (is.null(relation)) { stop(paste("No relation stated", call. = FALSE)) }
    
    lambda <- log10(relation*(P_b/P_a))/log10(D_b/D_a)   

    cat(paste0("Lambda = ", lambda), "\n")
    
    invisible(lambda)
  }
  
  else
  {
    if (is.null(lambda)) { stop(paste("No Lambda stated", call. = FALSE)) }
    
    relation <- ((P_a/P_b)^gamma)*((D_b/D_a))^lambda   

    B_a <- relation/(1+relation)   

    B_b <- 1-B_a

    results <- list(relation_AB = relation, prop_A = B_a, prop_B = B_b)   

    cat("Results:", "\n")
    print(as.data.frame(results))
    
    invisible(results)   
  }
  
}
