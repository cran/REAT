converse <-
function (P_a, P_b, D_ab) 
{
  
  cat ("Converse Breaking-Point Formula", "\n")
  cat ("\n")
  
  B_a <- D_ab/(1+sqrt(P_b/P_a))   
  B_b <- D_ab/(1+sqrt(P_a/P_b))

  results <- list(BP_A = B_a, BP_B = B_b)

  cat("Results:", "\n")
  print(as.data.frame(results))
  
  invisible(results)   
}
