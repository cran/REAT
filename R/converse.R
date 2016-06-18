converse <-
function (P_a, P_b, D_ab) 
{
  
  B_a <- D_ab/(1+sqrt(P_b/P_a))   
  B_b <- D_ab/(1+sqrt(P_a/P_b))

  results <- list(B_a=B_a,B_b=B_b)
  
  return(results)
}
