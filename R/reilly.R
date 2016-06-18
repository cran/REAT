reilly <-
function (P_a, P_b, D_a, D_b, gamma = 1, lambda = 2) 
{
  
  B_a_div_B_b <- ((P_a/P_b)^gamma)*((D_b/D_a))^lambda   
  B_a <- B_a_div_B_b/(1+B_a_div_B_b)   
  B_b <- 1-B_a

  results <- list(relation_AB=B_a_div_B_b, prop_A=B_a, prop_B=B_b)   

  return(results)   
}
