reilly.lambda <-
function (P_a, P_b, D_a, D_b, B_a_div_B_b) 
{
  
  lambda <- log10(B_a_div_B_b*(P_b/P_a))/log10(D_b/D_a)   
  return(lambda)
}
