cv <-
function (x, coefnorm = FALSE)  

{ v <- sd(x)/mean(x);  

v.norm <- v/sqrt(length(x))

if (coefnorm == FALSE) {
  return (v)
}   

if (coefnorm == TRUE) {
  return(v.norm)
}

}
