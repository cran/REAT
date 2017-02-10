disp <-
function (x) {
  H <- herf(x)
  H.norm <- herf(x, coefnorm = TRUE)
  H.eq <- herf(x, output = "eq")
  G <- gini(x)
  G.norm <- gini(x, coefnorm = TRUE)
  cv <- cv(x)
  cv.norm <- cv(x, coefnorm = TRUE)
  
  dispvalues <- list(HHI=H, HHIn=H.norm, HHIeq=H.eq, GINI=G, GINIn=G.norm, CV=cv, CVn=cv.norm)
 
  return (dispvalues)
}
