conc <- 
function(x, na.rm = TRUE) {
  
  x_ncol <- ncol(as.matrix(x))
  
  conc_results <- matrix (ncol = x_ncol, nrow = 7)

  i <- 0
  
  for (i in 1:x_ncol) {
    conc_results[1,i] <- gini (as.matrix(x)[,i], na.rm = na.rm)
    conc_results[2,i] <- gini (as.matrix(x)[,i], coefnorm = TRUE, na.rm = na.rm)
    conc_results[3,i] <- herf (as.matrix(x)[,i], coefnorm = FALSE, na.rm = na.rm)
    conc_results[4,i] <- herf (as.matrix(x)[,i], coefnorm = TRUE, na.rm = na.rm)
    conc_results[5,i] <- herf (as.matrix(x)[,i], output = "eq", na.rm = na.rm)
    conc_results[6,i] <- hoover (as.matrix(x)[,i], na.rm = na.rm)
    conc_results[7,i] <- theil (as.matrix(x)[,i], na.rm = na.rm)
  }
  
  rownames(conc_results) <- c("Gini", "Gini norm.", "HHI", "HHI norm.", "HHI eq.", "Hoover", "Theil")
  colnames(conc_results) <- colnames(x)
  
  return(conc_results)
}
