dalton <- function (x, na.rm = TRUE) {
  
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  
  if (any((x) <= 0)) { return (NA) }

  x_mean <- mean2(x, na.rm = na.rm)
  x_gmean <- mean2(x, na.rm = na.rm, output = "geom")
  
  delta <- log10(x_mean)/log10(x_gmean)

  return(delta)
  
}
