theil <-
function (x, na.rm = TRUE)
{
  
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  
  x_mean <- mean2(x, na.rm = na.rm)

  ln_mean <- log (x_mean/x)

  TE <- mean2 (ln_mean, na.rm = na.rm)

  return (TE)
}
