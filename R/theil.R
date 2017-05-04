theil <-
function (x)
{
  x_mean <- mean(x)

  ln_mean <- log (x_mean/x)

  TE <- mean (ln_mean)

  return (TE)
}
