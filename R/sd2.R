sd2 <-
function (x, is.sample = TRUE, weighting = NULL, wmean = FALSE, na.rm = FALSE)
{
  var_x <- var2(x, is.sample = is.sample, weighting = weighting, wmean = wmean, na.rm = na.rm)
  sd_x <- sqrt(var_x)

  return(sd_x)
}
