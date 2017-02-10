cv <-
function (x, is.sample = TRUE, coefnorm = FALSE, weighting = NULL, output = "cv")
{ 

if (is.sample == TRUE)
{
  n <- length(x)-1
}
else
{
  n <- length(x)
}
  
if (!is.null(weighting)) 
{
  mean_x <- (sum(x*weighting)/sum(weighting))
  dev_x_sq <- weighting*(x-mean_x)^2
  sd_x <- sqrt(sum(dev_x_sq)/sum(weighting))
}
else
{
  mean_x <- mean(x)
  dev_x_sq <- (x-mean_x)^2
  sd_x <- sqrt(sum(dev_x_sq)/n)
}

if (output == "sd") { return (sd_x) }
if (output == "mean") { return (mean_x) }

v <- sd_x/abs(mean_x)

if (coefnorm == FALSE) {
  return (v)
}   

if (coefnorm == TRUE) {
  v.norm <- v/sqrt(n)
  return(v.norm)
}

}
