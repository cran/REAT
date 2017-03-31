rca <-
function (gdp1, time1, gdp2, time2, output = "all", sigma.measure = "cv", sigma.log = TRUE, sigma.norm = FALSE, sigma.weighting = NULL, digs = 5)
{
  
  tinterval <- time2-time1
  
  ln_growth <- log (gdp2/gdp1) 

  ln_initial <- log (gdp1)

  conv_model <- lm (ln_growth ~ ln_initial)

  if (output == "lm") 
  { 
    return(summary(conv_model)) 
  }

  if (output == "data") 
  {
    diff <- gdp2-gdp1
    diff_rel <- diff/gdp1
    transf_data <- data.frame(gdp1, gdp2, diff, diff_rel, ln_growth, ln_initial)
    return(transf_data)
  }

  constant <- round(as.numeric(conv_model$coefficients[1]), digs)

  betaconv <- round(as.numeric(conv_model$coefficients[2]), digs)

  if (betaconv < 0)
  {
    lambda <- round((-log(1+betaconv))/tinterval, digs)  

    halflife <- round(log(2)/lambda, digs)
  }
  else
  {
    lambda <- NA
    halflife <- NA
  }
  
  rsq <- round(as.numeric(summary(conv_model)$r.squared), digs)

  N <- length(gdp1)

  if (sigma.log == TRUE)
  {
    gdp1 <- log(gdp1)
    gdp2 <- log(gdp2)
  }
  
  if (sigma.measure == "sd") {
    sd1 <- sd2(gdp1, is.sample = FALSE, coefnorm = sigma.norm, weighting = sigma.weighting)
    sd2 <- sd2(gdp2, is.sample = FALSE, coefnorm = sigma.norm, weighting = sigma.weighting)
    sigmaconv <- round(sd1-sd2, digs)
  }
  else {
    cv1 <- cv(gdp1, is.sample = FALSE, coefnorm = sigma.norm, weighting = sigma.weighting)
    cv2 <- cv(gdp2, is.sample = FALSE, coefnorm = sigma.norm, weighting = sigma.weighting)
    sigmaconv <- round(cv1-cv2, digs)
  }
  
  return (list(constant=constant, beta=betaconv, tinterval=tinterval, lambda=lambda, halflife=halflife, r.squared=rsq, N=N, sigma=sigmaconv))
  
}
