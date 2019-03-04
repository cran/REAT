sigmaconv <-
function (gdp1, time1, gdp2, time2, 
                       sigma.measure = "sd", 
                       sigma.log = TRUE, sigma.weighting = NULL, sigma.norm = FALSE, sigma.issample = FALSE,
                       print.results = FALSE)
{
  
  if (ncol(as.data.frame(gdp1)) > 1)
  {
    stop("First argument gdp1 must consist of one column (one year)", call. = FALSE)
  }
  
  if (ncol(as.data.frame(gdp2)) > 1)
  {
    gdp2 <- gdp2[,(ncol(as.data.frame(gdp2)))]

    warning("Only last time period is regarded", call. = FALSE)  
  }
  
  if (sigma.log == TRUE)
  {
    gdp1 <- log(gdp1)
    gdp2 <- log(gdp2)
  }
  
  if (sigma.measure == "cv") {
    sigma.measure.name = "CV"
    sigma1 <- cv(gdp1, is.sample = sigma.issample, coefnorm = sigma.norm, weighting = sigma.weighting)
    sigma2 <- cv(gdp2, is.sample = sigma.issample, coefnorm = sigma.norm, weighting = sigma.weighting)
  }
  else if (sigma.measure == "var") {
    sigma.measure.name = "Var"
    sigma1 <- var2(gdp1, is.sample = sigma.issample, weighting = sigma.weighting)
    sigma2 <- var2(gdp2, is.sample = sigma.issample, weighting = sigma.weighting)
  }
  else {
    sigma.measure.name = "SD"
    sigma1 <- sd2(gdp1, is.sample = sigma.issample, weighting = sigma.weighting)
    sigma2 <- sd2(gdp2, is.sample = sigma.issample, weighting = sigma.weighting)
  }
  
  sigmaquo <- sigma1/sigma2

  sigmaquo_test <- var.test(gdp1, gdp2)
  

  sigmaconv <- matrix (nrow = 3, ncol = 5)
  
  sigmaconv[1,1] <- sigma1
  sigmaconv[2,1] <- sigma2
  sigmaconv[3,1] <- sigmaquo
  
  sigmaconv[3,2] <- sigmaquo_test$statistic
  sigmaconv[3,3] <- sigmaquo_test$parameter[1]
  sigmaconv[3,4] <- sigmaquo_test$parameter[2]
  sigmaconv[3,5] <- sigmaquo_test$p.value
  
  rownames(sigmaconv) <- c((paste (sigma.measure.name, time1)), (paste (sigma.measure.name, time2)), "Quotient")
  colnames(sigmaconv) <- c("Estimate", "F value", "df1", "df2", "Pr (>F)")
  

  if (print.results == TRUE) {
    cat ("Sigma convergence for two periods (ANOVA)", "\n")
    print (as.data.frame(sigmaconv))
  }
  
  results <- list (sigmaconv = sigmaconv)
  invisible (results)
}
