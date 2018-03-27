rca <-
function (gdp1, time1, gdp2, time2, conditions = NULL, conditions.formula = NULL, conditions.startval = NULL,
                 beta.estimate = "ols",
                 beta.plot = FALSE, beta.plotPSize = 1, beta.plotPCol = "black", beta.plotLine = FALSE, beta.plotLineCol = "red", 
                 beta.plotX = "Ln (initial)", beta.plotY = "Ln (growth)", beta.plotTitle = "Beta convergence",
                 beta.bgCol = "gray95", beta.bgrid = TRUE, beta.bgridCol = "white", beta.bgridSize = 2, beta.bgridType = "solid",
                 sigma.type = "anova", sigma.measure = "sd", sigma.log = TRUE, sigma.weighting = NULL, sigma.issample = FALSE,
                 sigma.plot = FALSE, sigma.plotLSize = 1, sigma.plotLineCol = "black", sigma.plotRLine = FALSE, sigma.plotRLineCol = "blue", sigma.Ymin = 0,
                 sigma.plotX = "Time", sigma.plotY = "Variation", sigma.plotTitle = "Sigma convergence",
                 sigma.bgCol = "gray95", sigma.bgrid = TRUE, sigma.bgridCol = "white", sigma.bgridSize = 2, sigma.bgridType = "solid")
{
  cat ("Regional Beta and Sigma Convergence")
  cat ("\n", "\n")
  
  if (beta.plot == TRUE) {
    dev.new()
  }
  
  if (beta.estimate == "nls")
  {
    betaconv <- betaconv.nls (gdp1 = gdp1, time1 = time1, gdp2 = gdp2, time2 = time2, conditions = conditions, conditions.formula = conditions.formula, conditions.startval = conditions.startval,
                       beta.plot = beta.plot, beta.plotPSize = beta.plotPSize, beta.plotPCol = beta.plotPCol, beta.plotLine = beta.plotLine, beta.plotLineCol = beta.plotLineCol, 
                       beta.plotX = beta.plotX, beta.plotY = beta.plotY, beta.plotTitle = beta.plotTitle,
                       beta.bgCol = beta.bgCol, beta.bgrid = beta.bgrid, beta.bgridCol = beta.bgridCol, beta.bgridSize = beta.bgridSize, beta.bgridType = beta.bgridType,
                       output.results = TRUE)
    
  }
  
  else {
    betaconv <- betaconv.ols (gdp1 = gdp1, time1 = time1, gdp2 = gdp2, time2 = time2, conditions = conditions, 
                       beta.plot = beta.plot, beta.plotPSize = beta.plotPSize, beta.plotPCol = beta.plotPCol, beta.plotLine = beta.plotLine, beta.plotLineCol = beta.plotLineCol, 
                       beta.plotX = beta.plotX, beta.plotY = beta.plotY, beta.plotTitle = beta.plotTitle,
                       beta.bgCol = beta.bgCol, beta.bgrid = beta.bgrid, beta.bgridCol = beta.bgridCol, beta.bgridSize = beta.bgridSize, beta.bgridType = beta.bgridType,
                       output.results = TRUE)
    
  }
  
  cat ("\n")
  
  if ((sigma.plot == TRUE) && sigma.type == "trend") {
    dev.new()
  }
  
  
  if (sigma.type == "trend") 
  {
    sigmaconv <- sigmaconv.t (gdp1, time1, gdp2, time2, 
                 sigma.measure = sigma.measure, sigma.log = sigma.log, sigma.weighting = sigma.weighting, sigma.issample = sigma.issample,
                 sigma.plot = sigma.plot, sigma.plotLSize = sigma.plotLSize, sigma.plotLineCol = sigma.plotLineCol, sigma.plotRLine = sigma.plotRLine, sigma.plotRLineCol = sigma.plotRLineCol, sigma.Ymin = sigma.Ymin,
                 sigma.plotX = sigma.plotX, sigma.plotY = sigma.plotY, sigma.plotTitle = sigma.plotTitle,
                 sigma.bgCol = sigma.bgCol, sigma.bgrid = sigma.bgrid, sigma.bgridCol = sigma.bgridCol, sigma.bgridSize = sigma.bgridSize, sigma.bgridType = sigma.bgridType,
                 output.results = TRUE)
  } 
  else {
    sigmaconv <- sigmaconv (gdp1, time1, gdp2, time2, 
                 sigma.measure = sigma.measure, sigma.log = sigma.log, sigma.weighting = sigma.weighting, sigma.issample = sigma.issample,
                 output.results = TRUE)
    
  }
  
  results <- list (betaconv = betaconv, sigmaconv = sigmaconv)
  
  invisible(results)
  

}
