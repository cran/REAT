sigmaconv.t <-
function (gdp1, time1, gdp2, time2, 
                         sigma.measure = "sd", sigma.log = TRUE, sigma.weighting = NULL, sigma.issample = FALSE,
                         sigma.plot = FALSE, sigma.plotLSize = 1, sigma.plotLineCol = "black", sigma.plotRLine = FALSE, sigma.plotRLineCol = "blue", sigma.Ymin = 0,
                         sigma.plotX = "Time", sigma.plotY = "Variation", sigma.plotTitle = "Sigma convergence",
                         sigma.bgCol = "gray95", sigma.bgrid = TRUE, sigma.bgridCol = "white", sigma.bgridSize = 2, sigma.bgridType = "solid",
                         output.results = FALSE)
{

  gdp2_all <- cbind (gdp1, gdp2)

  
  if (sigma.log == TRUE)
  {
    gdp2_all <- log(gdp2_all)
  }

  no_years <- ncol(gdp2_all)-1
  
  if (sigma.measure == "cv") {
    sigma.measure.name = "CV"
    sigma.years <- apply (gdp2_all[1:(no_years+1)], MARGIN = 2, FUN = cv, is.sample = sigma.issample, weighting = sigma.weighting)
  }
  else if (sigma.measure == "var") {
    sigma.measure.name = "Var"
    sigma.years <- apply (gdp2_all[1:(no_years+1)], MARGIN = 2, FUN = var2, is.sample = sigma.issample, weighting = sigma.weighting)
  }
  else {
    sigma.measure.name = "SD"
    sigma.years <- apply (gdp2_all[1:(no_years+1)], MARGIN = 2, FUN = sd2, is.sample = sigma.issample, weighting = sigma.weighting)
  }
  
  years <- time1:time2
  
  sigma.trend <- data.frame(years, sigma.years)
  
  trendmodel <- lm (sigma.years ~ years, data = sigma.trend)
  
  scestimates <- matrix (nrow = 2, ncol = 4)
  
  trendmodel_rn <- rownames(coef(summary(trendmodel)))
  

  scestimates <- coef(summary(trendmodel))

  rownames(scestimates) <- c("Intercept", "Time")

  scmodelstat <- matrix (nrow = 1, ncol = 5)
  colnames(scmodelstat) <- c("Estimate", "F value", "df 1", "df 2", "Pr (>F)")
  rownames(scmodelstat) <- c("R-Squared")
  
  scmodelstat[1,1] <- as.numeric(summary(trendmodel)$r.squared)

  scmodelstat[1, 2:4] <- summary(trendmodel)$fstatistic[1:3]
  
  scmodelstat[1,5] <- pf(scmodelstat[1,2], scmodelstat[1,3], scmodelstat[1,4], lower.tail = FALSE) 


  if (sigma.plot == TRUE) {
    
    plot(years, sigma.years, cex = sigma.plotLSize, col = sigma.plotLineCol, 
         ylim = c(sigma.Ymin, (max(sigma.years)*1.1)),
         xlab = sigma.plotX, ylab = sigma.plotY, main = sigma.plotTitle)
    
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = sigma.bgCol)

    if (sigma.bgrid == TRUE)
    {
      grid (col = sigma.bgridCol, lty = sigma.bgridType, lwd = sigma.bgridSize)
    } 
    
    lines (years, sigma.years, cex = sigma.plotLSize, col = sigma.plotLineCol)
    
    if (sigma.plotRLine == TRUE) {
      abline(trendmodel, col = sigma.plotRLineCol)
    }
    
  }
  
  if (output.results == TRUE) {
    cat ("Sigma convergence (Trend regression)", "\n")
    print (as.data.frame(scestimates))
    cat ("Model summary", "\n")
    print (as.data.frame(scmodelstat))  
  }
  
  results <- list (sigma.trend = sigma.trend, estimates = scestimates, modelstat = scmodelstat)
  
  invisible(results)

}
