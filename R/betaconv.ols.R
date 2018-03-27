betaconv.ols <-
function (gdp1, time1, gdp2, time2, conditions = NULL, 
                      beta.plot = FALSE, beta.plotPSize = 1, beta.plotPCol = "black", beta.plotLine = FALSE, beta.plotLineCol = "red", 
                      beta.plotX = "Ln (initial)", beta.plotY = "Ln (growth)", beta.plotTitle = "Beta convergence",
                      beta.bgCol = "gray95", beta.bgrid = TRUE, beta.bgridCol = "white", beta.bgridSize = 2, beta.bgridType = "solid",
                      output.results = FALSE)
  {
  tinterval <- time2-time1
  
  N <- length(gdp1)


  gdp2_all <- cbind (gdp1, gdp2)
  
  no_years <- ncol(gdp2_all)-1
  
  growth <- growth(gdp1, gdp2)
  
  ln_growth <- log (growth) 
  ln_initial <- log (gdp1)

  regdata <- data.frame(gdp1, gdp2, growth, ln_initial, ln_growth)

  


  conv_model <- lm (ln_growth ~ ln_initial)

  alpha <- coef(summary(conv_model))[1, 1]

  beta <- coef(summary(conv_model))[2, 1]


  speed <- betaconv.speed(beta = beta, tinterval = tinterval, output.results = FALSE)
  lambda <- speed[1]
  halflife <- speed[2]
  
  


  bestimates <- matrix (nrow = 4, ncol = 4)
  rownames(bestimates) <- c("Alpha", "Beta", "Lambda", "Halflife")
  colnames(bestimates) <- c("Estimate", "Std. Error", "t value", "Pr (>|t|)")
  
  bestimates[1:2,] <- coef(summary(conv_model))

  bestimates[3,1] <- lambda
  
  bestimates[4,1] <- halflife
  
  

  bmodelstat <- matrix (nrow = 1, ncol = 5)
  colnames(bmodelstat) <- c("Estimate", "F value", "df 1", "df 2", "Pr (>F)")
  rownames(bmodelstat) <- c("R-Squared")
  
  bmodelstat[1,1] <- as.numeric(summary(conv_model)$r.squared)

  bmodelstat[1, 2:4] <- summary(conv_model)$fstatistic[1:3]
  
  bmodelstat[1,5] <- pf(bmodelstat[1,2], bmodelstat[1,3], bmodelstat[1,4], lower.tail = FALSE) 

  abeta = list(estimates = bestimates, modelstat = bmodelstat)
  
  results <- list(regdata = regdata, tinterval = tinterval, abeta = abeta)
  
  
  


  if (!is.null(conditions)) {
    
    cond <- as.data.frame(list(conditions))
    cond_names <- colnames(cond)

    model_conditions <- paste(cond_names, collapse="+")
    bcmodelc <- as.formula (paste("ln_growth ~ ln_initial +", model_conditions))

    conv_model_c <- lm (bcmodelc, data = cond)


    alpha <- coef(summary(conv_model_c))[1, 1]

    beta <- coef(summary(conv_model_c))[2, 1]


    speed <- betaconv.speed(beta = beta, tinterval = tinterval, output.results = FALSE)
    lambda <- speed[1]
    halflife <- speed[2]
    
    
    

    bcestimates <- matrix (nrow = 2+nrow(coef(summary(conv_model_c))), ncol = 4)
    
    conv_model_c_rn <- rownames(coef(summary(conv_model_c)))
    
    rownames(bcestimates) <- c("Alpha", "Beta", conv_model_c_rn[3:length(conv_model_c_rn)], "Lambda", "Halflife")
    
    colnames(bcestimates) <- c("Estimate", "Std. Error", "t value", "Pr (>|t|)")
    
    bcestimates[1:nrow(coef(summary(conv_model_c))),] <- coef(summary(conv_model_c))

    bcestimates[1+nrow(coef(summary(conv_model_c))),1] <- lambda
    
    bcestimates[2+nrow(coef(summary(conv_model_c))),1] <- halflife
    
    
    bcmodelstat <- matrix (nrow = 1, ncol = 5)
    colnames(bcmodelstat) <- c("Estimate", "F value", "df 1", "df 2", "Pr (>F)")
    rownames(bcmodelstat) <- c("R-Squared")
    
    bcmodelstat[1,1] <- as.numeric(summary(conv_model_c)$r.squared)

    bcmodelstat[1, 2:4] <- summary(conv_model_c)$fstatistic[1:3]
    
    bcmodelstat[1,5] <- pf(bcmodelstat[1,2], bcmodelstat[1,3], bcmodelstat[1,4], lower.tail = FALSE) 

    cbeta = list(estimates = bcestimates, modelstat = bcmodelstat)
    
    results <- cbind(results$regdata, conditions)
    
    results <- list(regdata = regdata, tinterval = tinterval, abeta = abeta, cbeta = cbeta)
  }
  

  if (output.results == TRUE) {
    cat ("Absolute Beta Convergence", "\n")
    cat ((paste0("Model coefficients (Estimation method: OLS)", "\n")))
    print (as.data.frame(bestimates))
    
    cat ("Model summary", "\n")
    print (as.data.frame(bmodelstat))
    
    
    if (!is.null(conditions)) {
      cat ("\n")
      cat ("Conditional Beta Convergence", "\n")
      cat ((paste0("Model coefficients (Estimation method: OLS)")), "\n")
      print (as.data.frame(bcestimates))
      
      cat ("Model summary", "\n")
      print (as.data.frame(bcmodelstat))
      
    }
    
  }

  if (beta.plot == TRUE) {
    
    plot(ln_initial, ln_growth, type = "p", xlab = beta.plotX, ylab = beta.plotY, main = beta.plotTitle)

    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = beta.bgCol)

    if (beta.bgrid == TRUE)
    {
      grid (col = beta.bgridCol, lty = beta.bgridType, lwd = beta.bgridSize)
    } 
    
    points (ln_initial, ln_growth, pch = 16, col = beta.plotPCol, cex = beta.plotPSize)
    
    if (beta.plotLine == TRUE) {
      abline(conv_model, col = beta.plotLineCol)
    }
    
  }
  
  invisible (results)
}
