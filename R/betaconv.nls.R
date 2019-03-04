betaconv.nls <- function (gdp1, time1, gdp2, time2, conditions = NULL, conditions.formula = NULL, conditions.startval = NULL,
                          beta.plot = FALSE, beta.plotPSize = 1, beta.plotPCol = "black", beta.plotLine = FALSE, beta.plotLineCol = "red", 
                          beta.plotX = "Ln (initial)", beta.plotY = "Ln (growth)", beta.plotTitle = "Beta convergence",
                          beta.bgCol = "gray95", beta.bgrid = TRUE, beta.bgridCol = "white", beta.bgridSize = 2, beta.bgridType = "solid",
                          print.results = TRUE)
{
  

  conv_model_lin <- betaconv.ols (gdp1 = gdp1, time1 = time1, gdp2 = gdp2, time2 = time2, conditions = NULL,
                                  beta.plot = beta.plot, beta.plotPSize = beta.plotPSize, beta.plotPCol = beta.plotPCol,
                                  beta.plotLine = beta.plotLine, beta.plotLineCol = beta.plotLineCol, 
                                  beta.plotX = beta.plotX, beta.plotY = beta.plotY, beta.plotTitle = beta.plotTitle,
                                  beta.bgCol = beta.bgCol, beta.bgrid = beta.bgrid, beta.bgridCol = beta.bgridCol, 
                                  beta.bgridSize = beta.bgridSize, beta.bgridType = beta.bgridType,
                                  print.results = FALSE)
  
  ln_growth <- conv_model_lin$regdata$ln_growth
  ln_initial <- conv_model_lin$regdata$ln_initial
  tinterval <- conv_model_lin$tinterval

  alpha_ols <- conv_model_lin$abeta$estimates[1,1]
  beta_ols <- conv_model_lin$abeta$estimates[2,1]



  conv_model <- nls(ln_growth ~ alphan-((1-exp(-betan*tinterval))/tinterval)*ln_initial, start = c(alphan = alpha_ols, betan = -beta_ols))

  alpha <- coef(summary(conv_model))[1, 1]
  beta <- coef(summary(conv_model))[2, 1]

  

  speed <- betaconv.speed(beta = -beta, tinterval = tinterval, print.results = FALSE)
  lambda <- speed[1]
  halflife <- speed[2]
  
  


  bestimates <- matrix (nrow = 4, ncol = 4)
  rownames(bestimates) <- c("Alpha", "Beta", "Lambda", "Halflife")
  colnames(bestimates) <- c("Estimate", "Std. Error", "t value", "Pr (>|t|)")
  
  bestimates[1:2,] <- coef(summary(conv_model))

  bestimates[3,1] <- lambda
  
  bestimates[4,1] <- halflife
  
  abeta = list(estimates = bestimates)
  
  results <- list (regdata = conv_model_lin$regdata, abeta = abeta)
  


  if (!is.null(conditions)) {
    
    cond <- as.data.frame(list(conditions))
    cond_names <- colnames(cond)


    bcmodelc <- as.formula (paste("ln_growth ~ alphan-((1-exp(-betan*tinterval))/tinterval)*ln_initial", conditions.formula))


    abstartval <- list(alphan = -alpha, betan = beta)
    startval <- c(abstartval, conditions.startval)
    

    conv_model_c <- nls(bcmodelc, start = startval, data = cond)


    
    alpha <- coef(summary(conv_model_c))[1, 1]

    beta <- coef(summary(conv_model_c))[2, 1]


    speed <- betaconv.speed(beta = beta, tinterval = tinterval, print.results = FALSE)
    lambda <- speed[1]
    halflife <- speed[2]
    
    
    
    bcestimates <- matrix (nrow = 2+nrow(coef(summary(conv_model_c))), ncol = 4)
    
    conv_model_c_rn <- rownames(coef(summary(conv_model_c)))
    
    rownames(bcestimates) <- c("Alpha", "Beta", conv_model_c_rn[3:length(conv_model_c_rn)], "Lambda", "Halflife")
    
    colnames(bcestimates) <- c("Estimate", "Std. Error", "t value", "Pr (>|t|)")
    
    bcestimates[1:nrow(coef(summary(conv_model_c))),] <- coef(summary(conv_model_c))

    bcestimates[1+nrow(coef(summary(conv_model_c))),1] <- lambda
    
    bcestimates[2+nrow(coef(summary(conv_model_c))),1] <- halflife
    
    cbeta = list(estimates = bcestimates)
    
    regdata <- cbind(conv_model_lin$regdata, conditions)

    results <- list (regdata = regdata, abeta = abeta, cbeta = cbeta)
  }
  
  
  

  if (print.results == TRUE) {
    
    cat ("Absolute Beta Convergence", "\n")
    cat ((paste0("Model coefficients (Estimation method: NLS)", "\n")))
    print (as.data.frame(bestimates))
    
    if (!is.null(conditions)) {
      cat ("\n")
      cat ("Conditional Beta Convergence", "\n")
      cat ((paste0("Model coefficients (Estimation method: NLS)")), "\n")
      print (as.data.frame(bcestimates))
      
    }
    
  }
  
  
  invisible(results)
  
  
}