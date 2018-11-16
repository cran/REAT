curvefit <- 
function (x, y, y.max = NULL,
                      extrapol = NULL,
                      plot.curves = TRUE, pcol = "black", ptype = 19, psize = 1, 
                      lin.col = "blue", pow.col = "green", exp.col = "orange", logi.col = "red")
{
  
  modlin <- lm (y ~ x)
  
  
  modelsy <- matrix(nrow = length(x), ncol = 6)
  colnames(modelsy) <- c("x", "y", "ypred_lin", "ypred_pow", "ypred_exp", "ypred_logi")
  
  modelsy[,1] <- x
  modelsy[,2] <- y
  modelsy[,3] <- modlin$coefficients[1]+modlin$coefficients[2]*x
  
  
  modpow_lin <- NA
  modexp_lin <- NA
  modlog_lin <- NA
  
  
  if (all(y > 0) & all(x > 0)) {


    modpow_lin <- lm (log10(y) ~ log10(x))
    
    modelsy[,4] <- (10^(modpow_lin$coefficients[1]))*(x^(modpow_lin$coefficients[2]))
    
  }  
  
  
  if (all(y > 0)) {


    modexp_lin <- lm (log(y) ~ x)
    
    

    if (is.null(y.max)) {
      y.max <- (max(y)*1.001)
    }
    
    modlog_lin <- lm ( (log((y.max-y)/y)) ~ x)
    
    modelsy[,5] <- (exp(modexp_lin$coefficients[1]))*(exp(x*(modexp_lin$coefficients[2])))
    modelsy[,6] <- (y.max/(1+(exp(modlog_lin$coefficients[1]+modlog_lin$coefficients[2]*x))))
    
  }
  
  
  if (!is.null(extrapol)) {
    
    modelsy <- matrix(nrow = length(x)+extrapol, ncol = 6)
    
    i <- 0
    
    for (i in 1:extrapol) {
      x[length(x)+1] <- x[length(x)]+1
    }
    
    y[(length(y)+1):(length(y)+extrapol)] <- NA

    modelsy[,1] <- x
    modelsy[,2] <- y
    modelsy[,3] <- modlin$coefficients[1]+modlin$coefficients[2]*x
    
    if (all(y > 0) & all(x > 0)) {
      modelsy[,4] <- (10^(modpow_lin$coefficients[1]))*(x^(modpow_lin$coefficients[2])) 
    }
    
    if (all(y > 0)) {
      modelsy[,5] <- (exp(modexp_lin$coefficients[1]))*(exp(x*(modexp_lin$coefficients[2])))
      modelsy[,6] <- (y.max/(1+(exp(modlog_lin$coefficients[1]+modlog_lin$coefficients[2]*x)))) 
    }
  }
  
  colnames(modelsy) <- c("x", "y", "ypred_lin", "ypred_pow", "ypred_exp", "ypred_logi")
  

  
  modelsy_sort <- modelsy[order(modelsy[,1]),]

  if (plot.curves == TRUE) {

    dev.new()
    plot(modelsy_sort[,1], modelsy_sort[,2], xlim = c(min(modelsy_sort[,1]), max(modelsy_sort[,1])), ylim = c((min(modelsy_sort[,2:5], na.rm=TRUE)), (max(modelsy_sort[,2:5], na.rm=TRUE))), xlab = "x", ylab = "y", col = pcol, pch = ptype, cex = psize)
    
    
    lines(x = modelsy_sort[,1], y = modelsy_sort[,3], col = lin.col, lwd = 1.5)
    
    if (all(y > 0) & all(x > 0)) {
      
      lines(x = modelsy_sort[,1], y = modelsy_sort[,4], col = pow.col, lwd = 1.5)
      lines(x = modelsy_sort[,1], y = modelsy_sort[,5], col = exp.col, lwd = 1.5)
      lines(x = modelsy_sort[,1], y = modelsy_sort[,6], col = logi.col, lwd = 1.5)
      
    }
  }
  
  models_comp <- matrix (ncol = 10, nrow = 4)
  
  modlin_summary <- summary(modlin)

  models_comp[1,1] <- modlin_summary$coefficients[1,1]
  models_comp[1,2] <- modlin_summary$coefficients[2,1]

  models_comp[1,3] <- modlin_summary$coefficients[1,2]
  models_comp[1,4] <- modlin_summary$coefficients[2,2]

  models_comp[1,5] <- modlin_summary$coefficients[1,3]
  models_comp[1,6] <- modlin_summary$coefficients[2,3]

  models_comp[1,7] <- modlin_summary$coefficients[1,4]
  models_comp[1,8] <- modlin_summary$coefficients[2,4]

  models_comp[1,9] <- modlin_summary$r.squared
  models_comp[1,10] <- modlin_summary$adj.r.squared

  if (all(y > 0) & all(x > 0)) {
    models_comp[2,1] <- 10^(modpow_lin$coefficients[1])
    models_comp[2,2] <- modpow_lin$coefficients[2]
  }
  
  if (all(y > 0) & all(x > 0)) {
    models_comp[3,1] <- exp(modexp_lin$coefficients[1])
    models_comp[3,2] <- modexp_lin$coefficients[2]
    
    models_comp[4,1] <- modlog_lin$coefficients[1]
    models_comp[4,2] <- modlog_lin$coefficients[2]
  }
  
  colnames(models_comp) <- c("a", "b", "Std. Error a", "Std. Error b", 
                             "t value a", "t value b", "Pr(>|t|) a", "Pr(>|t|) b", "R squared", "Adj. R squared")
  rownames(models_comp) <- c("Linear", "Power", "Exponential", "Logistic")
  

  cat("Curve fitting", "\n")
  cat("\n")
  print(as.data.frame(models_comp))
  
  invisible(list (models_comp = models_comp, models_y = modelsy))
  
}
