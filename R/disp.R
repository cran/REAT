disp <- function (x, weighting = NULL, at.epsilon = 0.5, na.rm = TRUE) { 

  x_ncol <- ncol(as.matrix(x))
  
  i <- 0
  
  if (!is.null(weighting)) {
    
    disp_results <- matrix (ncol = x_ncol, nrow = 19)

    for (i in 1:x_ncol) {
      disp_results[1,i] <- gini2 (as.matrix(x)[,i], na.rm = na.rm)
      disp_results[2,i] <- gini2 (as.matrix(x)[,i], coefnorm = TRUE, na.rm = na.rm)
      disp_results[3,i] <- gini2 (as.matrix(x)[,i], weighting = (as.matrix(weighting)), na.rm = na.rm)
      disp_results[4,i] <- gini2 (as.matrix(x)[,i], weighting = (as.matrix(weighting)), coefnorm = TRUE, na.rm = na.rm)
      
      disp_results[5,i] <- herf (as.matrix(x)[,i], coefnorm = FALSE, na.rm = na.rm)
      disp_results[6,i] <- herf (as.matrix(x)[,i], coefnorm = TRUE, na.rm = na.rm)
      disp_results[7,i] <- herf (as.matrix(x)[,i], output = "eq", na.rm = na.rm)
      
      disp_results[8,i] <- hoover (as.matrix(x)[,i], na.rm = na.rm)
      disp_results[9,i] <- hoover (as.matrix(x)[,i], ref = (as.matrix(weighting)), weighting = (as.matrix(weighting)), na.rm = na.rm)
      
      disp_results[10,i] <- theil (as.matrix(x)[,i], na.rm = na.rm)
      disp_results[11,i] <- theil (as.matrix(x)[,i], weighting = (as.matrix(weighting)), na.rm = na.rm)
      
      disp_results[12,i] <- coulter (as.matrix(x)[,i], weighting = (as.matrix(weighting)), na.rm = na.rm)
      
      disp_results[13,i] <- atkinson (as.matrix(x)[,i], epsilon = at.epsilon, na.rm = na.rm)
      
      disp_results[14,i] <- dalton (as.matrix(x)[,i], na.rm = na.rm)
      
      disp_results[15,i] <- sd2 (as.matrix(x)[,i], na.rm = na.rm)
      disp_results[16,i] <- sd2 (as.matrix(x)[,i], weighting = (as.matrix(weighting)), na.rm = na.rm)
      
      disp_results[17,i] <- cv (as.matrix(x)[,i], na.rm = na.rm)
      disp_results[18,i] <- cv (as.matrix(x)[,i], na.rm = na.rm, coefnorm = TRUE)
      
      disp_results[19,i] <- williamson (as.matrix(x)[,i], weighting = (as.matrix(weighting)), na.rm = na.rm)
    }
    
    rownames(disp_results) <- c("Gini", "Gini n", "Gini w", "Gini w n",
                                "HHI", "HHI n", "HHI eq", 
                                "Hoover", "Hoover w", 
                                "Theil", "Theil w",
                                "Coulter", 
                                "Atkinson",
                                "Dalton",
                                "SD", "SD w", 
                                "CV", "CV n", 
                                "Williamson")
    colnames(disp_results) <- colnames(x)
    
  }
  
  else {
    
    disp_results <- matrix (ncol = x_ncol, nrow = 13)

    for (i in 1:x_ncol) {
      disp_results[1,i] <- gini2 (as.matrix(x)[,i], na.rm = na.rm)
      disp_results[2,i] <- gini2 (as.matrix(x)[,i], coefnorm = TRUE, na.rm = na.rm)
      
      disp_results[3,i] <- herf (as.matrix(x)[,i], coefnorm = FALSE, na.rm = na.rm)
      disp_results[4,i] <- herf (as.matrix(x)[,i], coefnorm = TRUE, na.rm = na.rm)
      disp_results[5,i] <- herf (as.matrix(x)[,i], output = "eq", na.rm = na.rm)
      
      disp_results[6,i] <- hoover (as.matrix(x)[,i], na.rm = na.rm)
      
      disp_results[7,i] <- theil (as.matrix(x)[,i], na.rm = na.rm)
      
      disp_results[8,i] <- coulter (as.matrix(x)[,i], na.rm = na.rm)
      
      disp_results[9,i] <- atkinson (as.matrix(x)[,i], epsilon = at.epsilon, na.rm = na.rm)
      
      disp_results[10,i] <- dalton (as.matrix(x)[,i], na.rm = na.rm)
      
      disp_results[11,i] <- sd2 (as.matrix(x)[,i], na.rm = na.rm)
      
      disp_results[12,i] <- cv (as.matrix(x)[,i], na.rm = na.rm)
      disp_results[13,i] <- cv (as.matrix(x)[,i], na.rm = na.rm, coefnorm = TRUE)
      
    }
    
    rownames(disp_results) <- c("Gini", "Gini n", 
                                "HHI", "HHI n", "HHI eq", 
                                "Hoover non-w", 
                                "Theil",
                                "Coulter non-w",
                                "Atkinson",
                                "Dalton",
                                "SD", 
                                "CV", "CV n")
    colnames(disp_results) <- colnames(x)
  }
  
  cat("Concentration and dispersion measures", "\n")
  cat("Note: w = weighted, n = normalized, eq = equivalent number", "\n")
  cat("\n")
  
  print(as.data.frame(disp_results))
  
  invisible(disp_results)
}