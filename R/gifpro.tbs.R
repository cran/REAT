gifpro.tbs <-
function (e_ij, a_i, sq_ij, rq_ij, ru_ij = NULL, ai_ij, 
          time.base, tinterval = 1, prog.func = rep("lin", nrow(e_ij)), 
          prog.plot = TRUE, plot.single = FALSE, multiplot.col = NULL, multiplot.row = NULL,
          industry.names = NULL, emp.only = FALSE, output = "short") {
  
  industries <- length(e_ij)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  time.count <- 1:ncol(as.matrix(e_ij))
  time.years <- ncol(as.matrix(e_ij))

  time.years_emp <- cumsum(c(time.base, rep(1, time.years-1)))
  time.years_prog <- cumsum(c((time.base+time.years), rep(1, tinterval-1)))

  time.years_prog_count <- length(time.years_prog)

  x_time <- c(time.years_emp, time.years_prog)


  industries_no <- nrow(e_ij)
  i <- 0 
  
  emp <- matrix (nrow = tinterval, ncol = industries_no)
  models <- list()
  
  plot.legend = TRUE
  
  if ((prog.plot == TRUE) & (plot.single == FALSE)) {
    par_mfrow <- par("mfrow")
    par_mar <- par("mar")
    par_adj <- par("adj")
    par_cexmain <- par("cex.main")
    
    if (is.null(multiplot.row)) {
      ind_rows <- ceiling(sqrt(industries_no))+1
    }
    else {
      ind_rows <- multiplot.row
    }
    
    if (is.null(multiplot.col)) {
      ind_cols <- ceiling(sqrt(industries_no))
    }
    else {
      ind_cols <- multiplot.col
    }
    

    par (mfrow = c(ind_rows,ind_cols), xpd = TRUE)
    par (mar = c(1,1,1,1))
    
    plot.legend <- FALSE
    
  }
  
  for (i in 1:industries_no) {
    
    if ((prog.plot == TRUE) & (plot.single == TRUE)) { 
      dev.new() 
    }
    
    industry_curvefit <- curvefit (x = time.years_emp, y = as.numeric(unlist(e_ij[i,])), 
                                   extrapol = tinterval, plot.curves = prog.plot, plot.title = "",
                                   xlab = "Time", ylab = "Employment", y.min = NULL, plot.legend = plot.legend, xaxt = "n", yaxt = "n",
                                   print.results = FALSE)
    
    if (prog.plot == TRUE) {
      axis (1, at = 1:length(x_time), labels = x_time)
      abline (v = time.years_prog[1])

      par (adj = 0)
      par (cex.main = 1.5) 
      
      title(industry.names[i])
    }
    
    model_industry <- industry_curvefit$models_comp
    models[i] <- list(model_industry)
    
    if (prog.func[i] == "lin") emp[,i] <- industry_curvefit$models_y[(time.years+1):(time.years+tinterval),3]
    if (prog.func[i] == "pow") emp[,i]<- industry_curvefit$models_y[(time.years+1):(time.years+tinterval),4]
    if (prog.func[i] == "exp") emp[,i] <- industry_curvefit$models_y[(time.years+1):(time.years+tinterval),5]
    if (prog.func[i] == "logi") emp[,i] <- industry_curvefit$models_y[(time.years+1):(time.years+tinterval),6]
    
  }
  
  
  if ((plot.single == FALSE) & (prog.plot == TRUE)) {
    
    plot.new()
    legend("center", c("Linear", "Power", "Exponential", "Logistic"), 
           lty = c(1, 1), col = c("blue", "green", "orange", "red"), cex = 1)
    
    
    par(mfrow = par_mfrow)
    par(mar = par_mar)
    par(adj = 0.5)
    par(cex.main = par_cexmain)
  }
  
  colnames(emp) <- industry.names
  rownames(emp) <- time.years_prog
  
  industry_forecast <- list (emp = emp, models = models)
  

  i <- 0
  
  results_peryear <- matrix (ncol = 2, nrow = time.years_prog_count)

  resettlement <- matrix (ncol = time.years_prog_count, nrow = industries_no)
  relocation <- matrix (ncol = time.years_prog_count, nrow = industries_no)
  reuse <- matrix (ncol = time.years_prog_count, nrow = industries_no)
  employment <- matrix (ncol = time.years_prog_count, nrow = industries_no)
  
  
  for (i in 1:time.years_prog_count) {
    
    gifpro_year <- gifpro (e_ij = industry_forecast$emp[i,], 
                           a_i = a_i, sq_ij = sq_ij,
                           rq_ij = rq_ij, tinterval = 1, ai_ij = ai_ij, 
                           time.base = time.base+i, 
                           industry.names = industry.names, output = NULL)
    
    results_peryear[i,] <- gifpro_year$results$peryear
    
    resettlement[,i] <- gifpro_year$components$resettlement[,1]
    relocation[,i] <- gifpro_year$components$relocation[,1]
    reuse[,i] <- gifpro_year$components$reuse[,1]
    employment[,i] <- gifpro_year$components$employment[,1]
  }
  
  colnames (results_peryear) <- c("Employment", "CommercialArea")
  rownames (results_peryear) <- time.years_prog
  
  results_allover <- matrix(ncol = 2, nrow = 2)
  results_allover[1,] <- colSums(results_peryear)
  results_allover[2,] <- results_allover[1,]/tinterval 
  rownames(results_allover) <- c("Sum", "Average")
  colnames(results_allover) <- c("Employment", "Commercial area")
  
  colnames(resettlement) <- time.years_prog
  rownames(resettlement) <- industry.names
  colnames(relocation) <- time.years_prog
  rownames(relocation) <- industry.names
  colnames(reuse) <- time.years_prog
  rownames(reuse) <- industry.names
  colnames(employment) <- time.years_prog
  rownames(employment) <- industry.names
  
  components <- list(resettlement = resettlement, relocation = relocation, reuse = reuse, employment = employment)

  results <- list(peryear = results_peryear, allover = results_allover, industry.forecast = industry_forecast)
  
  
  if (!is.null(output)) {
    
    cat ("\n")
    cat ("GIFPRO", "\n")
    cat ("Method: TBS-GIFPRO", "\n")
    cat ("\n")
    cat ("Employment and commercial area changes (allover)", "\n")
    print(as.data.frame(results_allover))
    cat ("\n")
    
    if (output == "full")
    {
      
      cat ("Employment and commercial area changes (per time unit)", "\n")
      print(as.data.frame(results_peryear))
      cat ("\n")
      
    }
    
    cat ("Calculation for", industries, "industries", "\n")
    cat ("\n")  
    
  }
  
  invisible (list (components = components, results = results))
  
}