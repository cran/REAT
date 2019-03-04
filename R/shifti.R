shifti <- function (e_ij1, e_ij2, e_i1, e_i2, 
                    industry.names = NULL, 
                    shift.method = "Dunn", 
                    print.results = TRUE, 
                    plot.results = FALSE, plot.colours = NULL, plot.title = NULL,
                    plot.portfolio = FALSE, ...)

{
  
  industries <- length(e_ij1)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  e_j1 <- sum(e_ij1)
  e_j2 <- sum(e_ij2)
  e1 <- sum(e_i1)
  e2 <- sum(e_i2)

  growth <- shift.growth(e_ij1 = e_ij1, e_ij2 = e_ij2, e_i1 = e_i1, e_i2 = e_i2, industry.names = industry.names)
  
  shift_all <- shift (e_ij1, e_ij2, e_i1, e_i2, shift.method = shift.method, print.results = FALSE)

  components.industry <- matrix(ncol = industries, nrow = nrow(shift_all$components)) 

  i <- 0
  
  for (i in 1:industries)
  {
    shift_industry <- shift ((e_ij1[i]), (e_ij2[i]), (e_i1[i]), (e_i2[i]),
                             shift.method = shift.method, print.results = FALSE)
    
    components.industry[,i] <- shift_industry$components[,1]
  }
  
  colnames(components.industry) <- industry.names
  rownames(components.industry) <- rownames(shift_industry$components)
  
  components.industry <- components.industry[rownames(components.industry) != "Industrial mix",] 
  
  
  if (print.results == TRUE) { 

    cat ("\n")
    cat ("Shift-Share Analysis", "\n")
    cat ("Method:", shift.method, "\n")
    cat ("\n")
    cat ("Shift-share components", "\n")
    
    print(as.data.frame(components.industry))
    
    cat ("\n")
    
    cat ("Calculation for", industries, "industries", "\n")
    cat ("\n")  
  }
  
  
  if (plot.results == TRUE) {
    
    if (is.null(plot.colours)) {
      plot.colours <- sample(colours(), ncol(components.industry))
    }
    
    if (is.null(plot.title)) {
      plot.title <- "Shift-share analysis"
    }
    
    dev.new()
    
    shiftplot <- barplot (components.industry, names.arg = NULL, col = plot.colours, legend = NULL, main = plot.title, beside = TRUE, cex.names = 0.6)
    legend("topright", legend = rownames(components.industry), fill = plot.colours, cex = 0.7)
    text(shiftplot, components.industry/2, labels = round(components.industry, 2), cex = 0.6)
    
  }
  
  if (plot.portfolio == TRUE) {
    
    portfolio (e_ij1, e_ij2, e_i1, e_i2, industry.names = industry.names, ...)
  }
  
  
  results <- list (components = shift_all$components, components.industry = components.industry, growth = growth, method = shift.method)
  
  invisible(results)

}