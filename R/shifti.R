shifti <-
function (region1, region2, nation1, nation2, 
                    industry.names = NULL, 
                    shift.method = "Dunn", 
                    output.results = TRUE, 
                    plot.results = FALSE, plot.colours = NULL, plot.title = NULL,
                    plot.portfolio = FALSE, ...)

{
  
  industries <- length(region1)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  sum.region1 <- sum(region1)
  sum.region2 <- sum(region2)
  sum.nation1 <- sum(nation1)
  sum.nation2 <- sum(nation2)

  growth <- shift.growth(region1 = region1, region2 = region2, nation1 = nation1, nation2 = nation2, industry.names = industry.names)
  
  shift_all <- shift (region1, region2, nation1, nation2, shift.method = shift.method, output.results = FALSE)

  components.industry <- matrix(ncol = industries, nrow = nrow(shift_all$components)) 

  i <- 0
  
  for (i in 1:industries)
  {
    shift_industry <- shift ((region1[i]), (region2[i]), (nation1[i]), (nation2[i]),
                         shift.method = shift.method, output.results = FALSE)
    
    components.industry[,i] <- shift_industry$components[,1]
  }
  
  colnames(components.industry) <- industry.names
  rownames(components.industry) <- rownames(shift_industry$components)
  
  components.industry <- components.industry[rownames(components.industry) != "Industrial mix",] 


  if (output.results == TRUE) { 

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
    
    shiftplot <- barplot (components.industry, names.arg = NULL, col = plot.colours, legend = NULL, main = plot.title, beside = TRUE)
    legend("topright", legend = rownames(components.industry), fill = plot.colours)
    text(shiftplot, components.industry/2, labels = round(components.industry, 2), cex = 0.8)

  }
  
  if (plot.portfolio == TRUE) {
    
    dev.new()
    
    portfolio (region1 = region1, region2 = region2, nation1 = nation1, nation2 = nation2, 
               industry.names = industry.names, ...)
  }
  
  
  results <- list (components = shift_all$components, components.industry = components.industry, growth = growth, method = shift.method)
  
  invisible(results)

}
