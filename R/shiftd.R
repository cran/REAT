shiftd <-
function (region1, region2, nation1, nation2, time1, time2,
                    industry.names = NULL, 
                    shift.method = "Dunn",
                    gerfin.shifts = "sum", 
                    output.results = TRUE, 
                    plot.results = FALSE, plot.colours = NULL, plot.title = NULL,
                    plot.portfolio = FALSE, ...)
{
  
  if ((ncol(as.data.frame(region1)) > 1) | (ncol(as.data.frame(nation1)) > 1))
  {
    stop (paste("Datasets for initial time period must consist of 1 column (= 1 time period)"), call. = FALSE)
  }
  
  if ((ncol(as.data.frame(region2)) == 1) & (ncol(as.data.frame(nation2)) == 1))
  {
    shift (region1, region2, nation1, nation2, industry.names = industry.names, 
    shift.method = shift.method, output.results = output.results, 
    plot.results = plot.results, plot.colours = plot.colours, plot.title = plot.title,
    plot.portfolio = plot.portfolio, ...)
    
    stop ("No dynamic shift-share analysis (region2 and nation2 consist of only one time period). Function shift() used.", call. = FALSE)
    
  }
  
  if (ncol(as.data.frame(region2)) != ncol(as.data.frame(nation2)))
  {
    stop ("Compared region data must consist of the same time periods", call. = FALSE)
  }
  
  
  industries <- length(region1)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  
  
  sum.region1 <- sum(region1)
  sum.region2 <- sum(region2[,ncol(region2)])
  sum.nation1 <- sum(nation1)
  sum.nation2 <- sum(nation2[,ncol(nation2)])

  growth <- shift.growth(region1 = region1, region2 = region2, nation1 = nation1, nation2 = nation2, industry.names = industry.names)
  
  region_all <- cbind (region1, region2)
  nation_all <- cbind (nation1, nation2)

  years <- time1:time2
  no_years <- length(time1:time2)-1

  years.growth <- vector()
  
  i <- 0
  for (i in 1:no_years)
  {
    years.growth[i] <- paste0 (years[i], "-", years[i+1])
  }
  

  i <- 0

  shift_test <- shift ((region_all[,1]), (region_all[,2]), (nation_all[,1]), (nation_all[,2]), 
                       shift.method = shift.method, output.results = FALSE)

  components.year <- matrix(ncol = no_years, nrow = nrow(shift_test$components)) 

  for (i in 1:no_years)
  {
    shift_year <- shift ((region_all[,i]), (region_all[,(i+1)]), (nation_all[,i]), (nation_all[,(i+1)]),
           shift.method = shift.method, output.results = FALSE)
    
    components.year[,i] <- shift_year$components[,1]
  }
  

  colnames(components.year) <- years.growth
  rownames(components.year) <- rownames(shift_year$components)

  if ((shift.method == "Gerfin") && (gerfin.shifts == "mean"))
  {
    components <- as.matrix(rowMeans (components.year))
  }
  else {
    components <- as.matrix(rowSums (components.year))
  }
  
  colnames(components) <- c("Components")

  
  if (output.results == TRUE) { 

    cat ("\n")
    cat ("Dynamic Shift-Share Analysis", "\n")
    cat ("Method:", shift.method, "\n")
    cat ("\n")
    cat ("Shift-share components", "\n")
    
    print(as.data.frame(components))

    cat ("\n")
    
    cat ("Calculation for", industries, "industries", "\n")
    cat ("Regional employment at time t: ", sum.region1, ", at time t+1: ", sum.region2, " (", growth(sum.region1, sum.region2, growth.type = "abs"), " / ", growth(sum.region1, sum.region2, growth.type = "rate"), " %)", sep="", "\n")
    cat ("National employment at time t: ", sum.nation1, ", at time t+1: ", sum.nation2, " (", growth(sum.nation1, sum.nation2, growth.type = "abs"), " / ", growth(sum.nation1, sum.nation2, growth.type = "rate"), " %)", sep="", "\n")
    cat ("\n")  
  }
  
  
  if (plot.results == TRUE) {
    
    if (is.null(plot.colours)) {
      plot.colours <- sample(colours(), length(components))
    }
    
    if (is.null(plot.title)) {
      plot.title <- "Shift-share analysis"
    }
    
    shiftplot <- barplot (components[,1], names.arg = NULL, col = plot.colours, legend = NULL, main = plot.title)
    legend("topright", legend = rownames(components), fill = plot.colours)
    text(shiftplot, components/2, labels = round(components, 2), cex = 0.8)
    
  }
  
  if (plot.portfolio == TRUE) {
    
    dev.new()
    
    portfolio (region1 = region1, region2 = region2, nation1 = nation1, nation2 = nation2, 
               industry.names = industry.names, ...)
  }
  
  
  results <- list (components = components, components.year = components.year, growth = growth, method = shift.method)
  
  invisible(results)

}
