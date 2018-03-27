shift <-
function (region1, region2, nation1, nation2, industry.names = NULL, 
                   shift.method = "Dunn", 
                   output.results = TRUE, 
                   plot.results = FALSE, plot.colours = NULL, plot.title = NULL,
                   plot.portfolio = FALSE, ...) 
  {

  if ((ncol(as.data.frame(region1)) > 1) | (ncol(as.data.frame(nation1)) > 1))
  {
    stop (paste("Datasets for initial time period must consist of 1 column (= 1 time period)"), call. = FALSE)
  }
  
  if ((ncol(as.data.frame(region2)) > 1) | (ncol(as.data.frame(nation2)) > 1))
  {
    stop ("Use function shiftd() for dynamic shift-share-analysis", call. = FALSE)
  }
  
  
  if (!shift.method %in% c("Dunn", "Gerfin")) {
    shift.method <- "Dunn"
  }
  
  industries <- length(region1)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  sum.region1 <- sum(region1)
  sum.region2 <- sum(region2)
  sum.nation1 <- sum(nation1)
  sum.nation2 <- sum(nation2)

  growth <- shift.growth(region1 = region1, region2 = region2, nation1 = nation1, nation2 = nation2, industry.names = industry.names)
  

  if (shift.method == "Dunn") {
    
    components <- matrix (nrow = 5, ncol = 1)

    rownames(components) <- c("Growth (t1-t)", "National share", "Industrial mix", "Regional shift", "Net total shift")
    colnames(components) <- c("Components")
    
    components[1] <- growth (sum.region1, sum.region2, growth.type = "abs") 

    components[2] <- sum.region1*sum.nation2/sum.nation1-sum.region1

    components[3] <- sum (region1*(nation2/nation1))-(sum.region1*(sum.nation2/sum.nation1))

    components[4] <- sum (region1*(region2/region1-nation2/nation1))

    components[5] <- sum.region2-(sum.region1*(sum.nation2/sum.nation1))

  }
  
  
  if (shift.method == "Gerfin")
  {

    components <- matrix (nrow = 3, ncol = 1)

    rownames(components) <- c("Industrial mix", "Regional shift", "Net total shift")
    colnames(components) <- c("Components")
    
    growthin.prop <- growth (nation1, nation2, growth.type = "growth")  
    growthir.exp <- region1*growthin.prop
    components[1] <- (sum(growthir.exp)/sum.region1)/(sum.nation2/sum.nation1)

    components[2] <- (sum.region2/sum.region1)/(sum(growthir.exp)/sum.region1)

    components[3] <- (sum.region2/sum.region1)/(sum.nation2/sum.nation1)

  }

    
  if (output.results == TRUE) { 

    
    cat ("\n")
    cat ("Shift-Share Analysis", "\n")
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
  
  results <- list (components = components, growth = growth, method = shift.method)
  
  invisible(results)

}
