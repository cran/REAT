shift <- function (e_ij1, e_ij2, e_i1, e_i2, industry.names = NULL, 
                   shift.method = "Dunn", 
                   print.results = TRUE, 
                   plot.results = FALSE, plot.colours = NULL, plot.title = NULL,
                   plot.portfolio = FALSE, ...) 
{

  
  if ((ncol(as.data.frame(e_ij1)) > 1) | (ncol(as.data.frame(e_i1)) > 1))
  {
    stop (paste("Datasets for initial time period must consist of 1 column (= 1 time period)"), call. = FALSE)
  }
  
  if ((ncol(as.data.frame(e_ij2)) > 1) | (ncol(as.data.frame(e_i2)) > 1))
  {
    stop ("Use function shiftd() for dynamic shift-share-analysis", call. = FALSE)
  }
  
  
  if (!shift.method %in% c("Dunn", "Esteban", "Gerfin")) {
    shift.method <- "Dunn"
  }

  industries <- length(e_ij1)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  e_j1 <- sum(e_ij1)
  e_j2 <- sum(e_ij2)
  e1 <- sum(e_i1)
  e2 <- sum(e_i2)

  growth <- shift.growth(e_ij1 = e_ij1, e_ij2 = e_ij2, e_i1 = e_i1, e_i2 = e_i2, industry.names = industry.names)
  
  
  if (shift.method == "Dunn") {
    

    components <- matrix (nrow = 5, ncol = 1)

    rownames(components) <- c("Growth (t1-t)", "National share", "Industrial mix", "Regional share", "Net total shift")
    colnames(components) <- c("Components")
    
    components[1] <- growth (e_j1, e_j2, growth.type = "abs") 

    components[2] <- e_j1*e2/e1-e_j1

    
    components[3] <- sum (e_ij1*(e_i2/e_i1))-(e_j1*(e2/e1))

    
    components[4] <- sum (e_ij1*(e_ij2/e_ij1-e_i2/e_i1))
 
    
    components[5] <- e_j2-(e_j1*(e2/e1))

  }
  
  
  if (shift.method == "Esteban") {
    

    components <- matrix (nrow = 5, ncol = 1)

    rownames(components) <- c("Growth (t1-t)", "National share", "Industrial mix", "Regional share", "Allocation effect")
    colnames(components) <- c("Components")
    
    components[1] <- growth (e_j1, e_j2, growth.type = "abs") #e_j2-e_j1

    components[2] <- e_j1*e2/e1-e_j1
 
    
    components[3] <- sum (e_ij1*(e_i2/e_i1))-(e_j1*(e2/e1))
  
    
    he_e_ij1 <- e_j1*(e_i1/e1)

    components[4] <- sum (he_e_ij1*(e_ij2/e_ij1-e_i2/e_i1))

    
    components[5] <-  sum((e_ij1-he_e_ij1)*(e_ij2/e_ij1-e_i2/e_i1))

  }
  
  
  if (shift.method == "Gerfin")
  {

    components <- matrix (nrow = 3, ncol = 1)

    rownames(components) <- c("Industrial mix", "Regional share", "Net total shift")
    colnames(components) <- c("Components")
    
    growthin.prop <- growth (e_i1, e_i2, growth.type = "growth")  #(e_i2/e_i1)
    growthir.exp <- e_ij1*growthin.prop
    components[1] <- (sum(growthir.exp)/e_j1)/(e2/e1)

    components[2] <- (e_j2/e_j1)/(sum(growthir.exp)/e_j1)

    components[3] <- (e_j2/e_j1)/(e2/e1)

  }

  
  if (print.results == TRUE) { 

    
    cat ("\n")
    cat ("Shift-Share Analysis", "\n")
    cat ("Method:", shift.method, "\n")
    cat ("\n")
    cat ("Shift-share components", "\n")
    
    print(as.data.frame(components))

    cat ("\n")
    
    cat ("Calculation for", industries, "industries", "\n")
    cat ("Regional employment at time t: ", e_j1, ", at time t+1: ", e_j2, " (", growth(e_j1, e_j2, growth.type = "abs"), " / ", growth(e_j1, e_j2, growth.type = "rate", rate.perc = TRUE), " %)", sep="", "\n")
    cat ("National employment at time t: ", e1, ", at time t+1: ", e2, " (", growth(e1, e2, growth.type = "abs"), " / ", growth(e1, e2, growth.type = "rate", rate.perc = TRUE), " %)", sep="", "\n")
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
    

    portfolio (e_ij1, e_ij2, e_i1, e_i2, 
               industry.names = industry.names, ...)
  }
  
  
  results <- list (components = components, growth = growth, method = shift.method)
  
  invisible(results)

}