locq <- function (e_ij, e_j, e_i, e, industry.names = NULL, plot.results = FALSE, LQ.method = "m",
                  plot.title = "Localization quotients", bar.col = "lightblue", line.col = "red", 
                  arg.size = 1) {

  
  if (nrow(as.matrix(e_ij)) != nrow(as.matrix(e_i))) {
    stop("Regional and national employment vectors must have the same length")
  }
  
  if ((nrow(as.matrix(e_j)) > 1) | (nrow(as.matrix(e)) > 1)) {
    stop("Total industry and national employment must be a single value")
  }
  
  if (sum(e_ij) > sum(e_j)) { return (NA) }

  if (sum(e_i) > sum(e)) { return (NA) }

  if (sum(e_j) > sum(e)) { return (NA) }

  s_ij <- e_ij/e_i

  s_j <- e_j/e  

  if (LQ.method == "a") {
    
    LQ <- s_ij-s_j

  } else {
    
    LQ <- s_ij/s_j
  }
  
  
  if ((length(e_ij) > 1) & (plot.results == TRUE)) {
    
    if (is.null(industry.names)) {
      industry.names <- as.character(1:length(e_ij))
    }
    
    dev.new()
    
    expandval <- (max(nchar(as.character(industry.names))))*0.33
    
    par(mar=c(5.1, ((4.1+expandval)*arg.size), 4.1, 2.1), xpd=FALSE)
    
    LQ_mat <- matrix(nrow = length(LQ), ncol = 1)
    LQ_mat[,1] <- LQ
    rownames(LQ_mat) <- industry.names
    colnames(LQ_mat) <- "LQ"
    LQ_mat2 <- LQ_mat[order(rownames(LQ_mat), decreasing = TRUE),]
    
    barplot_locq <- barplot (LQ_mat2, horiz = TRUE, cex.names = arg.size, las = 1, col = bar.col, main = plot.title)
    abline (v = 1, lwd = 2,  col = line.col)
    
    par(mar=c(5.1, 4.1, 4.1, 2.1)) 

    cat("Location quotients", "\n")
    cat(paste0("I = ", nrow(LQ_mat), " industries"), "\n")
    cat("\n")
    
    return(LQ_mat)
  }
  
  return(LQ)
}