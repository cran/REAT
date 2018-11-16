locq.growth <-
function (e_ij1, e_ij2, e_i1, e_i2, industry.names = NULL,
                         y.axis = "r",
                         psize, psize.factor = 10, 
                         pmx = "Regional specialization", pmy = "Regional growth", 
                         pmtitle = "Portfolio matrix", pcol = NULL, pcol.border = NULL,
                         leg = FALSE, leg.fsize = 1, 
                         leg.x = 0, leg.y = y_min*1.5,
                         bg.col = "gray95", bgrid = TRUE, bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
                         seg.x = 1, seg.y = 0) {
  
  if (is.null(industry.names)) {
    industry.names <- as.character(1:length(e_ij1))
  }
  
  shift_growth <- shift.growth (e_ij1, e_ij2, e_i1, e_i2, industry.names = industry.names)


  e_ij <- cbind(e_ij1, e_ij2)
  e_i <- cbind(e_i1, e_i2)

  no_years <- ncol(as.matrix(e_ij))
  
  y <- 0
  
  
  locqs_year <- matrix (nrow = nrow(as.matrix(e_ij)), ncol = no_years)
  
  for (y in 1:no_years) {
    locqs_year[,y] <- locq(e_ij[,y], sum(e_ij[,y]), e_i[,y], sum(e_i[,y]))
  }
  
  rownames(locqs_year) <- industry.names
  

  locqs <- rowMeans(locqs_year)

  
  if (is.null(pcol)) {  pcol <- sample(colours(), nrow(as.data.frame(e_ij1))) }
  else { 
    pcol <- as.character(pcol)
  }
  
  
  point_size <- (psize/max(psize))*psize.factor

  locq_growth <- matrix (ncol = 3, nrow = length(locqs))
  locq_growth[,1] <- locqs
  
  if (y.axis == "n") {
    locq_growth[,2] <- shift_growth[,8]*100
  }
  else if (y.axis == "rn") {
    locq_growth[,2] <- ((shift_growth[,4]/shift_growth[,8])-1)*100
  }
  
  else {
    locq_growth[,2] <- shift_growth[,4]*100
  }
  
  locq_growth[,3] <- point_size
  rownames(locq_growth) <- rownames(shift_growth)
  colnames(locq_growth) <- c("locq", "growth", "point_size")
  
  locq_max <- (max(abs(locq_growth[,1])))
  x_max <- round(locq_max/0.5)*0.5
  x_range <- c(0, x_max)
  
  
  growth_max <- (max(abs(locq_growth[,2])))
  y_max <- round(growth_max/0.5)*0.5
  y_range <- c(-growth_max, growth_max)
  
  y_min <- -growth_max
  
  locq_growth_sorted <- locq_growth[order(-psize),]
  

  dev.new()
  
  if (leg == TRUE) {
    par(mar=c(8.1, 5.6, 4.1, 5.6)) 
  }
  
  
  plot(0, xlim = x_range, ylim = y_range, xlab = pmx, ylab = pmy, main = pmtitle)
  
  
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg.col)

  
  if (bgrid == TRUE)
  {
    grid (col = bgrid.col, lty = bgrid.type, lwd = bgrid.size)
  } 
  
  abline (h = seg.y, v = seg.x)
  
  points(locq_growth_sorted[,1], locq_growth_sorted[,2], xlim = x_range, ylim = y_range, cex = locq_growth_sorted[,3],
         pch = 21, col = as.character(pcol.border), bg = as.character(pcol))
  
  
  if (leg == TRUE) {

    par(mar=c(8.1, 5.6, 4.1, 5.6), xpd=TRUE) 


    legend(leg.x, leg.y, legend = industry.names, fill = as.character(pcol), cex = leg.fsize, ncol = 2, bg = "white")

    par(mar=c(5.1, 4.1, 4.1, 2.1)) 
  }
  
  invisible(list (portfolio.data = locq_growth, locq = locqs_year, growth = shift_growth))
}
