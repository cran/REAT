portfolio <-
function (e_ij1, e_ij2, e_i1, e_i2, industry.names = NULL, 
                       psize, psize.factor = 10, 
                       pmx = "Regional growth", pmy = "National growth", 
                       pmtitle = "Portfolio matrix", pcol = NULL, pcol.border = NULL,
                       leg = FALSE, leg.fsize = 1, 
                       leg.x = -max_val, leg.y = -max_val*1.5,
                       bg.col = "gray95", bgrid = TRUE, bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
                       seg.x = 0, seg.y = 0)

  {
    
    if (is.null(industry.names)) {
      industry.names <- as.character(1:length(e_ij1))
    }
  
    if (is.null(pcol)) {  pcol <- sample(colours(), nrow(as.data.frame(e_ij1))) }
    else { 
      pcol <- as.character(pcol)
    }
  

    e_ij1_workfile <- data.frame(industry.names, e_ij1, psize, pcol)
    e_ij1_workfile <- e_ij1_workfile[order(-psize),]

    e_ij2_workfile <- data.frame(industry.names, e_ij2, psize)
    e_ij2_workfile <- e_ij2_workfile[order(-psize),]
    
    e_i1_workfile <- data.frame(industry.names, e_i1, psize)
    e_i1_workfile <- e_i1_workfile[order(-psize),]
    
    e_i2_workfile <- data.frame(industry.names, e_i2, psize)
    e_i2_workfile <- e_i2_workfile[order(-psize),]
    
    shiftgrowth <- shift.growth (e_ij1_workfile[,2], e_ij2_workfile[,2:(ncol(as.matrix(e_ij2_workfile))-1)],
                                 e_i1_workfile[,2], e_i2_workfile[,2:(ncol(as.matrix(e_i2_workfile))-1)],
                                 industry.names = e_ij1_workfile[,1])
    
    max_val <- (max((shiftgrowth[,4]*100), (shiftgrowth[,8]*100)))*1.2

    point_size <- (e_ij1_workfile$psize/max(e_ij1_workfile$psize))*psize.factor

    max_val_inc <- 0
    
    dev.new(width = 1000, height = 1000, unit = "px")
    

    if (leg == TRUE) {
      par(mar=c(8.1, 5.6, 4.1, 5.6)) 
    }
    
    plot(0, type = 'n', xlab = pmx, ylab = pmy, main = pmtitle, 
         xlim = c(-max_val,max_val), ylim = c(-max_val-max_val_inc,max_val))

    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg.col)

    
    
    if (bgrid == TRUE)
    {
      grid (col = bgrid.col, lty = bgrid.type, lwd = bgrid.size)
    } 
    
    abline (h = seg.y, v = seg.x, col = "black")
    

    points((shiftgrowth[,4]*100), (shiftgrowth[,8]*100), cex = point_size, pch = 21, col = as.character(pcol.border), bg = as.character(e_ij1_workfile$pcol))

    if (leg == TRUE) {

      par(mar=c(8.1, 5.6, 4.1, 5.6), xpd=TRUE) 

      e_ij1_workfile_leg <- e_ij1_workfile[order(e_ij1_workfile$industry.names),]

      legend(leg.x, leg.y, legend = e_ij1_workfile_leg$industry.names, fill = as.character(e_ij1_workfile_leg$pcol), cex = leg.fsize, ncol = 2, bg = "white")

      par(mar=c(5.1, 4.1, 4.1, 2.1)) 
    }
}
