portfolio <-
function (region1, region2, nation1, nation2, industry.names = NULL, 
                       psize, psize.factor = 10,
                       pmx = "Regional growth", pmy = "National growth", 
                       pmtitle = "Portfolio matrix", pcol = NULL,
                       leg = FALSE, leg.fsize = 1, 
                       leg.x = -max_val, leg.y = -max_val/2,
                       bg.col = "gray95", bgrid = TRUE, bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
                       seg.x = 0, seg.y = 0)

  {

    if (is.null(industry.names)) {
      industry.names <- as.character(1:length(region1))
    }
  
    growth_region <- growth (region1, region2, growth.type = "rate")
    growth_nation <- growth (nation1, nation2, growth.type = "rate")
    
    max_val <- max(growth_region, growth_nation)

    if (is.null(pcol)) { pcol <- sample(colours(), length(growth_region)) }
    
    point_size <- (psize/max(psize))*psize.factor
    
    max_val_inc <- 0
    
    plot(0, type = 'n', xlab = pmx, ylab = pmy, main = pmtitle, 
         xlim = c(-max_val,max_val), ylim = c(-max_val-max_val_inc,max_val))

    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg.col)

    if (bgrid == TRUE)
    {
      grid (col = bgrid.col, lty = bgrid.type, lwd = bgrid.size)
    } 
    
    points(growth_region, growth_nation, cex = point_size, 
          pch = 16, col = as.character(pcol))

    abline (h = seg.y, v = seg.x, col = "black")
    
    if (leg == TRUE) {
      legend(leg.x, leg.y, legend = industry.names, fill = as.character(pcol), cex = leg.fsize, ncol = 2, bg = "white")
    }
  }
