lorenz <-
function (x, weighting = NULL, z = NULL, na.rm = TRUE,
                    lcx = "% of objects", lcy = "% of regarded variable", 
                    lctitle = "Lorenz curve", le.col = "blue", lc.col = "black",
                    lsize = 1.5, ltype = "solid",
                    bg.col = "gray95", 
                    bgrid = TRUE, bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
                    lcg = FALSE, lcgn = FALSE, lcg.caption = NULL, lcg.lab.x = 0, lcg.lab.y = 1, 
                    add.lc = FALSE, plot.lc = TRUE)
{
  
  if (!is.null(weighting))
  {
    
    if (na.rm == TRUE) {
      weighting <- weighting[!is.na(weighting)]
    }
    
    if (length(x) != length(weighting))
    {
      stop("Frequency and weighting differ in length", call. = FALSE)
    }
  }
  
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  
  
  x_sort <- sort(x)   

  i <- length(x)   

  if (add.lc == FALSE)
  {
    
    k <- c(0,1)
    l <- c(0,1)

    
    plot(k, l, type = "l", col = le.col, xlab = lcx, ylab = lcy, main = lctitle, lwd = lsize, lty = ltype)

    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg.col)

    if (bgrid == TRUE)
    {
      grid (col = bgrid.col, lty = bgrid.type, lwd = bgrid.size)
    }   
    
    lines(k, l, col = le.col, xlab = lcx, ylab = lcy, main = lctitle, lwd = lsize, lty = ltype)

  }

  if (is.null(weighting)) { 


  sum_x <- sum(x_sort)
  
  a_i <- x_sort/sum(x)

  y_i <- cumsum(a_i)


  if (is.null(z)) {
    j <- 1:i
    z_i <- j/i
  }
  
  else {
    xz <- cbind(x,z)
    xz <- xz[order(x),] 

    z_rel <- xz[,2]/sum(xz[,2])
    z_i <- cumsum(z_rel)

  }
   
  start_start <- c(0.0, z_i[1])
  start_end <- c(0.0, y_i[1])

  if (plot.lc == TRUE)
  {
    lines (z_i, y_i, col = lc.col, lwd = lsize, lty = ltype)
    lines (start_start, start_end, col = lc.col, lwd = lsize, lty = ltype)

  }
 
  }
  
  else { 

    n_j <- weighting
    n <- sum (n_j)
    n_j_n <- n_j/n
    z_j <- cumsum (n_j_n)
    x_j <- x_sort
    x_j_n_j <- (x_j*n_j)/sum(x_j*n_j)
    y_j <- cumsum(x_j_n_j)

    start_start <- c(0.0, z_j[1])
    start_end <- c(0.0, y_j[1])

    if (plot.lc == TRUE)
    {
      lines (z_j, y_j, col = lc.col, lwd = lsize, lty = ltype)
      lines (start_start, start_end, col = lc.col, lwd = lsize, lty = ltype)
    }
    
}
  
  
  if (lcg == TRUE) {
    G <- gini (x, weighting = weighting)

      if (!is.null(lcg.caption))
      {
        text (lcg.lab.x, lcg.lab.y, lcg.caption, pos = 4, col = lc.col)
      }
        
    text (lcg.lab.x, lcg.lab.y-0.05, paste("G =", round(G, 4)), pos = 4, col = lc.col)
  }
  
  if (lcgn == TRUE) {
    G.norm <- gini (x, weighting = weighting, coefnorm = TRUE)
    text (lcg.lab.x, lcg.lab.y-0.1, paste("G* =", round(G.norm, 4)), pos = 4, col = lc.col)
  }
  
}
