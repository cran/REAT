gini <-
function (x, coefnorm = FALSE, weighting = NULL,
                  lc = FALSE, 
                  lcx = "% of objects", lcy = "% of regarded variable", 
                  lctitle = "Lorenz curve", le.col = "blue", lc.col = "black",
                  lsize = 1, ltype = "solid",
                  bg.col = "gray95", 
                  bgrid = TRUE, bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
                  lcg = FALSE, lcgn = FALSE, lcg.caption = NULL, lcg.lab.x = 0, lcg.lab.y = 1, 
                  add.lc = FALSE) 
{   

  if (!is.null(weighting))
  {
    if (length(x) != length(weighting))
    {
      stop("Frequency and weighting differ in length")
    }
  }
  
  x_sort <- sort(x)   

  i <- length(x)   

  if (is.null(weighting)) { # Calculation from Doersam (2004), p. 44

    sum_x <- sum(x_sort)

    a_i <- x_sort/sum(x)

    y_i <- cumsum(a_i)

    z_i <- 0
    
    for (j in 1:i) {   
      z_i[j] <- j/i
    }
    
    j <- 0
    sum_y_i <- 0
    sum_y_i[1] <- y_i[1]+0
    
    for (j in 2:i) {
      sum_y_i[j] <- y_i[j-1]+y_i[j]
    }
    
    G <- 1-1/i*sum(sum_y_i)   
  }
  
  else { # Calculation from Doersam (2004), p. 46-48

    n_j <- weighting
    n <- sum (n_j)
    n_j_n <- n_j/n
    z_j <- cumsum (n_j_n)
    x_j <- x_sort
    x_j_n_j <- (x_j*n_j)/sum(x_j*n_j)
    y_j <- cumsum(x_j_n_j)

    j <- 0
    sum_y_j <- 0
    
    sum_y_j[1] <- (y_j[1])
    
    for (j in 2:i) {
      sum_y_j[j] <- (y_j[j-1]+y_j[j])
    }
    
    G <- 1-sum(sum_y_j*n_j_n)
  }
  
  if (lc == TRUE) { 
    lorenz (x, weighting = NULL, 
            lcx = lcx, lcy = lcy, lctitle = lctitle, le.col = le.col, lc.col = lc.col, 
            lsize = lsize, ltype = ltype, bg.col = bg.col, bgrid = bgrid, bgrid.col = bgrid.col,
            bgrid.size = bgrid.size, bgrid.type = bgrid.type, 
            lcg = lcg, lcgn = lcgn, lcg.lab.x = lcg.lab.x, lcg.lab.y = lcg.lab.y,
            add.lc = add.lc)
  }
  
  if (coefnorm == FALSE) {   
    return(G)   
  } 
  else {
    G.norm <- (i/(i-1))*G   
    return (G.norm)   
  }
  
}