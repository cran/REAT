gini.conc <-
function (e_ij, e_j, 
                       lc = FALSE, 
                       lcx = "% of objects", lcy = "% of regarded variable", 
                       lctitle = "Lorenz curve", le.col = "blue", lc.col = "black",
                       lsize = 1, ltype = "solid",
                       bg.col = "gray95", 
                       bgrid = TRUE, bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
                       lcg = FALSE, lcgn = FALSE, lcg.caption = NULL, lcg.lab.x = 0, lcg.lab.y = 1, 
                       add.lc = FALSE, plot.lc = TRUE) {

  e_i <- sum(e_ij)

  e <- sum(e_j)

  regions <- length(e_ij)

  C_j <- vector()

  for (r in 1:regions) { 
    C_j[r] <- locq (e_ij[r], e_j[r], e_i, e)   
  }
  
  C_j_sort <- sort(C_j)

  lambda <- 1:(nrow(as.data.frame(C_j_sort)))

  C_mean <- mean(C_j)

  C_j_minus_C_mean <- C_j_sort-C_mean

  sum_C <- sum(lambda*C_j_minus_C_mean)

  G_i <- (2/((regions^2)*C_mean))*sum_C

  if (lc == TRUE) { 

    lorenz (x = e_ij, z = e_j, 
            lcx = lcx, lcy = lcy, lctitle = lctitle, le.col = le.col, lc.col = lc.col, 
            lsize = lsize, ltype = ltype, bg.col = bg.col, bgrid = bgrid, bgrid.col = bgrid.col,
            bgrid.size = bgrid.size, bgrid.type = bgrid.type, 
            lcg = lcg, lcgn = lcgn, lcg.lab.x = lcg.lab.x, lcg.lab.y = lcg.lab.y,
            add.lc = add.lc, plot.lc = plot.lc)
  }
  
  return(G_i)
}
