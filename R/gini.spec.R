gini.spec <-
function (e_ij, e_i,
                       lc = FALSE, 
                       lcx = "% of objects", lcy = "% of regarded variable", 
                       lctitle = "Lorenz curve", le.col = "blue", lc.col = "black",
                       lsize = 1, ltype = "solid",
                       bg.col = "gray95", 
                       bgrid = TRUE, bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid",
                       lcg = FALSE, lcgn = FALSE, lcg.caption = NULL, lcg.lab.x = 0, lcg.lab.y = 1, 
                       add.lc = FALSE, plot.lc = TRUE)
  {

  e_j <- sum(e_ij)

  e <- sum(e_i)

  industries <- length(e_ij)

  R_i <- vector()

  for (i in 1:industries) { 
    R_i[i] <- locq (e_ij[i], e_j, e_i[i], e)

  }
  
  R_i_sort <- sort(R_i)

  lambda <- 1:(nrow(as.data.frame(R_i_sort)))

  R_mean <- mean(R_i)

  R_i_minus_R_mean <- R_i_sort-R_mean

  sum_R <- sum(lambda*R_i_minus_R_mean)

  G_j <- (2/((industries^2)*R_mean))*sum_R

  if (lc == TRUE) { 

    lorenz (x = e_ij, z = e_i, 
            lcx = lcx, lcy = lcy, lctitle = lctitle, le.col = le.col, lc.col = lc.col, 
            lsize = lsize, ltype = ltype, bg.col = bg.col, bgrid = bgrid, bgrid.col = bgrid.col,
            bgrid.size = bgrid.size, bgrid.type = bgrid.type, 
            lcg = lcg, lcgn = lcgn, lcg.lab.x = lcg.lab.x, lcg.lab.y = lcg.lab.y,
            add.lc = add.lc, plot.lc = plot.lc)
  }
  
  return(G_j)

}
