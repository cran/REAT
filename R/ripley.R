ripley <- function (loc_df, loc_id, loc_lat, loc_lon, 
                    area, t.max, t.sep = 10, 
                    K.local = FALSE, ci.boot = FALSE, ci.alpha = 0.05, ciboot.samples = 100,
                    progmsg = FALSE, 
                    K.plot = TRUE, 
                    Kplot.func = "K",
                    plot.title = "Ripley's K",
                    plotX = "t", 
                    plotY = paste(Kplot.func, "Observed vs. expected"), 
                    lcol.exp = "blue", 
                    lcol.emp = "red",
                    lsize.exp = 1, ltype.exp = "solid",
                    lsize.emp = 1, ltype.emp = "solid",
                    bg.col = "gray95", 
                    bgrid = TRUE, bgrid.col = "white", bgrid.size = 2, bgrid.type = "solid")
{
  
  n <- nrow(as.matrix(loc_df))

  cat("Ripley's K", "\n")
  cat(paste0("n = ", n, " points"), "\n")
  cat("\n")
  
  

  if (progmsg == TRUE) { cat("Calculating distance matrix ...", "\n") }
  
  distbuf <- dist.buf (loc_df, loc_id, loc_lat, loc_lon, loc_df, loc_id, loc_lat, loc_lon, bufdist = t.max, extract_local = TRUE)

  t_val <- 1:t.max

  lambda <- n/area

  
  t_cuts <- 1:t.sep

  t_cuts_val <- rep ((t.max/t.sep), t.sep)

  t_cuts_val_cumsum <- cumsum(t_cuts_val)

  
  if (progmsg == TRUE) { cat("Analyzing point process ...", "\n") }
  
  
  i <- 0
  
  t <- vector()
  K_t_exp <- vector()
  K_t <- vector()
  K_t_diff <- vector()
  L_t <- vector()
  H_t <- vector()
  
  for (i in 1:t.sep) {
    t[i] <- t_cuts_val_cumsum[i]

    I <- sum(distbuf$distmat[distbuf$distmat$distance <= t[i],]$count)

    K_t_exp[i] <- pi*(t[i]^2)

    K_t[i] <- 1/lambda*(sum(I/n))

    
    K_t_diff[i] <- K_t[i]-K_t_exp[i]
    
    L_t[i] <- sqrt((K_t[i]/pi))

    H_t[i] <- L_t[i]-t[i]
  }
  
  K <- matrix (ncol = 6, nrow = t.sep)
  
  K[,1] <- t
  K[,2] <- K_t_exp
  K[,3] <- K_t
  K[,4] <- K_t_diff
  K[,5] <- L_t
  K[,6] <- H_t
  
  colnames(K) <- c("t <=", "K t exp", "K t", "Kt-Kt exp", "L t", "H t")
  rownames(K) <- 1:i
  
  results <- K
  
  
  
  if (ci.boot == TRUE) {
    K.local = TRUE
  }
  

  if (K.local == TRUE) {
    
    if (progmsg == TRUE) { cat("Calculating local values ...", "\n") }
    
    i <- 0
    j <- 0
    
    points_no <- nlevels(as.factor(distbuf$distmat$from))

    points_id <- as.character(levels(as.factor(distbuf$distmat$from)))

    t <- vector()
    
    I_local <- matrix(nrow = n, ncol = t.sep)
    K_local <- matrix(nrow = n, ncol = t.sep)
    L_local <- matrix(nrow = n, ncol = t.sep)
    H_local <- matrix(nrow = n, ncol = t.sep)
    K_local_mean <- vector()
    
    
    
    for (i in 1:t.sep) {
      
      t[i] <- t_cuts_val_cumsum[i]

      for (j in 1:points_no) {
        
        distmat_point <- distbuf$distmat[distbuf$distmat$from == points_id[j],]
        I_local[j,i] <- sum(distmat_point[distmat_point$distance <= t[i],]$count)

        K_local[j,i] <- (1/lambda)*(sum(I_local[j,i]))

        
      }

      
    }
    
    colnames(K_local) <- t
    rownames(K_local) <- points_id
    

    results <- list (K = K, K_local = K_local)
  }
  
  
  if (ci.boot == TRUE) {

    
    m <- 0
    K_local_means <- matrix(ncol = ciboot.samples, nrow = t.sep)
    
    for (m in 1:ciboot.samples) {
      
      K_local_sample <- K_local[sample(nrow(K_local), n, replace = TRUE),]


      K_local_means[,m] <- colMeans(K_local_sample)

    }
    
    colnames(K_local_means) <- 1:ciboot.samples
    rownames(K_local_means) <- t

    l <- 0
    
    local_ci <- matrix (ncol = 2, nrow = t.sep)
    
    for (l in 1:nrow(K_local_means))
    {
      local_ci[l,1:2] <- quantile(K_local_means[l,], c((ci.alpha/2), (1-(ci.alpha/2)))) 
    }
    
    colnames (local_ci) <- c((paste0 ("CI ", (ci.alpha/2))), (paste0("CI ", (1-(ci.alpha/2)))))
    rownames (local_ci) <- t
    
    results <- list (K = K, K_local = K_local, local_ci = local_ci)
    
  }
  
  

  if (K.plot == TRUE) {
    
    if (Kplot.func == "L") {
      plot (x = K[,1], y = K[,1], col = lcol.exp, type = "n", xlab = plotX, ylab = plotY, main = plot.title) 

      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg.col)

      if (bgrid == TRUE)
      {
        grid (col = bgrid.col, lty = bgrid.type, lwd = bgrid.size)
      }   
      
      lines (x = K[,1], y = K[,1], col = lcol.exp, type = "l", lwd = lsize.exp) 

      lines (x = K[,1], y = K[,5], col = lcol.emp, lwd = lsize.emp)

    }
    
    else if (Kplot.func == "H") {
      plot (x = K[,1], y = K[,6], type = "n", xlab = plotX, ylab = plotY, main = plot.title)  

      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg.col)

      if (bgrid == TRUE)
      {
        grid (col = bgrid.col, lty = bgrid.type, lwd = bgrid.size)
      }   
      
      
      lines (x = K[,1], y = K[,6], col = lcol.emp, lwd = lsize.emp)  

      abline (h = 0, col = lcol.exp, lwd = lsize.exp)
    }
    
    else {
      Kplot.func <- "K"
      
      plot (K[,1], K[,2], type = "n", xlab = plotX, ylab = plotY, main = plot.title) 

      
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg.col)

      if (bgrid == TRUE)
      {
        grid (col = bgrid.col, lty = bgrid.type, lwd = bgrid.size)
      }   
      
      lines (K[,1], K[,2], col = lcol.exp, xlab = plotX, ylab = plotY, lwd = lsize.emp) 

      lines (K[,1], K[,3], type = "l", col = lcol.emp, lwd = lsize.exp)  

      if (ci.boot == TRUE) {
        
        lines (K[,1], local_ci[,1], type = "l", col = "green", lwd = lsize.exp)
        lines (K[,1], local_ci[,2], type = "l", col = "green", lwd = lsize.exp)
        
      }
      
    }
    
  }
  

  print(K) 

  invisible(results)

}