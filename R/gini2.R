gini2 <- function (x, weighting = NULL, coefnorm = FALSE, na.rm = TRUE) {
  
  if (!is.null(weighting))
  {
    
    if (length(x) != length(weighting))
    {
      stop("Frequency and weighting differ in length", call. = FALSE)
    }
  }
  
  if (na.rm == TRUE) {
    
    if (!is.null(weighting)) {
      x_w <- as.matrix(cbind(x, weighting))

      x_w <- x_w[complete.cases(x_w),]

      x <- x_w[,1]
      weighting <- x_w[,2]
    }
    
    else {
      x <- x[!is.na(x)]
    }
    
  }
  
  n <- length (x)

  nN <- 1:n


  x_i <- data.frame(matrix(ncol = 2, nrow = n))
  x_i[,1] <- paste0("i", nN)
  x_i[,2] <- x
  colnames(x_i) <- c("i", "i_val")
  
  x_j <- data.frame(matrix(ncol = 2, nrow = n))
  x_j[,1] <- paste0("j", nN)
  x_j[,2] <- x
  colnames(x_j) <- c("j", "j_val")
  
  if (!is.null(weighting)) {
    x_i$i_w <- weighting/sum(weighting)
    x_j$j_w <- weighting/sum(weighting)
  }
  
  
  x_ij <- merge (x_i[,1], x_j[,1], all.x = TRUE, all.y = TRUE)
  colnames(x_ij) <- c("i", "j")

  x_ij <- merge (x_ij, x_i, by.x = "i", by.y = "i")

  x_ij <- merge (x_ij, x_j, by.x = "j", by.y = "j")


  if (!is.null(weighting)) {
    
    x_mean <- mean2(x, weighting = weighting)
    
    G <- sum(x_ij$i_w*x_ij$j_w*abs(x_ij$i_val-x_ij$j_val))/((2*x_mean))

  }
  else {
    
    x_mean <- mean2(x)
    
    G <- (sum(abs(x_ij$i_val-x_ij$j_val)))/((2*(n^2)*x_mean))

  }
  
  if (coefnorm == TRUE)
  {
    Gn <- (n/(n-1))*G
    return(Gn)
  }
  
  else {
    
    return(G)  
    
  }
  
  
}