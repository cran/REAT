hoover <- function (x, ref = NULL, weighting = NULL, output = "HC", na.rm = TRUE)
{
  
  n <- nrow(as.matrix(x))

  if ((!is.null(ref)) && (n != nrow(as.matrix((ref))))) {
    stop("Frequency and reference distribution differ in length", call. = FALSE)
  } 
  
  if ((!is.null(weighting)) && (n != nrow(as.matrix((weighting))))) {
    stop("Frequency and reference distribution differ in length", call. = FALSE)
  }
  
  if ((!is.null(ref)) && (!is.null(weighting)) && (nrow(as.matrix((weighting))) != nrow(as.matrix((ref))))) {
    stop("Weighting and reference distribution differ in length", call. = FALSE)
  }
  
  hooverworkfile <- matrix (ncol = 7, nrow = n)
  
  hooverworkfile[,1] <- x
  
  if (is.null(ref)) {
    hooverworkfile[,2] <- rep(1, n)
  } 
  else {
    hooverworkfile[,2] <- ref
  }
  
  if (is.null(weighting)) {
    hooverworkfile[,3] <- rep(0, n)
  }
  else {
    hooverworkfile[,3] <- weighting
  }
  
  hooverworkfile[1:n, 4:7] <- 1

  
  if (na.rm == TRUE) {
    
    hooverworkfile <- hooverworkfile[complete.cases(hooverworkfile),]

    n <- nrow (hooverworkfile)

  }
  
  
  hooverworkfile[,4] <- hooverworkfile[,1]/(sum((hooverworkfile[,1]), na.rm = TRUE))

  hooverworkfile[,5] <- hooverworkfile[,2]/(sum((hooverworkfile[,2]), na.rm = TRUE))

  if (!is.null(weighting)) {
    hooverworkfile[,6] <- hooverworkfile[,3]/(sum((hooverworkfile[,3]), na.rm = TRUE))
  }
  else {
    hooverworkfile[,6] <- rep(1, n)
  }
  
  
  colnames (hooverworkfile) <- c("x", "r", "w", "x_shares", "r_shares", "w_shares", "diff_xs_rs")
  rownames (hooverworkfile) <- 1:n
  
  hooverworkfile[,7] <- hooverworkfile[,6]*(abs(hooverworkfile[,4]-hooverworkfile[,5]))

  x_comp_sum <- sum(hooverworkfile[,7])

  HC <- x_comp_sum/2


  if (output == "data") {
    return(hooverworkfile)
  }
  else {
    return(HC)  
  }
  
}