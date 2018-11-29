gifpro <-
function (e_ij, a_i, sq_ij, rq_ij, ru_ij = NULL, ai_ij, time.base, tinterval = 1, industry.names = NULL, output = "short") {

  industries <- length(e_ij)

  if (is.null(industry.names)) {
    industry.names <- as.character(1:industries)
  }
  
  if (is.null(ru_ij)) {
    ru_ij <- vector(length = industries)
    ru_ij <- rep(c(0), times = industries)
  }
  
  i <- 0
  time.count <- vector(length = tinterval)

  e_ij_sq_t <- matrix(nrow = industries, ncol = tinterval)

  e_ij_rq_t <- matrix(nrow = industries, ncol = tinterval)

  e_ij_ru_t <- matrix(nrow = industries, ncol = tinterval)

  e_ij_t <- matrix(nrow = industries, ncol = tinterval)

  carea_ij_t <- matrix(nrow = industries, ncol = tinterval)

  
  for (i in 1:tinterval) {
    e_ij_sq_t[,i] <- ((e_ij*(a_i/100))*(sq_ij/100)) 

    e_ij_rq_t[,i] <- ((e_ij*(a_i/100))*(rq_ij/100)) 
 
    e_ij_t[,i] <- e_ij_sq_t[,i]+e_ij_rq_t[,i]

    e_ij_ru_t[,i] <- ((e_ij_t[,i])*(ru_ij/100))

    carea_ij_t[,i] <- ((e_ij_t[,i])-(e_ij_ru_t[,i]))*ai_ij

    time.count[i] <- time.base+i
  }
  
  rownames(e_ij_sq_t) <- industry.names
  colnames(e_ij_sq_t) <- time.count
  
  rownames(e_ij_rq_t) <- industry.names
  colnames(e_ij_rq_t) <- time.count

  rownames(e_ij_ru_t) <- industry.names
  colnames(e_ij_ru_t) <- time.count
  
  rownames(e_ij_t) <- industry.names
  colnames(e_ij_t) <- time.count
  
  components <- list(resettlement = e_ij_sq_t, relocation = e_ij_rq_t, reuse = e_ij_ru_t, employment = e_ij_t)

  rownames(carea_ij_t) <- industry.names
  colnames(carea_ij_t) <- time.count
  
  carea_j_t <- matrix(ncol = 2, nrow = tinterval)
  carea_j_t[,1] <- colSums(e_ij_t)
  carea_j_t[,2] <- colSums(carea_ij_t)
  rownames(carea_j_t) <- time.count
  colnames(carea_j_t) <- c("Employment", "CommercialArea")
 
  carea_j <- matrix(ncol = 2, nrow = 2)
  carea_j[1,] <- colSums(carea_j_t)
  carea_j[2,] <- carea_j[1,]/tinterval 
  rownames(carea_j) <- c("Sum", "Average")
  colnames(carea_j) <- c("Employment", "CommercialArea")
  
  
  results <- list(peryear = carea_j_t, allover = carea_j)
  
  if (!is.null(output)) {

  cat ("\n")
  cat ("GIFPRO", "\n")
  cat ("Method: GIFPRO", "\n")
  cat ("\n")
  cat ("Employment and commercial area changes (allover)", "\n")
  print(as.data.frame(carea_j))
  cat ("\n")
  
  if (output == "full")
  {

    cat ("Employment and commercial area changes (per time unit)", "\n")
    print(as.data.frame(carea_j_t))
    cat ("\n")
    
  }

  cat ("Calculation for", industries, "industries", "\n")
  cat ("\n")  

  }
  
  invisible(list(components = components, results = results))
  
}
