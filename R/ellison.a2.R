ellison.a2 <- function (e_ik, industry, region, print.results = TRUE) {

  ij_dummies <- merge(levels(as.factor(industry)), levels(as.factor(region)), all = TRUE)
  ij_dummies[,3] <- 0
  ij_dummies <- ij_dummies[c(3,1,2)]
  colnames(ij_dummies) <- c("e_ik", "industry", "region")

  ellisonworkfile <- data.frame(e_ik, industry, region) 
  
  ellisonworkfile <- rbind(ellisonworkfile, ij_dummies)

  ellisonworkfile <- ellisonworkfile[order(ellisonworkfile$region, ellisonworkfile$industry),]

  e_j <- aggregate (ellisonworkfile$e_ik, by = list(ellisonworkfile$region), FUN = sum)
  colnames(e_j) <- c("j_region", "e_j")
  
  I <- length(levels(as.factor(ellisonworkfile$industry)))
  i_names <- as.character(levels(as.factor(ellisonworkfile$industry)))

  K <- nrow(as.matrix((ellisonworkfile)))

  J <- length(levels(as.factor(region)))

  i <- 0
  
  EG <- matrix(ncol = 5, nrow = I)
  
  for (i in 1:I) {
    industrydata <- ellisonworkfile[ellisonworkfile$industry == i_names[i],]
    industrydata <- merge(industrydata, e_j, by.x = "region", by.y = "j_region")
    EG[i,] <- ellison.a(industrydata$e_ik, e_j[,2], industrydata$region, print.results = FALSE)
  }
  
  EG[,4] <- EG[,4]-(nlevels(as.factor(region)))

  colnames(EG) <- c("Gamma i", "G i", "z Gi", "K i", "HHI i")
  rownames(EG) <- i_names
  
  if (print.results == TRUE) {
    
    cat("Ellison-Glaeser Agglomeration Index", "\n")
    cat(paste0("K = ", (K-(I*(nlevels(as.factor(region))))), " firms, I = ", I, " industries, J = ", J, " regions"), "\n")
    cat("\n")
    
    gamma_i_df <- as.data.frame(EG[,1])
    colnames(gamma_i_df) <- c("Gamma i")
    
    print (gamma_i_df)
  }
  
  
  invisible(EG)

  
}