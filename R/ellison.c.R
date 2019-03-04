ellison.c <- function (e_ik, industry, region, e_j = NULL, c.industries = NULL) {

  ellison.a_i <- ellison.a2 (e_ik, industry, region, print.results = FALSE)

  gamma_i <- data.frame(rownames(ellison.a_i), ellison.a_i[,1])
  colnames(gamma_i) <- c("i_industry", "gamma_i")
  H_i <- data.frame(rownames(ellison.a_i), ellison.a_i[,5])
  colnames(H_i) <- c("i_industry", "H_i")

  
  ij_dummies <- merge(levels(as.factor(industry)), levels(as.factor(region)), all = TRUE)
  ij_dummies[,3] <- 0
  ij_dummies <- ij_dummies[c(3,1,2)]
  colnames(ij_dummies) <- c("e_ik", "industry", "region")

  ellisonworkfile <- data.frame(e_ik, industry, region) 
  ellisonworkfile <- rbind(ellisonworkfile, ij_dummies)
  ellisonworkfile <- ellisonworkfile[order(ellisonworkfile$region, ellisonworkfile$industry),]

  
  I <- length(levels(as.factor(ellisonworkfile$industry)))
  i_names <- as.character(levels(as.factor(ellisonworkfile$industry)))

  K <- nrow(as.matrix((ellisonworkfile)))

  J <- length(levels(as.factor(ellisonworkfile$region)))
  j_names <- levels(as.factor(ellisonworkfile$region))
  j_names <- factor(ellisonworkfile$region, levels = unique(ellisonworkfile$region))
  j_names <- levels(j_names)

  
  if (!is.null(e_j)) {

    e_j2 <- data.frame(region, e_j)
    e_j <- aggregate (e_j2$e_j, by = list(e_j2$region), FUN = mean)

    e <- sum(e_j[,2])
  }
  else {
    e <- sum (ellisonworkfile$e_ik)
    e_j <- aggregate (ellisonworkfile$e_ik, by = list(ellisonworkfile$region), FUN = sum)
  }
  
  colnames(e_j) <- c("j_region", "e_j")

  e_j$s_j <- e_j$e_j/e

  
  e_i <- aggregate (ellisonworkfile$e_ik, by = list(ellisonworkfile$industry), FUN = sum)
  colnames(e_i) <- c("i_industry", "e_i")

  e_i$s_i <- e_i$e_i/e

  
  e_ij <- aggregate (ellisonworkfile$e_ik, by = list(ellisonworkfile$industry, ellisonworkfile$region), FUN = sum)
  colnames(e_ij) <- c("i_industry", "j_region", "e_ij")
  
  e_ij <- merge (e_ij, e_i, by.x = "i_industry", by.y = "i_industry", all.x = TRUE)

  
  e_j2 <- merge (e_j, e_ij, by.x = "j_region", by.y = "j_region", all.x = TRUE)

  
  if (!is.null(c.industries)) {
    e_j2 <- e_j2[e_j2$i_industry %in% c.industries,]
  }
  
  
  e_j2$x_ij <- e_j2$e_ij/e_j2$e_i
  x_j <- aggregate(e_j2$x_ij, by = list (e_j2$j_region), FUN = sum)
  colnames(x_j) <- c("j_region", "x_j")
  
  
  e_j <- merge(e_j, x_j, by.x = "j_region", by.y = "j_region")
  e_j$diffxs <- e_j$x_j-e_j$s_j
  e_j$diffxs2 <- e_j$diffxs^2
  G <- sum(e_j$diffxs2)
  
  if (!is.null(c.industries)) {
    e_i <- e_i[e_i$i_industry %in% c.industries,]
    H_i <- H_i[H_i$i_industry %in% c.industries,]
  }
  
  
  e_i <- merge (e_i, H_i, by.x = "i_industry", by.y = "i_industry")
  H_U <- sum((e_i$s_i^2)*(e_i$H_i))

  
  sum_sj2 <- sum(e_j$s_j^2)
  sum_si2 <- sum(e_i$s_i^2)
  
  gamma_c <- (G/(1-sum_sj2)-H_U-(sum((gamma_i$gamma_i)*sum_si2*(1-(H_i$H_i)))))/(1-(sum(sum_si2)))

  return(gamma_c)
  
  
}