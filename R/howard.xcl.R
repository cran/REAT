howard.xcl <- function (k, industry, region, industry1, industry2, no.samples = 50, e_k = NULL) {
  
  if (!is.null(e_k)) {
    howardworkfile <- data.frame(k, industry, region, e_k)
  }
  else {
    howardworkfile <- data.frame(k, industry, region)
  }
  
  howardworkfile <- howardworkfile[(howardworkfile$industry == industry1) | (howardworkfile$industry == industry2),]
  
  CL <- howard.cl (howardworkfile$k, howardworkfile$industry, howardworkfile$region, industry1, industry2, e_k)
  
  K_i <- nrow(howardworkfile[howardworkfile$industry == industry1,])
  K_q <- nrow(howardworkfile[howardworkfile$industry == industry2,])
  
  K_iq <- nrow (howardworkfile)
  
  i <- 0
  
  CL_rnd <- vector()
  
  k_random <- data.frame(howardworkfile$k, howardworkfile$industry)
  colnames(k_random) <- c("k", "ind_rnd")
  
  
  for (i in 1:no.samples) {
    
    k_random$rnd <- runif(n = K_iq, min = 0, max = 1)
    
    k_random <- k_random[order(k_random$rnd),] 
    
    howardworkfile$industry <- k_random$ind_rnd
    
    CL_rnd[i] <- howard.cl (howardworkfile$k, howardworkfile$industry, howardworkfile$region, industry1, industry2, e_k)
    
  }
  
  CL_rnd_mean <- mean2(CL_rnd)
  
  XCL <- CL-CL_rnd_mean
  
  return(XCL)
  
}