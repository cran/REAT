shiftp <- function (e_ij1, e_ij2, e_i1, e_i2, e_i3, time1, time2, time3,
                    industry.names = NULL, 
                    print.results = TRUE, 
                    plot.results = FALSE, plot.colours = NULL, plot.title = NULL,
                    plot.portfolio = FALSE, ...)

{
  shift_i <- shifti (e_ij1, e_ij2, e_i1, e_i2, time1, time2,
                     industry.names = industry.names, 
                     shift.method = "Gerfin",
                     gerfin.shifts = "mean",
                     print.results = FALSE, 
                     plot.results = FALSE, plot.colours = NULL, plot.title = NULL,
                     plot.portfolio = FALSE, ...)

  e_i3_growth <- e_i3/e_i2

  e_ij3_ind <- e_i3_growth*e_ij2

  e_ij3 <- e_ij3_ind*shift_i$components[2]
  
  prog <- t(as.matrix(e_ij3))
  colnames(prog) <- colnames(shift_i$components.industry)
  rownames(prog) <- time3

  
  cat ("\n")
  cat ("Shift-Share Prognosis", "\n")
  cat ("Method: Gerfin", "\n")
  cat ("\n")
  cat ("Employment prognosis", "\n")
  
  print(as.data.frame(prog))
  
  cat ("\n")
  
  
  results <- list (components = shift_i$components, components.industry = shift_i$components.industry, growth = shift_i$growth, prog = prog, method = shift_i$method)
  
  invisible(results)
}