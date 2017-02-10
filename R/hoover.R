hoover <-
function (inc, pop = NULL)
{
  inc_share <- inc/sum(inc)

  if (is.null(pop))
  {
    pop_share <- 1/length(inc)
  }
  else { pop_share <- pop/sum(pop) }

  share_comp <- abs(inc_share-pop_share)
  share_comp_sum <- sum(share_comp)

  CI <- share_comp_sum/2

  return(CI)
}
