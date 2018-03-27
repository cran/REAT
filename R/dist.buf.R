dist.buf <-
function (startpoints, sp_id, lat_start, lon_start, endpoints, ep_id, lat_end, lon_end, ep_sum = NULL, bufdist = 500, extract_local = TRUE, unit = "m")
{
  
  distmat_workfile <- dist.mat(startpoints, sp_id, lat_start, lon_start, endpoints, ep_id, lat_end, lon_end, unit = "m")
  distmat_workfile <- distmat_workfile[!is.na(distmat_workfile$distance),]

  if (extract_local == TRUE)
  {
    distmat_workfile <- distmat_workfile[as.character(distmat_workfile$from) != as.character(distmat_workfile$to),]
  }
  
  

  i <- 0

  for (i in 1:nrow(distmat_workfile))
  {
    
    if (distmat_workfile$distance[i] <= bufdist) distmat_workfile$count[i] <- 1
    else distmat_workfile$count[i] <- 0

  }
  
  count_table <- aggregate(distmat_workfile$count, by = list(distmat_workfile$from), FUN = sum)
  colnames(count_table) <- c("from", paste0("count_", names(endpoints[ep_id])))

  

  if (!is.null(ep_sum))
  {
    count_table_full <- count_table
    
    distmat_workfile_hit <- distmat_workfile[distmat_workfile$count == 1,]

    distmat_workfile_hit_data <- merge(distmat_workfile_hit, endpoints, by.x = "to", by.y = ep_id)

    sum_table <- aggregate(distmat_workfile_hit_data[[ep_sum]], by = list(from=distmat_workfile_hit_data$from), FUN = sum)

    colnames(sum_table) <- c("from", paste0("sum_", names(distmat_workfile_hit_data[ep_sum])))

    count_table <- merge (count_table, sum_table)

    if (nrow(count_table) < nrow(startpoints))
    {
      count_table <- merge (count_table, count_table_full, by.x = "from", by.y = "from", all.y=TRUE)
      count_table[,2][is.na(count_table[,2])] <- 0
      count_table[,3][is.na(count_table[,3])] <- 0
      count_table[,4] <- NULL
      colnames(count_table) <- c("from", paste0("count_", names(endpoints[ep_id])), paste0("sum_", names(distmat_workfile_hit_data[ep_sum])))
    }
    
  }
  
  return(count_table)
}
