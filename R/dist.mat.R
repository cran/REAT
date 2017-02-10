dist.mat <-
function (startpoints, sp_id, lat_start, lon_start, endpoints, ep_id, lat_end, lon_end, unit = "km")
{
  startpoints_count <- as.numeric(nrow(startpoints))
  
  endpoints_count <- as.numeric(nrow(endpoints))
  
  startpoints_df <- data.frame(startpoints[[sp_id]], startpoints[[lat_start]], startpoints[[lon_start]])
  colnames(startpoints_df) <- c("s_id", "s_lat", "s_lon")

  endpoints_df <- data.frame(endpoints[[ep_id]], endpoints[[lat_end]], endpoints[[lon_end]])
  colnames(endpoints_df) <- c("e_id", "e_lat", "e_lon")

  mat_startend <- merge (startpoints_df, endpoints_df)

  mat_nr <- nrow(mat_startend)

  i <- 0
  
  distance <- vector()
  from <- vector()
  to <- vector()
  from_to <- vector()
  
  for (i in 1:mat_nr)
  {
    from[i] <- as.character(mat_startend$s_id[i])
    to[i] <- as.character(mat_startend$e_id[i])
    from_to[i] <- paste0(from[i], "-", to[i])

    distance[i] <- dist.calc(mat_startend$s_lat[i], mat_startend$s_lon[i], mat_startend$e_lat[i], mat_startend$e_lon[i], unit = unit)
  }
  
  distmat <- data.frame(from, to, from_to, distance)
  return(distmat)
}
