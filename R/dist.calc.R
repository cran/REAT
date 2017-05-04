dist.calc <-
function (lat1, lon1, lat2, lon2, unit = "km")
{
  lat1_r <- lat1*pi/180
  lon1_r <- lon1*pi/180
  lat2_r <- lat2*pi/180
  lon2_r <- lon2*pi/180
  
  distance <- 6378 * (acos(sin(lat1_r) * sin(lat2_r) + cos(lat1_r) * cos(lat2_r) * cos(lon2_r-lon1_r)))
 
  if (unit == "m") distance <- distance*1000
  if (unit == "mile") distance <- distance/1.60934
  
  return(distance)
}
