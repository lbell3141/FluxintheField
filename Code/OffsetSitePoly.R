#offset site extent box based on prevailing wind direction 

loca_data <- data.frame(SiteID = c("US-SRM", "US-Jo1"),
                        Lat = c(31.8214,32.582),
                        Long = c(-110.8661, -106.635),
                        Diameter = c(400, 200),
                        PrevWind = c(240, 170))

# Function to create a square polygon with offset from the tower's coordinates
create_offset_square <- function(lon, lat, diameter, offset_distance, wind_direction_deg) {
  # Convert meters to degrees for latitude and longitude
  deg_per_meter <- 1 / 111320  # Convert meters to degrees for latitude
  half_side_deg <- (diameter / 2) * deg_per_meter
  half_side_lon <- half_side_deg / cos(lat * pi / 180)  # Adjust for longitude
  
  # Convert wind direction (degrees) to radians for the offset calculation
  wind_direction_rad <- pi * wind_direction_deg / 180
  
  # Calculate the offset in meters (convert to degrees)
  offset_lon_deg <- (offset_distance * cos(wind_direction_rad)) * deg_per_meter
  offset_lat_deg <- (offset_distance * sin(wind_direction_rad)) * deg_per_meter
  
  # Adjust the coordinates based on the wind direction offset
  new_lon <- lon + offset_lon_deg
  new_lat <- lat + offset_lat_deg
  
  # Now, create the square around the new coordinates (offset tower position)
  coords <- matrix(
    c(new_lon - half_side_lon, new_lat - half_side_deg,
      new_lon + half_side_lon, new_lat - half_side_deg,
      new_lon + half_side_lon, new_lat + half_side_deg,
      new_lon - half_side_lon, new_lat + half_side_deg,
      new_lon - half_side_lon, new_lat - half_side_deg),  # Close the polygon
    ncol = 2, byrow = TRUE
  )
  
  square <- st_polygon(list(coords))  # Return `sfg` polygon
  
  return(square)
}
