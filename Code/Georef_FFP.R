#convert ffp graphical output to georeferenced contours
library(sf)
library(terra)

chosen_crs = 

ffp_contours_to_mask <- function(xr_list, yr_list, rast) {
  
  #lists needed to store: 1) standard plane coord pairs from ffp output, 2) coords converted to lat/long, and 3) points converted to lines
  coord_pair_list <- list()
  latlon_list <- list()
  lines_list <- list()
  
  #format raw points as numeric matrix with NAs removed to avoid errors
  for (i in 1:length(xr_list)) {
    df <- data.frame(x = as.numeric(xr_list[[i]]), y = as.numeric(yr_list[[i]]))
    df <- na.omit(df)
    coord_pair_list[[i]] <- as.matrix(df)
  }
  
  #convert standard plane coords to lat/lon coords (see notes above)
  for (i in seq_along(coord_pair_list)) {
    lat <- (coord_pair_list[[i]][, 2] / 111111) + 31.6637
    lon <- (coord_pair_list[[i]][, 1] / (111111 * cos(31.6637 * pi / 180))) - 110.1777
    latlon_list[[i]] <- cbind(lon, lat)
  }
  
  #convert points matrix to SpatVects to work with mask function
  for (i in seq_along(latlon_list)) {
    lines_list[[i]] <- vect(latlon_list[[i]], type = "lines")
  }
  
  #add raster CRS to FFP contours
  for (i in seq_along(lines_list)) {
    crs(lines_list[[i]]) <- crs(rast)
  }
  
  #make a list of 30%, 60%, and 90% contours
  part_con_list <- list(lines_list[[3]], lines_list[[6]], lines_list[[9]])
  #make a list of 60%, and 90% contours
  #part_con_list <- list(lines_list[[8]], lines_list[[9]])
  #part_con_list <- list(lines_list[[9]])
  
  #mask the raster with the contours
  masked_rast <- list()
  for (i in seq_along(part_con_list)) {
    masked_rast[[i]] <- mask(rast, part_con_list[[i]])
  }
  
  #subtract inner contours to get different contour groupings
  dif_con_list <- list(masked_rast[[1]], 
                       mask(masked_rast[[2]], masked_rast[[1]], inverse = TRUE),
                       mask(masked_rast[[3]], masked_rast[[2]], inverse = TRUE))
  #dif_con_list <- list(mask(masked_rast[[2]], masked_rast[[1]], inverse = TRUE))
  
  #compute avg values of pixels for each contour grouping
  rap_vals <- list()
  for (i in seq_along(dif_con_list)) {
    rap_vals[[i]] <- global(dif_con_list[[i]], fun = mean, na.rm = TRUE)
  }
  
  #weight contour areas
  veg_cover <- 0.3 * sum(unlist(rap_vals[[1]])) + 0.3 * sum(unlist(rap_vals[[2]])) + 0.3* sum(unlist(rap_vals[[3]]))
  #veg_cover <- 0.6 * sum(unlist(rap_vals[[1]])) + 0.3 * sum(unlist(rap_vals[[2]]))
  #veg_cover <- 1 * sum(unlist(rap_vals[[1]]))
  return(veg_cover)
}