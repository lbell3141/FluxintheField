library(terra)
library(dplyr)
library(treetop)
library(sf)
library(readr)

tifs <- list.files("./Data/NEONCHMtiffs/lidartifs", pattern = "\\.tif$", full.names = TRUE)
rasters <- lapply(tifs, rast)

fullrast <- do.call(mosaic, rasters)
#plot(fullrast)

#writeRaster(fullrast, "./Data/NEONCHMtiffs/stitched_raster.tif", overwrite = TRUE)

plotext <- vect("output_polygons/US-SRM.shp")

plotext_utm <- project(plotext, crs(fullrast))
plotCHM <- crop(fullrast, plotext_utm)
plotCHM <- mask(plotCHM, plotext_utm)
#plot(plotCHM)
#writeRaster(plotCHM, "./Data/NEONCHMtiffs/tower_CHM.tif", overwrite = TRUE)

#add sampling plots
points <- vect("./plot_points_shp/points.shp")
#create 20mx20m square buffer with point as centroid
coords <- crds(points)
squares <- vect(lapply(1:nrow(coords), function(i) {
  x <- coords[i, 1]
  y <- coords[i, 2]
  ext(x - 10, x + 10, y - 10, y + 10) |> as.polygons()
}), crs = crs(points))

#plot CHM with sample plots
plot(plotCHM)
plot(squares, add = TRUE, border = "red", lwd = 2)
#points(points, pch = 20, col = "blue")

#-------------------------------------------------------------------------------
#calc biometric values for each plot: avg woody cover height, avg herb cover, % ground cover
chm <- plotCHM
sqrs <- squares

chm_vals <- extract(chm, squares, cells = TRUE)
chm_vals <- chm_vals%>%
  rename(CHM = NEON_D14_SRER_DP3_501000_3518000_CHM,
         PlotID = ID)

summary_df <- chm_vals %>%
  group_by(PlotID) %>%
  summarise(
    woody_height = mean(ifelse(CHM > 1, CHM, NA), na.rm = TRUE),
    herb_height  = mean(ifelse(CHM > 0.1 & CHM <= 1, CHM, NA), na.rm = TRUE),
    ground_cover = sum(CHM <= 0.1, na.rm = TRUE) / n() * 100
  )

centroids <- centroids(squares)
coords <- crds(centroids)
summary_df$x <- coords[, 1]
summary_df$y <- coords[, 2]

summary_df <- summary_df %>%
  rename(
    plot = PlotID,
    centroid_x = x,
    centroid_y = y,
    avg_woody_height = woody_height,
    avg_herb_height = herb_height,
    percent_ground_cover = ground_cover
  )%>%
  select(plot, centroid_x, centroid_y, everything())
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
library(remotes)
install_github("https://github.com/carlos-alberto-silva/weblidar-treetop", dependencies = TRUE)

library(treetop)
launchApp(launch.browser = TRUE)
plot(chm)


#-------------------------------------------------------------------------------
#csv with tree locations from treetop 
treelocs <- read.csv("./Treestower_CHM.csv")
tree_pts <- st_as_sf(treelocs, coords = c("x", "y"), crs = st_crs(squares))
squares_sf <- st_as_sf(squares)
squares_sf$PlotID <- paste0(seq_len(nrow(squares_sf)))

trees_in_plots <- st_join(tree_pts, squares_sf, join = st_within) %>%
  filter(!is.na(PlotID))


treetop_summary <- trees_in_plots %>%
  group_by(PlotID) %>%
  summarise(
    tree_count = n(),
    avg_tree_height = mean(Height, na.rm = TRUE)
  ) %>%
  rename(plot = PlotID)%>%
  mutate(plot = as.numeric(plot))

final_summary <- summary_df %>%
  left_join(treetop_summary, by = "plot")

