#make CHM for SRG

library(lidR)
library(terra)

LidarPath <- "Z:/Drone_Wim/SRERGrass5-28-25/SRERLidar5-28-25/lidars/terra_las/cloud_merged.las"

lidar <- readLAS(LidarPath)

lidar <- classify_ground(lidar, csf())

las_norm <- normalize_height(lidar, tin())

chm_raster <- rasterize_canopy(las_norm, res = 1, algorithm = p2r(subcircle = 0.2))

chm_raster[chm_raster < 0 | chm_raster > 20] <- NA

writeRaster(chm_raster, "./testrest.tif", overwrite = T)
