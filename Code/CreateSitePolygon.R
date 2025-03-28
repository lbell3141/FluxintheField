#create site polygons 

library(dplyr)
library(sf)
library(purrr)

badm <- read.csv(PathtoBADM)

loca_data <- data.frame(SiteID = c("US-SRM", "US-Jo1"),
                        Lat = c(31.8214,32.582),
                        Long = c(-110.8661, -106.635),
                        Diameter = c(360, 200))
library(dplyr)
library(sf)
library(purrr)

# Function to create square polygon with proper coordinate conversion
create_square <- function(lon, lat, diameter) {
  deg_per_meter <- 1 / 111320  # Convert meters to degrees for latitude
  half_side_deg <- (diameter / 2) * deg_per_meter
  half_side_lon <- half_side_deg / cos(lat * pi / 180)  # Adjust for longitude
  
  coords <- matrix(
    c(lon - half_side_lon, lat - half_side_deg,
      lon + half_side_lon, lat - half_side_deg,
      lon + half_side_lon, lat + half_side_deg,
      lon - half_side_lon, lat + half_side_deg,
      lon - half_side_lon, lat - half_side_deg),  # Close the polygon
    ncol = 2, byrow = TRUE
  )
  
  square <- st_polygon(list(coords))  # Return `sfg` polygon
  
  return(square)
}

# Example data
loca_data <- data.frame(
  SiteID = c("US-SRM", "US-Jo1"),
  Lat = c(31.8214, 32.582),
  Long = c(-110.8661, -106.635),
  Diameter = c(400, 200)  # In meters
)

# Apply function to each row
squares <- pmap(loca_data, ~ create_square(..3, ..2, ..4))

# Convert list to an `sfc` object
squares_sfc <- st_sfc(squares, crs = 4326)

# Create sf object
squares_sf <- loca_data |> mutate(geometry = squares_sfc) |> st_as_sf()

# Create output folder
output_folder <- "output_polygons"
dir.create(output_folder, showWarnings = FALSE)

# Split `sf` object into individual rows and save each as a separate shapefile
squares_list <- split(squares_sf, squares_sf$SiteID)

walk(squares_list, function(polygon) {
  site_id <- polygon$SiteID[1]  # Extract SiteID
  file_path <- file.path(output_folder, paste0(site_id, ".shp"))
  st_write(polygon, file_path, delete_dsn = TRUE)
})

print("Shapefiles saved successfully!")

library(dplyr)
library(sf)
library(raster)

# Create the output directory if it doesn't exist
dir.create("output_rasters", showWarnings = FALSE)

# Create a raster template with 20m resolution
raster_template <- raster(extent(squares_sf), res = 20)

# Set the CRS of the raster to WGS84 (EPSG:4326)
crs(raster_template) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Define UTM zone (you should adjust this based on your data's location)
utm_crs <- CRS("+proj=utm +zone=12 +datum=WGS84")  # Modify this CRS based on your region

# Reproject the polygons to UTM
squares_sf_utm <- st_transform(squares_sf, crs = utm_crs)

# Create a raster template with UTM coordinates (20m resolution)
raster_template_utm <- raster(extent(squares_sf_utm), res = 20)
crs(raster_template_utm) <- utm_crs

# Rasterize each polygon
raster_list <- lapply(1:nrow(squares_sf_utm), function(i) {
  # Extract the polygon
  polygon <- squares_sf_utm[i, ]
  
  # Rasterize the polygon (set value as 1 for the polygon)
  rasterized <- rasterize(polygon, raster_template_utm, field = 1, background = NA)
  
  # Save the raster to a file
  file_path <- file.path("output_rasters", paste0(squares_sf_utm$SiteID[i], "_raster.tif"))
  writeRaster(rasterized, file_path, overwrite = TRUE)
  
  return(rasterized)
})

print("Rasters saved successfully!")