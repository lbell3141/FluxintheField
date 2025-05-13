#delineate drone footprints 

library(sf)

# Define point coordinates (lon, lat) and buffer side length in meters
coords <- data.frame(
  site = c("SRM", "SRG"),
  lon = c(-110.8661, -110.8277),
  lat = c(31.8214, 31.7894),
  extent_m = c(400, 210)
)

# Convert to sf point object (in geographic coordinates)
points_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

# Reproject to a projected CRS in meters (use UTM zone for southern Arizona, e.g., EPSG:32612)
points_proj <- st_transform(points_sf, crs = 32612)

# Create square buffers around each point
square_buffers <- st_buffer(points_proj, dist = coords$extent_m / 2, endCapStyle = "SQUARE")

# Optional: transform back to WGS84 for export/plotting
square_buffers_wgs84 <- st_transform(square_buffers, crs = 4326)

# Plot (quick check)
plot(st_geometry(square_buffers_wgs84), border = "blue")
plot(st_geometry(points_sf), add = TRUE, col = "red", pch = 16)

# Add site names as IDs
square_buffers$site <- points_proj$site
points_proj$site <- points_proj$site  # already present

# Extract vertices and tag them
library(dplyr)

square_vertices_df <- square_buffers %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(site = rep(square_buffers$site, each = 5),  # 5 = 4 corners + repeat of first
         vertex_id = rep(1:5, times = nrow(square_buffers))) %>%
  select(site, vertex_id, X, Y)

# Create directory to hold outputs
dir.create("shapefiles", showWarnings = FALSE)

# Save individual points and squares
for (i in 1:nrow(points_proj)) {
  site_name <- points_proj$site[i]
  
  # Save point
  st_write(points_proj[i, ], paste0("shapefiles/", site_name, "_point.shp"), delete_layer = TRUE)
  
  # Save square buffer
  st_write(square_buffers[i, ], paste0("shapefiles/", site_name, "_square.shp"), delete_layer = TRUE)
}
write.csv(square_vertices_df, "shapefiles/square_vertices.csv", row.names = FALSE)
