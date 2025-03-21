aoi <- as(extent(c(2762179, 2763727, 1184568, 1185640)), "SpatialPolygons")
crs(aoi) <- CRS("+init=epsg:2056")

plot(aoi, border = "blue", lty = 2)
plot(study_area, add = T, lty = 2, border = "red", col = adjustcolor("red", alpha.f = 0.2))
axis(1)
axis(2)

sat <- read_osm(aoi, type = "bing", zoom = 17)

# Check what we downloaded
class(sat)

sat <- as(sat, "Raster")

# Reproject the data to the correct CRS using the "nearest neighbor" (ngb)
# method
sat <- projectRaster(sat, crs = CRS("+init=epsg:2056"), method = "ngb")

plot(sat, col = gray(0:100/100))

sat <- crop(sat, aoi)

# Plot them again
plot(sat, col = gray(0:100/100), nrow = 1)

plotRGB(sat, r = 1, g = 2, b = 3, axes = F)
plot(study_area, add = T, lty = 2, border = "red", col = adjustcolor("red", alpha.f = 0.2))
north.arrow(
  xb     = xmin(sat) + 150
  , yb     = ymin(sat) + 100
  , len    = 25
  , col    = "white"
  , border = "white"
  , tcol   = "white"
)
scalebar(
  type  = "bar"
  , xy    = c(xmax(sat) - 450 , ymin(sat) + 60)
  , d     = 400
  , divs  = 4
  , below = "meters"
  , col   = "white"
)

df <- as.data.frame(sat, xy = T)
head(df)

ggplot(df, aes(x = x, y = y, fill = rgb(red, green, blue, maxColorValue = 255))) +
  geom_raster() +
  scale_fill_identity() +
  coord_sf() +
  theme_minimal() +
  
  # Study area
  geom_sf(
    data        = st_as_sf(study_area)
    , inherit.aes = F
    , col         = "red"
    , fill        = "red"
    , alpha       = 0.2
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "bl"
    , pad_x    = unit(1, "cm")
    , pad_y    = unit(1, "cm")
    , style    = north_arrow_fancy_orienteering(
      fill      = c("white", "white")
      , line_col  = NA
      , text_col  = "white"
      , text_size = 12
    )
  ) +
  
  # Scalebar
  annotation_scale(
    location = "br"
    , pad_x    = unit(1, "cm")
    , pad_y    = unit(1, "cm")
    , text_col = "white"
  )
tm_shape(sat) +
  tm_rgb() +
  
  # Study area
  tm_shape(study_area) +
  tm_polygons(col = "red", alpha = 0.2, border.col = "red", border.alpha = 1) +
  
  # North arrow
  tm_compass(
    position   = c("left", "bottom")
    , text.color = "white"
    , color.dark = "white"
  ) +
  
  # Scalebar
  tm_scale_bar(
    position   = c("right", "bottom")
    , text.color = "white"
  ) +
  tm_graticules(lines = F)