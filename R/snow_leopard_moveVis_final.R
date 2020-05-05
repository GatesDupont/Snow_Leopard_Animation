library(ggplot2)
library(ggnewscale)
library(dplyr)
library(move)
library(moveVis)
library(sp)
library(leaflet)
library(lubridate)
library(stringr)
library(raster)
select =  dplyr::select


"EXPLORE & FORMAT"

# Load all GPS data
gps <- read.csv("data/gps/ali_sl_gps_fix.csv", row.names = NULL) %>%
  mutate(timestamps = ymd_hms(paste(Date, Time))) %>% # Convert date to POSIXct
  select(X = Longitude,
         Y = ï..Latitude, 
         timestamps) %>%
  mutate(track_id = "A") %>%
  select(X, Y, timestamps, track_id) %>%
  filter(X < 100) # Remove incorrect fixes

# Plot gps
ggplot(data = gps, aes(x=X,y=Y)) +
  geom_point(size=1.75, alpha=0.3, pch = 20) +
  coord_fixed() +
  theme_bw()


"ADD CRS"

# Assign CRS to convert
gps_spdf <- SpatialPointsDataFrame(
  coords = gps[,c("X", "Y")], 
  data = gps,
  proj4string = CRS("+init=epsg:4326"))

plot(gps_spdf)

# Check to make sure it makes sense
leaflet(gps_spdf) %>% addTiles() %>%
  addMarkers()


"GIS data"

# International borders
borders <- readOGR("data/gis/international_border/WorldAdministrativeDivisions.shp") %>%
  crop(., 4*extent(spTransform(gps_spdf, crs(.)))) %>%
  spTransform(., crs(gps_spdf))

# Settlements
settlements <- readOGR("data/gis/settlements/Settelments.shp") %>%
  spTransform(., crs(gps_spdf)) %>%
  crop(4*extent(gps_spdf))

# DEM
dem_raw <- raster("data/gis/dem/Elevation_UTM.tif") %>%
  crop(., 5*extent(spTransform(gps_spdf, crs(.)))) # crop to gps pts

# Reproject dem and crop
dem <- projectRaster(dem_raw, crs = crs(gps_spdf)) %>%
  crop(., 4*extent(gps_spdf))


"COLOR DEM"

# Shading
slope <- terrain(dem, opt="slope", unit='radians')
aspect <- terrain(dem, opt="aspect", unit='radians')
hillshade <- hillShade(slope, aspect)
plot(hillshade, col = gray.colors(20))

# Terrain colors
sl.terrain = colorRampPalette(c("darkgreen", "#4B371C", "#4B371C", "white"))
sl.terrain = colorRampPalette(c("#73866f", "#73866f", "#73866f", "#c0b195", "#c0b195", "#c0b195", "white", "white"))

# Test plot
plot(dem, col = sl.terrain(1000), alpha=1.5, zlim = c(1200, 5400))
plot(borders, lwd=2, add=T)
plot(hillshade, col = gray.colors(1000), alpha = 0.2, add=T, legend = F)
plot(slope, col = gray.colors(1000), alpha = 0.1, add=T, legend = F)
plot(settlements, add=T, pch = 21, col = "white", cex = 1.2)
plot(spTransform(gps_spdf, crs(dem)), add=T, col = "blue", pch = 1, cex=0.5)


"GGPLOT"

# Hillshade raster to df
hills.df <- rasterToPoints(hillshade)
hills.df <- data.frame(hills.df)
colnames(hills.df) <- c("lon", "lat", "hills")

# DEM raster to df
dem.df <- rasterToPoints(dem)
dem.df <- data.frame(dem.df)
colnames(dem.df) <- c("lon", "lat", "elev")

# Slope raster to df
slope.df <- rasterToPoints(slope)
slope.df <- data.frame(slope.df)
colnames(slope.df) <- c("lon", "lat", "slope")
slope.df$slope <- 1- slope.df$slope #invert the scale so that more slope is darker

# Prep admin boundaries
borders.fort <- fortify(borders)

# Add extent
extent.pak <- 1.1*extent(gps_spdf)

# Final plot
ggplot(data = gps, aes(x = X, y = Y)) +
  coord_quickmap(xlim = c(extent.pak@xmin, extent.pak@xmax),
                 ylim = c(extent.pak@ymin, extent.pak@ymax), expand=FALSE) +
  geom_tile(data = dem.df, aes(lon, lat, fill = elev, group=1)) +
  scale_fill_gradientn(colours = sl.terrain(1000)) +
  new_scale("fill") +
  geom_raster(data = hills.df, aes(lon, lat, fill = hills, group=1), alpha = .4, interpolate=TRUE) +
  geom_raster(data = slope.df, aes(lon, lat, fill = slope, group=2), alpha = .1, interpolate=TRUE) +
  scale_fill_gradientn(colours = grey.colors(100), guide='none') +
  geom_polygon(data = borders.fort, aes(x = long, y = lat, group = group), 
               colour = "black", fill = NA, size = 1) +
  geom_text(aes(label = "Pakistan"), x = 71.6, y = 35.95, color = "white", fontface = "bold") +
  geom_text(aes(label = "Afghanistan"), x = 71.25, y = 35.775, color = "white", fontface = "bold") +
  geom_point()


"MOVEVIS"

# Convert from SPDF to DF
gps4move <- gps_spdf %>%
  as.data.frame()

# Convert to move data
gps_move <- df2move(
  gps4move, x = "X.1", y = "Y.1", time = "timestamps", track_id = "track_id",
  proj = "+init=epsg:4326")

# Align move_data to a uniform time scale
sl_move_alinged <- align_move(
  gps_move, 
  res = 480, # should be 240
  digit = 0, 
  unit = "mins") # should be mins

# Start time for raster basemap
starttime = min(gps4move$timestamps)

# Make the map frames
frames.sp <- frames_spatial(
  m = sl_move_alinged, 
  r_list = dem, r_times = starttime, r_type = "gradient", fade_raster = F,
  path_colours = c("red"), path_alpha = 1, path_legend = FALSE, alpha = 1) %>%
  add_gg(frames = ., gg = expr(scale_fill_gradientn(colours = sl.terrain(1000)))) %>%
  add_gg(frames = ., gg = expr(labs(fill="Elevation (m)"))) %>%
  add_gg(frames = ., gg = expr(new_scale("fill"))) %>%
  add_timestamps(type = "label") %>% 
  add_progress() %>%
  add_gg(frames = ., gg = expr(geom_raster(data = hills.df, aes(lon, lat, fill = hills, group=1), alpha = .2, interpolate=TRUE)), data = hills.df) %>%
  add_gg(frames = ., gg = expr(geom_raster(data = slope.df, aes(lon, lat, fill = slope, group=2), alpha = .1, interpolate=TRUE)), data = slope.df) %>%
  add_gg(frames = ., gg = expr(scale_fill_gradientn(colours = grey.colors(100), guide='none'))) %>%
  add_gg(frames = ., gg = expr(geom_polygon(data = borders.fort, aes(x = long, y = lat, group = group), 
                                              colour = "#262322", fill = NA, size = 1)), data = borders.fort) %>%
  #add_gg(frames = ., gg = expr(geom_text(aes(label = "Pakistan"), x = 71.6, y = 35.95, color = "white", fontface = "bold"))) %>%
  #add_gg(frames = ., gg = expr(geom_text(aes(label = "Afghanistan"), x = 71.25, y = 35.7, color = "white", fontface = "bold"))) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% 
  add_text("Pakistan", x = 71.6, y = 35.95, colour = "#262322", size = 6) %>%
  add_text("Afghanistan", x = 71.25, y = 35.775, colour = "#262322", size = 6) %>%
  add_scalebar(height = 0.01)

# Make the graph frames
frames.flow <- frames_graph(m = sl_move_alinged, r_list = dem, r_times = starttime, r_type = "gradient", 
                            fade_raster = F, path_legend = FALSE, graph_type = "flow", val_by = 500, 
                            val_min = 1500, val_max = 5000)

# Checking out frames
frames.length <- sapply(list(frames.sp, frames.flow), length)
frames.length[1] == frames.length[2] # Should be true
frames.sp[[frames.length[1]]]
frames.flow[[frames.length[2]]]

# Joining frames
frames.join <- join_frames(list(frames.sp, frames.flow), ncol = 2, nrow = 1)

# Make final animation
animate_frames(frames.sp, out_file = "output/sl_dem_042920_2.mov")
