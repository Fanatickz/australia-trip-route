# This small project uses the coordinates of stops during my Australia Roadtrip
# in 2018 to create a set of maps for a photo album

# Source dependencies
library("magrittr")
library("XML")
library("dplyr")
library("purrr")
library("ggplot2")
library("ggmap")
library("kmlShape")

# Get list of sections
sections <- list.files("coordinates/03")

# Load csv with location names
locations <- read.csv("places/04_locations.csv", sep = ",")

# Set target directory
tar_dir <- "maps/03"
if (!dir.exists(tar_dir)){
  dir.create(tar_dir, recursive = TRUE)
}

###############################################################################
### MUNICH -SYDNEY
###############################################################################

zoom <- 4
hjust <- 0.5
vjust <- -0.3
loc_temp <- locations[1:2,]

# Set bounding box and zoom level
map_range <- make_bbox(lon = loc_temp$longitude, lat = loc_temp$latitude, f = .20)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_line(data = loc_temp, aes(longitude, latitude), linetype = "dashed") +
  geom_point(data = loc_temp, aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = loc_temp, aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", 1, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

###############################################################################
### BRISBANE - FIJI
###############################################################################

zoom <- 5
hjust <- 0.5
vjust <- -0.3
loc_temp <- locations[3:4,]

# Set bounding box and zoom level
map_range <- make_bbox(lon = loc_temp$longitude, lat = loc_temp$latitude, f = 1.7)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_line(data = loc_temp, aes(longitude, latitude), linetype = "dashed") +
  geom_point(data = loc_temp, aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = loc_temp, aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", 2, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

###############################################################################
### CAIRNS - SINGAPORE
###############################################################################

zoom <- 5
hjust <- 0.5
vjust <- -0.3
loc_temp <- locations[5:6,]

# Set bounding box and zoom level
map_range <- make_bbox(lon = loc_temp$longitude, lat = loc_temp$latitude, f = 1.5)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_line(data = loc_temp, aes(longitude, latitude), linetype = "dashed") +
  geom_point(data = loc_temp, aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = loc_temp, aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", 3, ".png"),width=8,height=11, units="in", res=500)
print(roadtrip)
dev.off()