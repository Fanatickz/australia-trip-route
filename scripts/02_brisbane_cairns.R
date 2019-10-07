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
sections <- list.files("coordinates/02")

# Load csv with location names
locations <- read.csv("places/03_locations.csv", sep = ",")

# Function for parsing gpx data to data.frame
gpx_to_df <- function(file_name){
  route_raw <- 
    xmlTreeParse(paste0("coordinates/02/", file_name)) %>%
    xmlRoot %>%
    xmlToList %>%
    (function(x) x$trk)
  
  a <- route_raw[seq(1, length(route_raw), 2)] %>% unlist() %>% as.numeric()
  b <- route_raw[seq(2, length(route_raw), 2)] %>% unlist() %>% as.numeric()
  data.frame(a,b)
}

# Init helper variables
counter <- 1
route <- NULL

# Set target directory
tar_dir <- "maps/02"
if (!dir.exists(tar_dir)){
  dir.create(tar_dir, recursive = TRUE)
}

# Looping through the gpx files hasn't worked out due the need of adjusting
# maps seperate
# Additionally the zoom and bbox can be fitted individually

###############################################################################
### BRISBANE - NGUNGUN
###############################################################################

number_of_points <- 200
zoom <- 10
hjust <- 0.5
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- new_route

# Set bounding box and zoom level
map_range <- make_bbox(lon = c(152.65, 153.33), lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### NGUNGUN - AUSTRALIA ZOO
###############################################################################

number_of_points <- 200
zoom <- 10
hjust <- 0.5
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = c(152.65, 153.33), lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### AUSTRALIA ZOO - HERVEY BAY
###############################################################################

number_of_points <- 200
zoom <- 9
hjust <- 0.5
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = c(152.65, 153.33), lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### HERVEY BAY - FRASER ISLAND
###############################################################################

number_of_points <- 200
zoom <- 9
hjust <- 0.5
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
#new_route <- gpx_to_df(sections[counter])
#new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
#names(new_route) <- c("lon", "lat")

# Combine with old route
#route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = c(152.65, 153.33), lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  geom_line(data = locations[counter:(counter+1),], aes(longitude, latitude), linetype = "dashed") +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

locations[locations$location == 'Fraser Island',]$latitude <- NA
locations[locations$location == 'Fraser Island',]$longitude <- NA
old_route <- route
counter <- counter + 1


###############################################################################
### FRASER ISLAND - RAINBOW BEACH
###############################################################################

number_of_points <- 200
zoom <- 9
hjust <- 0.5
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = c(152.65, 153.33), lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### RAINBOW BEACH - NOOSA
###############################################################################

number_of_points <- 200
zoom <- 9
hjust <- 0.5
vjust <- "top"

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = c(152.35, 153.53), lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### NOOSA - BUNDABERG
###############################################################################

number_of_points <- 400
zoom <- 9
hjust <- 0.5
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- new_route

# Set bounding box and zoom level
map_range <- make_bbox(lon = c(152.35, 153.53), lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

locations[2:counter,]$latitude <- NA
locations[2:counter,]$longitude <- NA
old_route <- route
counter <- counter + 1

###############################################################################
### BUNDABERG - AGNES WATER
###############################################################################

number_of_points <- 200
zoom <- 9
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = c(152.35, 153.53), lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=6,height=7.75, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### AGNES WATER - WHITSUNDAYS
###############################################################################

number_of_points <- 200
zoom <- 8
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .50)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+2),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[counter:(counter+2),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=4,height=5.5, units="in", res=500)
print(roadtrip)
dev.off()

locations[locations$location == 'Whitsundays',]$latitude <- NA
locations[locations$location == 'Whitsundays',]$longitude <- NA

old_route <- route
counter <- counter + 2

###############################################################################
### WHITSUNDAYS - TOWNSVILLE
###############################################################################

number_of_points <- 200
zoom <- 7
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter-1])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .50)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", 10, ".png"),width=4,height=5.5, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route

###############################################################################
### TOWNSVILLE - WALLAMAN FALLS
###############################################################################

number_of_points <- 200
zoom <- 7
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .50)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+2),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+2),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=4,height=5.5, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### WALLAMAN FALLS - MISSION BEACH
###############################################################################

number_of_points <- 200
zoom <- 7
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .50)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+2),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+2),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=4,height=5.5, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### MISSION BEACH - MOSSMAN GORGE
###############################################################################

number_of_points <- 200
zoom <- 7
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .50)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+2),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+2),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=4,height=5.5, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### MOSSMAN GORGE - DAINTREE
###############################################################################

number_of_points <- 200
zoom <- 7
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .50)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+2),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+2),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=4,height=5.5, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### DAINTREE - CAPE TRIBULATION
###############################################################################

number_of_points <- 200
zoom <- 7
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .50)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+2),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+2),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=4,height=5.5, units="in", res=500)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### CAPE TRIBULATION - CAIRNS
###############################################################################

number_of_points <- 200
zoom <- 7
hjust <- "left"
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .50)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+2),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+2),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0(tar_dir ,"/", counter, ".png"),width=4,height=5.5, units="in", res=500)
print(roadtrip)
dev.off()