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
  sections <- list.files("coordinates")
  
  # Load csv with location names
  locations <- read.csv("locations.csv", sep = ",")
  locations_round_trip <- read.csv("locations_round_trip.csv", sep = ",")
  
  # Function for parsing gpx data to data.frame
  gpx_to_df <- function(file_name){
    route_raw <- 
      xmlTreeParse(paste0("coordinates/", file_name)) %>%
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
  
  # Looping through the gpx files hasn't worked out due to horrendous errors in
  # the data which leads to zig-zaging that has to corrected manually.
  # Additionally the zoom and bbox can be fitted individually
  
  ###############################################################################
  ### SYDNEY - KATOOMBA
  ###############################################################################
  
  number_of_points <- 200
  zoom <- 9
  hjust <- 0
  vjust <- -0.2
  
  # Use Douglas-Peucker-Algorithm to downscale the GPS route
  new_route <- gpx_to_df(sections[counter])
  new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
  names(new_route) <- c("lon", "lat")
  
  # Set bounding box and zoom level
  map_range <- make_bbox(lon = new_route$lon, lat = new_route$lat, f = 2)
  roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")
  
  # Construct map
  roadtrip <- ggmap(roadmap) +
    geom_line(data = new_route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
    geom_point(data = locations[counter:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
    geom_label(data = locations[counter:(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust, size = 2) +
    theme_void()
  
  # Plot map to pdf
  png(paste0("maps/", counter, ".png"),width=4,height=5.5, units="in", res=600)
  print(roadtrip)
  dev.off()
  
  
  old_route <- new_route
  counter <- counter + 1
  ###############################################################################
  ### SYDNEY - KATOOMBA
  ###############################################################################
  
    number_of_points <- 200
    zoom <- 8
    hjust <- 0.5
    vjust <- -0.3
    
    # Use Douglas-Peucker-Algorithm to downscale the GPS route
    new_route <- gpx_to_df(sections[counter])
    new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
    names(new_route) <- c("lon", "lat")
    
    # Combine with old route
    route <- rbind(old_route, new_route)
    
    # Set bounding box and zoom level
    map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .5)
    roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")
    
    # Construct map
    roadtrip <- ggmap(roadmap) +
      geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
      geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
      geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
      geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
      theme_void()
    
    # Plot map to pdf
    png(paste0("maps/", counter, ".png"),width=4,height=5.5, units="in", res=300)
    print(roadtrip)
    dev.off()
  
  old_route <- route
  counter <- counter + 1
  
  ###############################################################################
  ### KATOOMBA - BYRON BAY
  ###############################################################################
  
  number_of_points <- 200
  zoom <- 7
  hjust <- 0.5
  vjust <- -0.3
  
  # Use Douglas-Peucker-Algorithm to downscale the GPS route
  new_route <- gpx_to_df(sections[counter])
  new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
  names(new_route) <- c("lon", "lat")
  
  # Combine with old route
  route <- rbind(old_route, new_route)
  
  # Set bounding box and zoom level
  map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .75)
  roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")
  
  # Construct map
  roadtrip <- ggmap(roadmap) +
    geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
    geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
    geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=0, vjust=0) +
    geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust="left", vjust=0) +
    theme_void()
  
  # Plot map to pdf
  png(paste0("maps/", counter, ".png"),width=4,height=5.5, units="in", res=300)
  print(roadtrip)
  dev.off()
  
  old_route <- route
  counter <- counter + 1
  
  ###############################################################################
  ### BYRON BAY - SURFERS PARADISE
  ###############################################################################
  
  number_of_points <- 200
  zoom <- 7
  hjust <- 0.5
  vjust <- -0.3
  
  # Use Douglas-Peucker-Algorithm to downscale the GPS route
  new_route <- gpx_to_df(sections[counter])
  new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
  names(new_route) <- c("lon", "lat")
  
  # Combine with old route
  route <- rbind(old_route, new_route)
  
  # Set bounding box and zoom level
  map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .75)
  roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")
  
  # Construct map
  roadtrip <- ggmap(roadmap) +
    geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
    geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
    geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
    geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
    theme_void()
  
  # Plot map to pdf
  png(paste0("maps/", counter, ".png"),width=4,height=5.5, units="in", res=300)
  print(roadtrip)
  dev.off()
  
  old_route <- route
  counter <- counter + 1
  
###############################################################################
### SURFERS PARADISE - BRISBANE
###############################################################################

number_of_points <- 200
zoom <- 7
hjust <- 0.5
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lon", "lat")

# Combine with old route
route <- rbind(old_route, new_route)

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

# Construct map
roadtrip <- ggmap(roadmap) +
  geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
  geom_point(data = locations[0:(counter+1),], aes(longitude, latitude), alpha = 0.85) +
  geom_label(data = locations[(counter+1),], aes(longitude, latitude, label=location),hjust=hjust, vjust=vjust) +
  geom_label(data = locations[1,], aes(longitude, latitude, label=location),hjust=1, vjust=0) +
  theme_void()

# Plot map to pdf
png(paste0("maps/", counter, ".png"),width=4,height=5.5, units="in", res=300)
print(roadtrip)
dev.off()

old_route <- route
counter <- counter + 1

###############################################################################
### END OF LAURAS SOLO TRIP
###############################################################################

###############################################################################
### BRISBANE DAY TRIP TO SOUTH
###############################################################################

number_of_points <- 200
zoom <- 10
hjust <- 0.5
vjust <- -0.3

# Use Douglas-Peucker-Algorithm to downscale the GPS route
new_route <- gpx_to_df(sections[counter])
#new_route <- DouglasPeuckerNbPoints(new_route[,2] ,new_route[,1] ,number_of_points)
names(new_route) <- c("lat", "lon")

# Combine with old route
route <- new_route

# Set bounding box and zoom level
map_range <- make_bbox(lon = route$lon, lat = route$lat, f = .75)
roadmap <- get_stamenmap(bbox = map_range, zoom = zoom, maptype = "watercolor")

  # Construct map
  roadtrip <- ggmap(roadmap) +
    geom_path(data = route, aes(lon, lat), size=1, show.legend = FALSE, linetype = "solid") +
    geom_point(data = locations_round_trip, aes(longitude, latitude), alpha = 0.85) +
    geom_label(data = locations_round_trip, aes(longitude, latitude, label=location),hjust=hjust, vjust=1.2) +
    theme_void()
    
  # Plot map to pdf
  png(paste0("maps/", counter, ".png"),width=4,height=5.5, units="in", res=300)
  print(roadtrip)
  dev.off()

old_route <- NULL
counter <- counter + 1