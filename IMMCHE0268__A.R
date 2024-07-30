# Load Egypt boundary shapefile using rnaturalearth
egypt <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(admin == "Egypt")

# Define the location of the Khufu Pyramid
khufu_pyramid <- data.frame(name = "Khufu Pyramid", lon = 31.1342, lat = 29.9792)

# Convert to sf object
khufu_pyramid_sf <- st_as_sf(khufu_pyramid, coords = c("lon", "lat"), crs = 4326)

# Coordinates for the Nile River (approximate)
nile_river <- data.frame(
  lon = c(30.5, 30.8, 31.0, 31.2, 31.4, 31.6),
  lat = c(22.0, 24.0, 26.0, 28.0, 30.0, 32.0)
)

# Set seed and generate scatter points
num_points <- 500
set.seed(123)  # For reproducibility

# Define quarries
quarry_khufu <- data.frame(lon = 31.1342, lat = 29.975)
quarry_khafre <- data.frame(lon = 31.0, lat = 29.9)

# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = 31.1342, sd = 0.02), rnorm(num_points * 0.45, mean = 31.0, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = 29.975, sd = 0.02), rnorm(num_points * 0.45, mean = 29.9, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

# Combine the datasets
scatter_points <- rbind(scatter_points_near_quarries, scatter_points_random)

# Create sf objects for quarries
quarry_khufu_sf <- quarry_khufu %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
quarry_khafre_sf <- quarry_khafre %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Create sf object for Nile River as a line
nile_river_coords <- matrix(c(30.5, 22.0,
                              30.8, 24.0,
                              31.0, 26.0,
                              31.2, 28.0,
                              31.4, 30.0,
                              31.6, 32.0), ncol = 2, byrow = TRUE)
nile_river_sf <- st_linestring(nile_river_coords) %>% st_sfc(crs = 4326)

# Create sf object for Khufu Pyramid
khufu_pyramid <- data.frame(
  lon = 31.1342,
  lat = 29.9792
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Convert scatter points to sf object
scatter_points_sf <- scatter_points %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Assuming `egypt` is already an sf object representing the map of Egypt
# If not, you need to load or create it as an sf object

# Plot
ggplot() +
  geom_sf(data = egypt, fill = "lightblue", color = "black") +  # Draw the map of Egypt
  geom_sf(data = scatter_points_sf, color = "blue", size = 0.5, alpha = 0.6) +  # Scatter points with smaller size and transparency
  geom_sf(data = khufu_pyramid, color = "red", size = 3) +  # Khufu Pyramid location
  geom_sf(data = nile_river_sf, color = "blue", size = 4, alpha = 0.5) +  # Nile River as a line
  geom_sf(data = quarry_khufu_sf, color = "green", size = 3) +  # Quarry near Khufu Pyramid
  geom_sf(data = quarry_khafre_sf, color = "purple", size = 3) +  # Khafre Quarry
  annotate("text", x = 31.31, y = 29.85, label = "Nile River", color = "blue", size = 4, angle = 0, vjust = -1) +
  annotate("text", x = 31.1342, y = 29.9792, label = "Khufu Pyramid", color = "red", size = 3, vjust = -1.5) +
  annotate("text", x = 31.1342, y = 29.975, label = "Quarry near Khufu", color = "green", size = 3, vjust = -1.5) +
  annotate("text", x = 31.0, y = 29.9, label = "Khafre Quarry", color = "purple", size = 3, vjust = -1.5) +
  xlim(30.8, 31.4) +  # Set limits for x-axis
  ylim(29.7, 30.1) +  # Set limits for y-axis
  ggtitle("Zoomed-in Map of Khufu Pyramid, Nile River, Quarries, and Scatter Points") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))


zoom_rect <- data.frame(
  xmin = 30.8,
  xmax = 31.4,
  ymin = 29.7,
  ymax = 30.1
)


# Plot
ggplot() +
  geom_sf(data = egypt, fill = "lightblue", color = "black") +  # Draw the map of Egypt
  geom_sf(data = scatter_points_sf, color = "blue", size = 0.5, alpha = 0.6) +  # Scatter points with smaller size and transparency
  geom_sf(data = khufu_pyramid, color = "red", size = 3) +  # Khufu Pyramid location
  geom_sf(data = nile_river_sf, color = "blue", size = 4, alpha = 0.5) +  # Nile River as a line
  geom_sf(data = quarry_khufu_sf, color = "green", size = 3) +  # Quarry near Khufu Pyramid
  geom_sf(data = quarry_khafre_sf, color = "purple", size = 3) +  # Khafre Quarry
  annotate("text", x = 31.31, y = 29.85, label = "Nile River", color = "blue", size = 4, angle = 0, vjust = -1) +
  annotate("text", x = 31.1342, y = 29.9792, label = "Khufu Pyramid", color = "red", size = 3, vjust = -1.5) +
  annotate("text", x = 31.1342, y = 29.975, label = "Quarry near Khufu", color = "green", size = 3, vjust = -1.5) +
  annotate("text", x = 31.0, y = 29.9, label = "Khafre Quarry", color = "purple", size = 3, vjust = -1.5) +
  geom_rect(data = zoom_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "red", fill = NA, size = 1) +
  ggtitle("Map of Egypt with Zoomed-in Area, Khufu Pyramid, Nile River, and Scatter Points") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))
#######################################################################################################################################
#########################################################Model2a#####################################################################
#######################################################################################################################################
#######################################################################################################################################

# Custom function to calculate Euclidean distance
euclidean_distance <- function(lon1, lat1, lon2, lat2) {
  sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)
}

# Set seed and generate scatter points
num_points <- 500
set.seed(123)  # For reproducibility

# Define quarries
quarry_khufu <- data.frame(lon = 31.1342, lat = 29.975)
quarry_khafre <- data.frame(lon = 31.0, lat = 29.9)

# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = 31.1342, sd = 0.02), rnorm(num_points * 0.45, mean = 31.0, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = 29.975, sd = 0.02), rnorm(num_points * 0.45, mean = 29.9, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

scatter_points <- rbind(scatter_points_random, scatter_points_near_quarries)

# Define the Khufu Pyramid location
khufu_pyramid <- data.frame(lon = 31.1342, lat = 29.9792)

# Capacity of the vehicle
c_1 <- 4

# Initialize loading count, vehicle location, and total distance
load_count_closest <- 0
current_location <- c(khufu_pyramid$lon, khufu_pyramid$lat)
total_distance <- 0

while (nrow(scatter_points) > 0) {
  # Calculate distances from current location to all scatter points
  scatter_points$distance <- euclidean_distance(current_location[1], current_location[2], scatter_points$lon, scatter_points$lat)
  
  # Find the closest c_1 points
  closest_points <- scatter_points %>% arrange(distance) %>% head(c_1)
  
  # Update total distance (including the return to the depot)
  trip_distance <- euclidean_distance(current_location[1], current_location[2], closest_points$lon[1], closest_points$lat[1])
  for (i in 1:(nrow(closest_points) - 1)) {
    trip_distance <- trip_distance + euclidean_distance(closest_points$lon[i], closest_points$lat[i], closest_points$lon[i + 1], closest_points$lat[i + 1])
  }
  trip_distance <- trip_distance + euclidean_distance(closest_points$lon[nrow(closest_points)], closest_points$lat[nrow(closest_points)], khufu_pyramid$lon, khufu_pyramid$lat)
  
  total_distance <- total_distance + trip_distance
  
  # Update current location to the last loaded point
  current_location <- c(closest_points$lon[nrow(closest_points)], closest_points$lat[nrow(closest_points)])
  
  # Remove the closest points from scatter_points
  scatter_points <- scatter_points %>% anti_join(closest_points, by = c("lon", "lat"))
  
  # Increment load count
  load_count_closest <- load_count_closest + 1
}

print(total_distance)
print(paste("Total number of loads required for closest points:", load_count_closest))



#######################################################################################################################################
#########################################################Strategy2#####################################################################
#######################################################################################################################################
#######################################################################################################################################


# Custom function to calculate Euclidean distance
euclidean_distance <- function(lon1, lat1, lon2, lat2) {
  sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)
}

# Set seed and generate scatter points
num_points <- 500
set.seed(123)  # For reproducibility

# Define quarries
quarry_khufu <- data.frame(lon = 31.1342, lat = 29.975)
quarry_khafre <- data.frame(lon = 31.0, lat = 29.9)

# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = 31.1342, sd = 0.02), rnorm(num_points * 0.45, mean = 31.0, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = 29.975, sd = 0.02), rnorm(num_points * 0.45, mean = 29.9, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

scatter_points <- rbind(scatter_points_random, scatter_points_near_quarries)

# Define the Khufu Pyramid location
khufu_pyramid <- data.frame(lon = 31.1342, lat = 29.9792)

# Capacity of the vehicle
c_1 <- 4

# Initialize loading count, vehicle location, and total distance
load_count_furthest <- 0
total_distance_furthest <- 0

scatter_points_combined <- scatter_points  # Clone scatter points data frame for use
scatter_points_combined$distance <- NA  # Add a distance column for initial calculations

loaded_points_v1 <- data.frame(lon = numeric(0), lat = numeric(0), distance = numeric(0))

while (nrow(scatter_points_combined) > 0) {
  scatter_points_combined$distance <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, scatter_points_combined$lon, scatter_points_combined$lat)
  
  trip_distance <- 0  # Initialize trip distance for the current trip
  current_location <- c(khufu_pyramid$lon, khufu_pyramid$lat)
  
  for (i in 1:c_1) {
    if (nrow(scatter_points_combined) == 0) { break }
    
    furthest_point <- scatter_points_combined %>% 
      arrange(desc(distance)) %>% 
      slice_head(n = 1)
    
    # Calculate distance from current location to furthest point
    trip_distance <- trip_distance + euclidean_distance(current_location[1], current_location[2], furthest_point$lon, furthest_point$lat)
    
    # Update current location to the furthest point
    current_location <- c(furthest_point$lon, furthest_point$lat)
    
    # Add the furthest point to the load
    loaded_points_v1 <- bind_rows(loaded_points_v1, furthest_point)
    
    # Remove the furthest point from scatter_points_combined
    scatter_points_combined <- scatter_points_combined %>% 
      anti_join(furthest_point, by = c("lon", "lat"))
    
    if (nrow(scatter_points_combined) > 0) {
      scatter_points_combined$distance_to_current <- euclidean_distance(current_location[1], current_location[2], scatter_points_combined$lon, scatter_points_combined$lat)
      closest_to_current <- scatter_points_combined %>% 
        arrange(distance_to_current) %>% 
        slice_head(n = 1)
      
      if (closest_to_current$distance_to_current >= furthest_point$distance) {
        break
      }
    }
  }
  
  # Add the return trip to the depot
  trip_distance <- trip_distance + euclidean_distance(current_location[1], current_location[2], khufu_pyramid$lon, khufu_pyramid$lat)
  
  # Increment total distance
  total_distance_furthest <- total_distance_furthest + trip_distance
  
  load_count_furthest <- load_count_furthest + 1  # Increment load count
}

print(total_distance_furthest)

# Output the number of loads
print(paste("Total number of loads required for closest points:", load_count_closest))
print(paste("Total number of loads required for furthest points:", load_count_furthest))


##################################################################
######################STRATEGY3&4#################################
##################################################################

# Set seed and generate scatter points
num_points <- 500
set.seed(123)  # For reproducibility

# Define quarries
quarry_khufu <- data.frame(lon = 31.1342, lat = 29.975)
quarry_khafre <- data.frame(lon = 31.0, lat = 29.9)

# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = 31.1342, sd = 0.02), rnorm(num_points * 0.45, mean = 31.0, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = 29.975, sd = 0.02), rnorm(num_points * 0.45, mean = 29.9, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

scatter_points <- rbind(scatter_points_random, scatter_points_near_quarries)

# Define the Khufu Pyramid location
khufu_pyramid <- data.frame(lon = 31.1342, lat = 29.9792)

c_1 <- 2
c_2 <- 6

# Initialize loading counts and total distances
load_count_v1_closest <- 0
load_count_v2_furthest <- 0
total_distance_v1 <- 0
total_distance_v2 <- 0
total_collected_points <- 0
scatter_points_combined <- scatter_points
current_location_v1 <- c(khufu_pyramid$lon, khufu_pyramid$lat)
current_location_v2 <- c(khufu_pyramid$lon, khufu_pyramid$lat)

# Custom function to calculate Euclidean distance
euclidean_distance <- function(lon1, lat1, lon2, lat2) {
  sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)
}

# Collect scatter points until all points are gathered or reach the count of num_points
while (total_collected_points < num_points && nrow(scatter_points) > 0) {
  # Vehicle 1 collects the closest points
  if (nrow(scatter_points) > 0) {
    scatter_points$distance_v1 <- euclidean_distance(current_location_v1[1], current_location_v1[2], scatter_points$lon, scatter_points$lat)
    closest_points_v1 <- scatter_points %>% arrange(distance_v1) %>% head(c_1)
    if (nrow(closest_points_v1) > 0) {
      trip_distance_v1 <- 0
      for (i in 1:nrow(closest_points_v1)) {
        trip_distance_v1 <- trip_distance_v1 + euclidean_distance(current_location_v1[1], current_location_v1[2], closest_points_v1$lon[i], closest_points_v1$lat[i])
        current_location_v1 <- c(closest_points_v1$lon[i], closest_points_v1$lat[i])
      }
      trip_distance_v1 <- trip_distance_v1 + euclidean_distance(current_location_v1[1], current_location_v1[2], khufu_pyramid$lon, khufu_pyramid$lat)
      total_distance_v1 <- total_distance_v1 + trip_distance_v1
      
      current_location_v1 <- c(khufu_pyramid$lon, khufu_pyramid$lat)
      scatter_points <- scatter_points %>% anti_join(closest_points_v1, by = c("lon", "lat"))
      total_collected_points <- total_collected_points + nrow(closest_points_v1)
      load_count_v1_closest <- load_count_v1_closest + 1
    }
  }
  
  # Vehicle 2 collects the furthest points
  if (nrow(scatter_points) > 0) {
    scatter_points$distance_v2 <- euclidean_distance(current_location_v2[1], current_location_v2[2], scatter_points$lon, scatter_points$lat)
    furthest_points_v2 <- scatter_points %>% arrange(desc(distance_v2)) %>% head(c_2)
    if (nrow(furthest_points_v2) > 0) {
      trip_distance_v2 <- 0
      for (i in 1:nrow(furthest_points_v2)) {
        trip_distance_v2 <- trip_distance_v2 + euclidean_distance(current_location_v2[1], current_location_v2[2], furthest_points_v2$lon[i], furthest_points_v2$lat[i])
        current_location_v2 <- c(furthest_points_v2$lon[i], furthest_points_v2$lat[i])
      }
      trip_distance_v2 <- trip_distance_v2 + euclidean_distance(current_location_v2[1], current_location_v2[2], khufu_pyramid$lon, khufu_pyramid$lat)
      total_distance_v2 <- total_distance_v2 + trip_distance_v2
      
      current_location_v2 <- c(khufu_pyramid$lon, khufu_pyramid$lat)
      scatter_points <- scatter_points %>% anti_join(furthest_points_v2, by = c("lon", "lat"))
      total_collected_points <- total_collected_points + nrow(furthest_points_v2)
      load_count_v2_furthest <- load_count_v2_furthest + 1
    }
  }
}

# Output the results
print(paste("Total loads for vehicle 1 (closest):", load_count_v1_closest))
print(paste("Total loads for vehicle 2 (furthest):", load_count_v2_furthest))
total_loads <- load_count_v1_closest + load_count_v2_furthest
print(paste("Total combined loads:", total_loads))

# Output the total distances traveled
total_distance <- total_distance_v1 + total_distance_v2
print(paste("Total distance traveled by both vehicles:", total_distance))

##################################################################
######################STRATEGY5#################################
##################################################################



# Custom function to calculate Euclidean distance
euclidean_distance <- function(lon1, lat1, lon2, lat2) {
  sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)
}

# Set seed and generate scatter points
num_points <- 500
set.seed(123)  # For reproducibility

# Define quarries
quarry_khufu <- data.frame(lon = 31.1342, lat = 29.975)
quarry_khafre <- data.frame(lon = 31.0, lat = 29.9)

# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = 31.1342, sd = 0.02), rnorm(num_points * 0.45, mean = 31.0, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = 29.975, sd = 0.02), rnorm(num_points * 0.45, mean = 29.9, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

# Define the Khufu Pyramid location
khufu_pyramid <- data.frame(lon = 31.1342, lat = 29.9792)

# Vehicle capacities
c_1 <- 6  # Capacity for vehicle handling quarry scatter points
c_2 <- 2  # Capacity for vehicle handling random scatter points

# Initialize loading counts and total distances
load_count_v1_closest <- 0
load_count_v2_furthest <- 0
total_distance_v1 <- 0
total_distance_v2 <- 0

# Collect scatter points for vehicle 1 (near quarries)
while (nrow(scatter_points_near_quarries) > 0) {
  scatter_points_near_quarries$distance_v1 <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, scatter_points_near_quarries$lon, scatter_points_near_quarries$lat)
  closest_points_v1 <- scatter_points_near_quarries %>% arrange(distance_v1) %>% head(c_1)
  
  if (nrow(closest_points_v1) > 0) {
    # Calculate the trip distance for vehicle 1
    trip_distance_v1 <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, closest_points_v1$lon[1], closest_points_v1$lat[1])
    for (i in 1:(nrow(closest_points_v1) - 1)) {
      trip_distance_v1 <- trip_distance_v1 + euclidean_distance(closest_points_v1$lon[i], closest_points_v1$lat[i], closest_points_v1$lon[i + 1], closest_points_v1$lat[i + 1])
    }
    trip_distance_v1 <- trip_distance_v1 + euclidean_distance(closest_points_v1$lon[nrow(closest_points_v1)], closest_points_v1$lat[nrow(closest_points_v1)], khufu_pyramid$lon, khufu_pyramid$lat)
    total_distance_v1 <- total_distance_v1 + trip_distance_v1
    
    scatter_points_near_quarries <- scatter_points_near_quarries %>% anti_join(closest_points_v1, by = c("lon", "lat"))
    load_count_v1_closest <- load_count_v1_closest + 1
  }
}

# Collect scatter points for vehicle 2 (random scatter points)
while (nrow(scatter_points_random) > 0) {
  scatter_points_random$distance_v2 <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, scatter_points_random$lon, scatter_points_random$lat)
  furthest_points_v2 <- scatter_points_random %>% arrange(desc(distance_v2)) %>% head(c_2)
  
  if (nrow(furthest_points_v2) > 0) {
    # Calculate the trip distance for vehicle 2
    trip_distance_v2 <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, furthest_points_v2$lon[1], furthest_points_v2$lat[1])
    for (i in 1:(nrow(furthest_points_v2) - 1)) {
      trip_distance_v2 <- trip_distance_v2 + euclidean_distance(furthest_points_v2$lon[i], furthest_points_v2$lat[i], furthest_points_v2$lon[i + 1], furthest_points_v2$lat[i + 1])
    }
    trip_distance_v2 <- trip_distance_v2 + euclidean_distance(furthest_points_v2$lon[nrow(furthest_points_v2)], furthest_points_v2$lat[nrow(furthest_points_v2)], khufu_pyramid$lon, khufu_pyramid$lat)
    total_distance_v2 <- total_distance_v2 + trip_distance_v2
    
    scatter_points_random <- scatter_points_random %>% anti_join(furthest_points_v2, by = c("lon", "lat"))
    load_count_v2_furthest <- load_count_v2_furthest + 1
  }
}

# Output the results
print(paste("Total loads for vehicle 1 (closest):", load_count_v1_closest))
print(paste("Total loads for vehicle 2 (furthest):", load_count_v2_furthest))
total_loads <- load_count_v1_closest + load_count_v2_furthest
print(paste("Total combined loads:", total_loads))
print(paste("Total distance traveled by vehicle 1:", total_distance_v1))
print(paste("Total distance traveled by vehicle 2:", total_distance_v2))
print(paste("Total distance traveled by both vehicles:", total_distance_v1 + total_distance_v2))



##########################################testing##################################################################


# Custom function to calculate Euclidean distance
euclidean_distance <- function(lon1, lat1, lon2, lat2) {
  sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)
}

# Set seed and generate scatter points
num_points <- 500
set.seed(123)  # For reproducibility

# Define quarries
quarry_khufu <- data.frame(lon = 31.1342, lat = 29.975)
quarry_khafre <- data.frame(lon = 31.0, lat = 29.9)

# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = 31.1342, sd = 0.02), rnorm(num_points * 0.45, mean = 31.0, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = 29.975, sd = 0.02), rnorm(num_points * 0.45, mean = 29.9, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

scatter_points <- rbind(scatter_points_random, scatter_points_near_quarries)

# Define the Khufu Pyramid location
khufu_pyramid <- data.frame(lon = 31.1342, lat = 29.9792)

# Vehicle capacities
c_1 <- 6  # Capacity for vehicle handling quarry scatter points
c_2 <- 2  # Capacity for vehicle handling random scatter points

# Initialize loading counts and total distances
load_count_v1_closest <- 0
load_count_v2_furthest <- 0
total_distance_v1 <- 0
total_distance_v2 <- 0

# Collect scatter points for vehicle 1 (near quarries)
while (nrow(scatter_points_near_quarries) > 0) {
  loaded_points_v1 <- data.frame()
  
  while (nrow(loaded_points_v1) < c_1 && nrow(scatter_points_near_quarries) > 0) {
    scatter_points_near_quarries$distance_v1 <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, scatter_points_near_quarries$lon, scatter_points_near_quarries$lat)
    closest_point <- scatter_points_near_quarries %>% arrange(distance_v1) %>% head(1)
    
    loaded_points_v1 <- rbind(loaded_points_v1, closest_point)
    scatter_points_near_quarries <- scatter_points_near_quarries %>% anti_join(closest_point, by = c("lon", "lat"))
  }
  
  if (nrow(loaded_points_v1) > 0) {
    # Calculate the trip distance for vehicle 1
    trip_distance_v1 <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, loaded_points_v1$lon[1], loaded_points_v1$lat[1])
    for (i in 1:(nrow(loaded_points_v1) - 1)) {
      trip_distance_v1 <- trip_distance_v1 + euclidean_distance(loaded_points_v1$lon[i], loaded_points_v1$lat[i], loaded_points_v1$lon[i + 1], loaded_points_v1$lat[i + 1])
    }
    trip_distance_v1 <- trip_distance_v1 + euclidean_distance(loaded_points_v1$lon[nrow(loaded_points_v1)], loaded_points_v1$lat[nrow(loaded_points_v1)], khufu_pyramid$lon, khufu_pyramid$lat)
    total_distance_v1 <- total_distance_v1 + trip_distance_v1
    
    load_count_v1_closest <- load_count_v1_closest + 1
  }
}

# Collect scatter points for vehicle 2 (random scatter points)
while (nrow(scatter_points_random) > 0) {
  loaded_points_v2 <- data.frame()
  
  while (nrow(loaded_points_v2) < c_2 && nrow(scatter_points_random) > 0) {
    scatter_points_random$distance_v2 <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, scatter_points_random$lon, scatter_points_random$lat)
    furthest_point <- scatter_points_random %>% arrange(desc(distance_v2)) %>% head(1)
    
    loaded_points_v2 <- rbind(loaded_points_v2, furthest_point)
    scatter_points_random <- scatter_points_random %>% anti_join(furthest_point, by = c("lon", "lat"))
  }
  
  if (nrow(loaded_points_v2) > 0) {
    # Calculate the trip distance for vehicle 2
    trip_distance_v2 <- euclidean_distance(khufu_pyramid$lon, khufu_pyramid$lat, loaded_points_v2$lon[1], loaded_points_v2$lat[1])
    for (i in 1:(nrow(loaded_points_v2) - 1)) {
      trip_distance_v2 <- trip_distance_v2 + euclidean_distance(loaded_points_v2$lon[i], loaded_points_v2$lat[i], loaded_points_v2$lon[i + 1], loaded_points_v2$lat[i + 1])
    }
    trip_distance_v2 <- trip_distance_v2 + euclidean_distance(loaded_points_v2$lon[nrow(loaded_points_v2)], loaded_points_v2$lat[nrow(loaded_points_v2)], khufu_pyramid$lon, khufu_pyramid$lat)
    total_distance_v2 <- total_distance_v2 + trip_distance_v2
    
    load_count_v2_furthest <- load_count_v2_furthest + 1
  }
}

# Output the results
print(paste("Total loads for vehicle 1 (closest):", load_count_v1_closest))
print(paste("Total loads for vehicle 2 (furthest):", load_count_v2_furthest))
total_loads <- load_count_v1_closest + load_count_v2_furthest
print(paste("Total combined loads:", total_loads))
print(paste("Total distance traveled by vehicle 1:", total_distance_v1))
print(paste("Total distance traveled by vehicle 2:", total_distance_v2))
print(paste("Total distance traveled by both vehicles:", total_distance_v1 + total_distance_v2))

#######################################Sensitivity#######################################
############################capacity

# Assuming euclidean_distance is defined and scatter_points, and current_location are initialized
set.seed(123)  # For reproducibility
num_points <- 500

# Define quarries
quarry_khufu <- list(lon = 31.1342, lat = 29.975)
quarry_khafre <- list(lon = 31.0, lat = 29.9)

# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = quarry_khufu$lon, sd = 0.02), 
          rnorm(num_points * 0.45, mean = quarry_khafre$lon, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = quarry_khufu$lat, sd = 0.02), 
          rnorm(num_points * 0.45, mean = quarry_khafre$lat, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

scatter_points <- rbind(scatter_points_random, scatter_points_near_quarries)

# Define the location of the Khufu Pyramid
khufu_pyramid <- data.frame(name = "Khufu Pyramid", lon = 31.1342, lat = 29.9792)

# Convert to sf object
khufu_pyramid_sf <- st_as_sf(khufu_pyramid, coords = c("lon", "lat"), crs = 4326)
# Function to calculate Euclidean distance
euclidean_distance <- function(lon1, lat1, lon2, lat2) {
  sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)
}

# Run model function for various capacities
results <- data.frame(c_1 = integer(), total_loads = integer())
for (capacity in 1:10) {
  total_loads <- model_function1(c_1 = capacity, 
                                 initial_lon = khufu_pyramid$lon, 
                                 initial_lat = khufu_pyramid$lat, 
                                 scatter_points = scatter_points)
  results <- rbind(results, data.frame(c_1 = capacity, total_loads = total_loads))
}

# Plotting the results
ggplot(results, aes(x = c_1, y = total_loads)) +
  geom_line(size = 1.0) + 
  geom_point(size = 1.5) +
  labs(title = "Sensitivity Analysis of Vehicle Capacity",
       x = "Vehicle Capacity (c_1)",
       y = "Total Loads Required")
#######Derivative
results$delta_loads <- c(NA, diff(results$total_loads))
results$delta_c1 <- c(NA, diff(results$c_1))

# Calculate derivative as delta_y / delta_x
results$derivative <- results$delta_loads / results$delta_c1
ggplot(results, aes(x = c_1)) +
  geom_line(aes(y = derivative), color = "#a3c5ea",size = 1.0) +
  geom_point(aes(y = derivative), color = "#a3c5ea",size = 1.5) +
  labs(title = "Derivative of Total Loads with respect to Vehicle Capacity",
       x = "Vehicle Capacity (c_1)",
       y = "Derivative (Total Loads Change Rate)") +
  theme_minimal()
############################Number of vehicles##############
# Define quarries
quarry_khufu <- data.frame(lon = 31.1342, lat = 29.975)
quarry_khafre <- data.frame(lon = 31.0, lat = 29.9)
# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = 31.1342, sd = 0.02), rnorm(num_points * 0.45, mean = 31.0, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = 29.975, sd = 0.02), rnorm(num_points * 0.45, mean = 29.9, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

scatter_points<-rbind(scatter_points_random,scatter_points_near_quarries)
# Define the location of the Khufu Pyramid
khufu_pyramid <- data.frame(name = "Khufu Pyramid", lon = 31.1342, lat = 29.9792)
# Convert to sf object
khufu_pyramid_sf <- st_as_sf(khufu_pyramid, coords = c("lon", "lat"), crs = 4326)
# Define the Euclidean distance function
euclidean_distance <- function(lon1, lat1, lon2, lat2) {
  sqrt((lon1 - lon2)^2 + (lat1 - lat2)^2)
}

set.seed(123)  # For reproducibility
num_points <- 500
# Function to simulate multiple vehicles collecting stones
model_function_multiple_vehicles <- function(num_vehicles, c_1, initial_locations, scatter_points) {
  load_counts <- rep(0, num_vehicles)
  current_locations <- initial_locations
  
  while (nrow(scatter_points) > 0) {
    for (i in seq_len(num_vehicles)) {
      if (nrow(scatter_points) == 0) break
      
      scatter_points$distance <- mapply(euclidean_distance,
                                        lon1 = current_locations[i, 1], 
                                        lat1 = current_locations[i, 2], 
                                        lon2 = scatter_points$lon, 
                                        lat2 = scatter_points$lat)
      
      closest_points <- scatter_points %>% arrange(distance) %>% head(c_1)
      if (nrow(closest_points) > 0) {
        current_locations[i, ] <- c(tail(closest_points$lon, 1), tail(closest_points$lat, 1))
        scatter_points <- scatter_points %>% anti_join(closest_points, by = c("lon", "lat"))
        load_counts[i] <- load_counts[i] + 1
      }
    }
  }
  
  return(sum(load_counts))
}

# Generate initial locations for each vehicle
initial_locations <- matrix(c(rep(quarry_khufu$lon, 10), rep(quarry_khufu$lat, 10)), ncol = 2, byrow = TRUE)
vehicle_counts <- 1:10  # Testing from 1 to 10 vehicles
results_multiple <- data.frame(num_vehicles = integer(), total_loads = integer())

for (num_vehicles in vehicle_counts) {
  total_loads <- model_function_multiple_vehicles(num_vehicles, c_1 = 4, 
                                                  initial_locations = initial_locations[1:num_vehicles, , drop = FALSE],
                                                  scatter_points = scatter_points)
  results_multiple <- rbind(results_multiple, data.frame(num_vehicles = num_vehicles, total_loads = total_loads))
}

# Print the results
print(results_multiple)

# Plot the results
ggplot(results_multiple, aes(x = num_vehicles, y = total_loads)) +
  geom_line(size = 1.0) +
  geom_point(size = 1.5) +
  labs(title = "Impact of Number of Vehicles on Total Loads",
       x = "Number of Vehicles",
       y = "Total Loads Required") +
  theme_minimal()
#########################value of n###################
num_points <- 500
set.seed(123)  # For reproducibility

# Define quarries
quarry_khufu <- data.frame(lon = 31.1342, lat = 29.975)
quarry_khafre <- data.frame(lon = 31.0, lat = 29.9)

# Generate scatter points near quarries with clustering
scatter_points_near_quarries <- data.frame(
  lon = c(rnorm(num_points * 0.45, mean = 31.1342, sd = 0.02), rnorm(num_points * 0.45, mean = 31.0, sd = 0.02)),
  lat = c(rnorm(num_points * 0.45, mean = 29.975, sd = 0.02), rnorm(num_points * 0.45, mean = 29.9, sd = 0.02))
)

# Generate some scatter points randomly across the map
scatter_points_random <- data.frame(
  lon = runif(num_points * 0.1, min = 30.8, max = 31.4),
  lat = runif(num_points * 0.1, min = 29.7, max = 30.1)
)

scatter_points<-rbind(scatter_points_random,scatter_points_near_quarries)

perform_simulation <- function(x, scatter_points, c_1, c_2, initial_location) {
  # Sorting scatter points by distance to the pyramid
  scatter_points$distance_to_pyramid <- mapply(euclidean_distance, initial_location[1], initial_location[2],
                                               scatter_points$lon, scatter_points$lat)
  sorted_scatter_points <- scatter_points[order(scatter_points$distance_to_pyramid), ]
  
  # Split points based on x
  points_within_mean <- sorted_scatter_points[1:x, ]
  points_beyond_mean <- sorted_scatter_points[(x + 1):nrow(sorted_scatter_points), ]
  
  # Initialize vehicle states
  load_count_closest <- 0
  load_count_furthest <- 0
  current_location_v1 <- initial_location
  current_location_v2 <- initial_location
  
  # Simulation loop for both vehicles
  while (nrow(points_within_mean) > 0 || nrow(points_beyond_mean) > 0) {
    if (nrow(points_within_mean) > 0) {
      points_within_mean$distance <- mapply(euclidean_distance, current_location_v1[1], current_location_v1[2],
                                            points_within_mean$lon, points_within_mean$lat)
      closest_points <- points_within_mean %>% dplyr::arrange(distance) %>% head(c_1)
      if (nrow(closest_points) > 0) {
        current_location_v1 <- c(tail(closest_points$lon, 1), tail(closest_points$lat, 1))
        points_within_mean <- points_within_mean %>% dplyr::anti_join(closest_points, by = c("lon", "lat"))
        load_count_closest <- load_count_closest + 1
      }
    }
    
    if (nrow(points_beyond_mean) > 0) {
      points_beyond_mean$distance <- mapply(euclidean_distance, current_location_v2[1], current_location_v2[2],
                                            points_beyond_mean$lon, points_beyond_mean$lat)
      furthest_points <- points_beyond_mean %>% dplyr::arrange(desc(distance)) %>% head(c_2)
      if (nrow(furthest_points) > 0) {
        current_location_v2 <- c(tail(furthest_points$lon, 1), tail(furthest_points$lat, 1))
        points_beyond_mean <- points_beyond_mean %>% dplyr::anti_join(furthest_points, by = c("lon", "lat"))
        load_count_furthest <- load_count_furthest + 1
      }
    }
  }
  
  return(load_count_closest + load_count_furthest)
}
results <- data.frame(x = integer(), total_loads = integer())
for (x in 1:250) {
  total_loads <- perform_simulation(x, scatter_points, 4, 9, c(khufu_pyramid$lon, khufu_pyramid$lat))
  results <- rbind(results, data.frame(x = x, total_loads = total_loads))
}


ggplot(results, aes(x = x, y = total_loads)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Impact of 'x' on Total Loads Required",
       x = "Value of x (split point)",
       y = "Total Loads Required") +
  theme_minimal()

# Add NAs to handle the boundaries properly
results$derivative <- c(NA, diff(results$total_loads), NA) / 2  # Using diff() for finite difference

# Clean up NAs for plotting (if necessary, depending on how you want to handle edges)
results <- na.omit(results)  # Optionally, omit NA if plotting methods require clean data


