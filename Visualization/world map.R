library(ggplot2)
library(maps)
library(mapdata)

# Create a data frame with countries
countries_colors <- data.frame(
  region = c("United States", "China", "Russia"),
  color_value = c(1, 2, 3)
)

# Create a map object of the world
world_map_object <- borders("world", fill = "white")

# Extract the data from the map object
world_map_data <- world_map_object$data

# Create a world map with ggplot2
world_map <- ggplot(world_map_data) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  coord_fixed(1.3)

# Color specific countries in the map
world_map + 
  geom_map(data = countries_colors,
           map = world_map_data,
           aes(map_id = region, fill = color_value),
           color = "black",
           linewidth = 0.05) + 
  scale_fill_gradient(low = "blue", high = "red") + 
  theme_void()

library(ggplot2)
library(maps)
library(mapdata)

# Create a data frame with countries and their fill colors
countries <- map_data("world") %>% 
  filter(region != "Antarctica")
countries$random_value <- runif(nrow(countries))

# Create a map object of the world
world_map_object <- borders("world", fill = "white")

# Extract the data from the map object
world_map_data <- world_map_object$data

# Create a world map with ggplot2
world_map <- ggplot(world_map_data) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  coord_fixed(1.3)

# Color specific countries in the map
world_map + 
  geom_map(data = countries,
           map = world_map_data,
           aes(map_id = region, fill = random_value),
           color = "black",
           linewidth = 0.05) + 
  scale_fill_gradient(low = "blue", high = "red") + 
  theme_void()