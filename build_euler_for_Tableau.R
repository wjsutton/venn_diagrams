
library(dplyr)
library(sf)
library(tibble)
library(eulerr)

# Function to create a circle polygon from a center and radius
create_circle <- function(center, radius, n_points = 100) {
  tibble(
    angle = seq(0, 2 * pi, length.out = n_points),
    x = center[1] + radius * cos(angle),
    y = center[2] + radius * sin(angle),
    path = rep(1, n_points)
  )
}

# Function to calculate the polygon coordinates for each unique and intersecting section
calculate_polygons <- function(circles) {
  # Convert the circles to sf objects
  circles_sf <- circles %>% 
    rowwise() %>% 
    mutate(geometry = st_sfc(st_buffer(st_point(c(x, y)), radius), crs = 4326)) %>% 
    st_as_sf() %>% 
    ungroup()
  
  # Calculate the unique and intersecting sections
  intersections <- list(
    A_only = st_difference(circles_sf[1,], st_union(circles_sf[2,], circles_sf[3,])),
    B_only = st_difference(circles_sf[2,], st_union(circles_sf[1,], circles_sf[3,])),
    C_only = st_difference(circles_sf[3,], st_union(circles_sf[1,], circles_sf[2,])),
    AB_not_C = st_difference(st_intersection(circles_sf[1,], circles_sf[2,]), circles_sf[3,]),
    AC_not_B = st_difference(st_intersection(circles_sf[1,], circles_sf[3,]), circles_sf[2,]),
    BC_not_A = st_difference(st_intersection(circles_sf[2,], circles_sf[3,]), circles_sf[1,]),
    ABC = st_intersection(st_intersection(circles_sf[1,], circles_sf[2,]), circles_sf[3,])
  )
  
  # Extract the coordinates from the sf objects
  coords_list <- lapply(intersections, function(poly) {
    if (st_is_empty(poly)) return(tibble(x = numeric(), y = numeric(), path = integer()))
    coords <- st_coordinates(st_geometry(poly))
    tibble(x = coords[,1], y = coords[,2], path = rep(1, nrow(coords)))
  })
  
  # Combine all coordinates into one dataframe
  coords_df <- bind_rows(coords_list, .id = "section") %>% 
    group_by(section) %>% 
    mutate(path = row_number()) %>% 
    ungroup()
  
  return(coords_df)
}

# Example usage:
# Define the centers and radii of the circles
# Create a named numeric vector to represent the sets and their intersections
sets <- c(A = 707, B = 788, C = 687, "A&B" = 86, "A&C" = 1, "B&C" = 68, "A&B&C" = 617)

# Generate an eulerr object that represents the diagram
fit <- euler(sets)

# Assuming 'fit' is your euler object from the 'eulerr' package
coords <- fit$ellipses

circles_df <- tibble(
  circle = row.names(coords),
  x = coords$h,
  y = coords$k,
  radius = coords$a
)

# Calculate the polygons
polygons_df <- calculate_polygons(circles_df)

# Output the dataframe to a CSV file
write.csv(polygons_df, "polygons_tableau.csv", row.names = FALSE)
