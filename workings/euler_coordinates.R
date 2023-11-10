library(tidyverse)
library(eulerr)

# Create a named numeric vector to represent the sets and their intersections
sets <- c(A = 25, B = 5, C = 5, "A&B" = 5, "A&C" = 5, "B&C" = 3, "A&B&C" = 3)

# Generate an eulerr object that represents the diagram
fit <- euler(sets)

# Assuming 'fit' is your euler object from the 'eulerr' package
coords <- fit$ellipses
set_names <- names(fit$original.values)[1:3]  # Assuming the first three values correspond to individual sets

# Define the number of points per circle
resolution <- 1000

# Function to generate points for a circle with set name and path
generate_points <- function(h, k, r, set_name, resolution) {
  angles <- seq(0, 2 * pi, length.out = resolution)
  tibble(
    x = h + r * cos(angles),
    y = k + r * sin(angles),
    set = set_name,
    path = 1:resolution
  )
}

point_in_ellipse <- function(point, ellipse) {
  # Shift the points to the origin based on the ellipse center
  x0 <- point$x - ellipse$h
  y0 <- point$y - ellipse$k
  
  # Calculate the angle of rotation and rotate the points back
  phi <- ellipse$phi
  x_rot <- x0 * cos(-phi) - y0 * sin(-phi)
  y_rot <- x0 * sin(-phi) + y0 * cos(-phi)
  
  # Check if the point lies within the ellipse
  inside <- (x_rot / ellipse$a)^2 + (y_rot / ellipse$b)^2 <= 1
  return(inside)
}

# Generate points for all circles with set names
points_list <- map2(1:nrow(coords), set_names, ~generate_points(coords$h[.x], coords$k[.x], coords$a[.x], .y, resolution))

# Combine all points into one data frame
all_points <- bind_rows(points_list)

point_in_circle <- function(points, center, radius) {
  distances <- sqrt((points$x - center$h)^2 + (points$y - center$k)^2)
  distances <= radius
}

# Define a function to check if points are within all given circles
points_in_all_circles <- function(points, centers, radii) {
  distances <- purrr::map2_dbl(centers$h, centers$k, ~sqrt((points$x - .x)^2 + (points$y - .y)^2))
  all(distances <= radii)
}

### Overlap ABC

all_points$overlap_ABC <- FALSE

# Iterate over each point and check for overlaps
for(i in 1:nrow(all_points)) {
  point <- all_points[i,]
  in_A <- point_in_ellipse(point, coords[1,])
  in_B <- point_in_ellipse(point, coords[2,])
  in_C <- point_in_ellipse(point, coords[3,])
  
  # Check for triple overlap first
  all_points$overlap_ABC[i] <- in_A && in_B && in_C
  
}

overlap_ABC_points <- all_points %>% filter(overlap_ABC)
overlap_ABC_points$set <- 'ABC'

# Assuming 'h' and 'k' are the coordinates of the ellipse's center
center_h <- mean(overlap_ABC_points$x)
center_k <- mean(overlap_ABC_points$y)

# Calculate angles for each point relative to the center
overlap_ABC_points <- overlap_ABC_points %>%
  mutate(angle = atan2(y - center_k, x - center_h)) %>%
  arrange(angle) %>%
  mutate(path = row_number())

overlap_ABC_points <- overlap_ABC_points[,c('x','y','set','path')]

# Initialize columns to identify overlaps
#all_points$overlap_AB <- FALSE
#all_points$overlap_AC <- FALSE
#all_points$overlap_BC <- FALSE

### Overlap AB
all_points_AB <- filter(all_points,set %in% c('A','B'))
all_points_AB$overlap_AB <- FALSE

# Iterate over each point and check for overlaps
for(i in 1:nrow(all_points_AB)) {
  point <- all_points_AB[i,]
  in_A <- point_in_ellipse(point, coords[1,])
  in_B <- point_in_ellipse(point, coords[2,])
  in_C <- point_in_ellipse(point, coords[3,])
  
  # Check for overlap 
  all_points_AB$overlap_AB[i] <- in_A && in_B
}

overlap_AB_points <- all_points_AB %>% filter(overlap_AB)
overlap_AB_points$set <- 'AB'

# Assuming 'h' and 'k' are the coordinates of the ellipse's center
center_h <- mean(overlap_AB_points$x)
center_k <- mean(overlap_AB_points$y)

# Calculate angles for each point relative to the center
overlap_AB_points <- overlap_AB_points %>%
  mutate(angle = atan2(y - center_k, x - center_h)) %>%
  arrange(angle) %>%
  mutate(path = row_number())

overlap_AB_points <- overlap_AB_points[,c('x','y','set','path')]



### Overlap AC
all_points_AC <- filter(all_points,set %in% c('A','C'))
all_points_AC$overlap_AC <- FALSE

# Iterate over each point and check for overlaps
for(i in 1:nrow(all_points_AC)) {
  point <- all_points_AC[i,]
  in_A <- point_in_ellipse(point, coords[1,])
  in_B <- point_in_ellipse(point, coords[2,])
  in_C <- point_in_ellipse(point, coords[3,])
  
  # Check for overlap 
  all_points_AC$overlap_AC[i] <- in_A && in_C
}

overlap_AC_points <- all_points_AC %>% filter(overlap_AC)
overlap_AC_points$set <- 'AC'

# Assuming 'h' and 'k' are the coordinates of the ellipse's center
center_h <- mean(overlap_AC_points$x)
center_k <- mean(overlap_AC_points$y)

# Calculate angles for each point relative to the center
overlap_AC_points <- overlap_AC_points %>%
  mutate(angle = atan2(y - center_k, x - center_h)) %>%
  arrange(angle) %>%
  mutate(path = row_number())

overlap_AC_points <- overlap_AC_points[,c('x','y','set','path')]

### Overlap BC
all_points_BC <- filter(all_points,set %in% c('B','C'))
all_points_BC$overlap_BC <- FALSE

# Iterate over each point and check for overlaps
for(i in 1:nrow(all_points_BC)) {
  point <- all_points_BC[i,]
  in_A <- point_in_ellipse(point, coords[1,])
  in_B <- point_in_ellipse(point, coords[2,])
  in_C <- point_in_ellipse(point, coords[3,])
  
  # Check for overlap 
  all_points_BC$overlap_BC[i] <- in_B && in_C
}

overlap_BC_points <- all_points_BC %>% filter(overlap_BC)
overlap_BC_points$set <- 'BC'

# Assuming 'h' and 'k' are the coordinates of the ellipse's center
center_h <- mean(overlap_BC_points$x)
center_k <- mean(overlap_BC_points$y)

# Calculate angles for eBCh point relative to the center
overlap_BC_points <- overlap_BC_points %>%
  mutate(angle = atan2(y - center_k, x - center_h)) %>%
  arrange(angle) %>%
  mutate(path = row_number())

overlap_BC_points <- overlap_BC_points[,c('x','y','set','path')]

circle_a <- filter(all_points,set == 'A')[,c('x','y')]
circle_a_not_bc <- rbind(circle_a,overlap_AB_points[,c('x','y')],overlap_AC_points[,c('x','y')])

# Define the center and radius of the circle
center_h <- -1.1652795
center_k <- -0.5599959
radius <- 3.456910

# Remove points outside the circle's radius
inside_circle_points <- circle_a_not_bc %>%
  filter((x - center_h)^2 + (y - center_k)^2 <= radius^2)


inside_circle_points <- inside_circle_points %>%
  mutate(
    radius = sqrt((x - center_h)^2 + (y - center_k)^2),
    angle = atan2(y - center_k, x - center_h)
  )

# Normalize angles to a 0 to 2*pi range
inside_circle_points$angle <- (inside_circle_points$angle + 2*pi) %% (2*pi)

# Find the closest point to the center for each angle
closest_points <- inside_circle_points %>%
  group_by(x,y) %>%
  slice_min(order_by = radius, n = 1) %>%
  ungroup()

circle_a_not_bc <- closest_points
circle_a_not_bc$set <- 'A_NOT_BC'



circle_a_not_bc <- circle_a_not_bc %>%
  mutate(angle = atan2(y - center_k, x - center_h)) %>%
  arrange(angle) %>%
  mutate(path = row_number())

circle_a_not_bc <- circle_a_not_bc[,c("x","y","set","path")]






# Assuming you have your 'all_points' dataframe with all the circle points and 'coords' with ellipse parameters

# Now all_points should contain only the points that are not in any intersection for each circle


# Iterate over each point and check for overlaps
#for(i in 1:nrow(all_points)) {
#  point <- all_points[i,]
#  in_A <- point_in_ellipse(point, coords[1,])
#  in_B <- point_in_ellipse(point, coords[2,])
#  in_C <- point_in_ellipse(point, coords[3,])
#  
#  # Check for triple overlap first
#  all_points$overlap_ABC[i] <- in_A && in_B && in_C
#  
#  # Check for double overlaps, excluding points in the third circle and the triple overlap
 # #all_points$overlap_AB[i] <- in_A && in_B && !in_C && !all_points$overlap_ABC[i]
#  #all_points$overlap_AC[i] <- in_A && in_C && !in_B && !all_points$overlap_ABC[i]
#  #all_points$overlap_BC[i] <- in_B && in_C && !in_A && !all_points$overlap_ABC[i]
#}

# Filter out the points based on overlap
#overlap_AB_points <- all_points %>% filter(overlap_AB)
#overlap_AC_points <- all_points %>% filter(overlap_AC)
#overlap_BC_points <- all_points %>% filter(overlap_BC)
#overlap_ABC_points <- all_points %>% filter(overlap_ABC)

#overlap_AB_points <- rbind(overlap_AB_points,overlap_ABC_points)
#overlap_AC_points <- rbind(overlap_AC_points,overlap_ABC_points)
#overlap_BC_points <- rbind(overlap_BC_points,overlap_ABC_points)

#overlap_ABC_points$set <- 'ABC'
#overlap_AB_points$set <- 'AB'
#overlap_AC_points$set <- 'AC'
#overlap_BC_points$set <- 'BC'


# Assuming 'h' and 'k' are the coordinates of the ellipse's center
#center_h <- mean(overlap_AB_points$x)
#center_k <- mean(overlap_AB_points$y)

# Calculate angles for each point relative to the center
#overlap_AB_points <- overlap_AB_points %>%
#  mutate(angle = atan2(y - center_k, x - center_h)) %>%
#  arrange(angle) %>%
#  mutate(path = row_number())

#overlap_AB_points$path <- 1:nrow(overlap_AB_points)
#overlap_AC_points$path <- 1:nrow(overlap_AC_points)
#overlap_BC_points$path <- 1:nrow(overlap_BC_points)

#overlap_ABC_points <- overlap_ABC_points[,c('x','y','set','path')]
#overlap_AB_points <- overlap_AB_points[,c('x','y','set','path')]
#overlap_AC_points <- overlap_AC_points[,c('x','y','set','path')]
#overlap_BC_points <- overlap_BC_points[,c('x','y','set','path')]

# Since we want to keep all unique rows from both dataframes, we can use concat and drop_duplicates
union_df <- rbind(all_points[,c('x','y','set','path')], overlap_ABC_points,overlap_AB_points,overlap_AC_points,overlap_BC_points,circle_a_not_bc)#,overlap_AB_points,overlap_AC_points,overlap_BC_points)

# Write the overlap points to a CSV file
#write_csv(overlap_points, "overlap_points.csv")
write_csv(union_df, "euler_points.csv")



