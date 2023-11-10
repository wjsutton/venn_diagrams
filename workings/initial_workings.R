library(eulerr)

# Create a named numeric vector to represent the sets and their intersections
#sets <- c(A = 25, B = 5, C = 5, "A&B" = 5, "A&C" = 5, "B&C" = 3, "A&B&C" = 3)
sets <- c(A = 707, B = 788, C = 687, "A&B" = 86, "A&C" = 1, "B&C" = 68, "A&B&C" = 617)

# Generate an eulerr object that represents the diagram
fit <- euler(sets)

# Plot the diagram
plot(fit, fills = c("skyblue", "plum", "sandybrown"), labels = list(font = 4), quantities = TRUE)


fit

coords <- fit$ellipses


# Extract the coordinate data
# Extract the coordinates of the centers
centers <- fit$ellipses[, c("h", "k")]

# Extract the dimensions of the circles (which are the radii in this case)
radii <- fit$ellipses[, "a"]  # since for circles, a and b are equal

# If needed, you can also extract the 'phi' column, but it should be 0 or irrelevant for circles
rotations <- fit$ellipses[, "phi"]
