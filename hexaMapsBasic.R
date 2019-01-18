

devtools::install_github(“56north/hexamapmaker”)
library(hexamapmaker)

# Create data frame
# Notice the spacing of the points

x <- c(1,3,2,4,1,3,7,8)
y <- c(1,1,3,3,5,5,1,3)
id <- c("test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8")
z <- data.frame(id,x,y)

# Plot points

library(ggplot2)
ggplot(z, aes(x, y, group = id)) +
  geom_point() +
  coord_fixed(ratio = 1) +
  ylim(0,max(y)) + xlim(0,max(x))

# Turn points into hexagons

library(hexamapmaker)

zz <- make_polygons(z)

ggplot(zz, aes(x, y, group = id)) +
  geom_polygon(colour="black", fill = NA) +
  coord_fixed(ratio = 1)