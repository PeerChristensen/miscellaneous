library(ggplot2)

lu_colors <- c(
  `dark blue`   = "#000080",
  `bronze`      = "#9C6114",
  `light green` = "#DFEFE8",
  `light blue`  = "#DBEFF8",
  `light pink`  = "#FBE5F0")

lu_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (lu_colors)
  
  lu_colors[cols]
}

lu_palettes <- list(
  `main`  = lu_cols("bronze","dark blue"),
  `pastels` = lu_cols("light green","light blue", "light pink"))

lu_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- lu_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


#colour
scale_colour_lu <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lu_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("lu_", palette), palette = pal, ...)
  } else {
    scale_colour_gradientn(colours = pal(256), ...)
  }
}

#fill
scale_fill_lu <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lu_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("lu_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#examples "main palette"
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_lu()

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 6, alpha = 0.8) +
  scale_colour_lu(discrete = FALSE)

ggplot(iris, aes(Species,Sepal.Length,fill=Species)) +
  geom_bar(stat="identity") +
  scale_fill_lu()

#examples
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_lu("pastels") +
  theme_dark()

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 6, alpha = 0.8) +
  scale_colour_lu(discrete = FALSE, palette = "pastels")

ggplot(iris, aes(Species,Sepal.Length,fill=Species)) +
  geom_bar(stat="identity") +
  scale_fill_lu("pastels")

ggplot(diamonds,aes(x=carat,y=price, colour= price)) + geom_point(alpha=.3) + scale_colour_lu(discrete=F,"main")
ggplot(diamonds,aes(x=carat,y=price, colour= price)) + geom_point(alpha=.3) + scale_colour_lu(discrete=F,"pastels")
