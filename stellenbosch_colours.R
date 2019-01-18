library(ggplot2)
library(pals)

stellenbosch_colors <- c(
  `brown`      = "#8B7355",
  `khaki`      = "#F0E68C",
  `green`      = "#008B45",
  `chardonnay` = "#ffffd4",
  `shiraz`     = "#660027")

stellenbosch_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (stellenbosch_colors)
  
  stellenbosch_colors[cols]
}

stellenbosch_palettes <- list(
  `main`  = stellenbosch_cols("brown","khaki", "green"),
  `wine` = stellenbosch_cols("shiraz","chardonnay"))

stellenbosch_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- stellenbosch_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


#colour
scale_colour_stellenbosch <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- stellenbosch_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("stellenbosch_", palette), palette = pal, ...)
  } else {
    scale_colour_gradientn(colours = pal(256), ...)
  }
}

#fill
scale_fill_stellenbosch <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- stellenbosch_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("stellenbosch_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#examples "main palette"
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_stellenbosch()

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 6, alpha = 0.8) +
  scale_colour_stellenbosch(discrete = FALSE)

ggplot(iris, aes(Species,Sepal.Length,fill=Species)) +
  geom_bar(stat="identity",alpha=.8) +
  scale_fill_stellenbosch()

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_stellenbosch("wine") +
  theme_dark()

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 6, alpha = 0.8) +
  scale_colour_stellenbosch(discrete = FALSE, palette = "wine")

ggplot(iris, aes(Species,Sepal.Length,fill=Species)) +
  geom_bar(stat="identity",alpha=.8) +
  scale_fill_stellenbosch("wine")

ggplot(diamonds,aes(x=carat,y=price, colour= price)) + geom_point(alpha=.3) + scale_colour_stellenbosch(discrete=F,"main")
ggplot(diamonds,aes(x=carat,y=price, colour= price)) + geom_point(alpha=.3) + scale_colour_stellenbosch(discrete=F,"wine")

#colorblindness test
pal.safe(stellenbosch_pal())
pal.safe(stellenbosch_pal("wine"))

#quality test
pal.test(stellenbosch_pal())
pal.test(stellenbosch_pal("wine"))

#bands
pal.bands(stellenbosch_pal(),stellenbosch_pal("wine"))
pal.bands(stellenbosch_pal(),stellenbosch_pal("wine"),main = "Discrete",show.names=T)



