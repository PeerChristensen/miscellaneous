# check plot colour palette: desaturation and colourblindness

library(ggplot2)
library(colorblindr)

#basic ggplot colours
fig <- ggplot(iris, aes(Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.7)
fig

# check
cvd_grid(fig)

#better with viridis palettes#

fig2 <- ggplot(iris, aes(Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_viridis(option="B",discrete=T)
fig2

cvd_grid(fig2)

fig3 <- ggplot(iris, aes(Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_viridis(option="D",discrete=T)
fig3

cvd_grid(fig3)

#stellenbosch?
fig4 <- ggplot(iris, aes(Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_stellenbosch("main",discrete=T)
fig4

cvd_grid(fig4)
