
blueprint_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "navy")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(text = element_text(colour = "white", size = 20)) +
    theme(axis.text = element_text(colour = "white", size = 15)) +
    theme(plot.title = element_text(colour = "white")) +
    theme(strip.text.x = element_text(colour = "white", face = "bold")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_line(colour = "white")) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank()) +
    theme(legend.position = c(0.9,0.08))
}

mtcars %>% 
  group_by(cyl) %>%
  summarise(m= mean(mpg)) %>%
  ggplot(aes(x=cyl,y=m)) +
  geom_bar(stat = "identity", fill="white") +
  blueprint_theme()

####################################

blueprint2_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "steelblue4")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(text = element_text(colour = "gray79", size = 20)) +
    theme(axis.text = element_text(colour = "gray79", size = 15)) +
    theme(plot.title = element_text(colour = "gray79")) +
    theme(strip.text.x = element_text(colour = "gray79", face = "bold")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_line(colour = "gray79")) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank()) +
    theme(legend.position = c(0.9,0.08))
}

mtcars %>% 
  group_by(cyl) %>%
  summarise(m= mean(mpg)) %>%
  ggplot(aes(x=cyl,y=m)) +
  geom_bar(stat = "identity", fill="gray79") +
  blueprint2_theme()


#other colours
#royalblue4
#blue3