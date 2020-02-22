library(maps)
library(ggmap)
library(socviz)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(mapproj)
us_states <- map_data("state")
county_map



values <- read.csv("CSV/Subprime_Credit_Population_by_County_Percent.csv")
colnames(values) <- c("county", "id", "score")
values$id <- as.integer(values$id)
county_map$id <- as.integer(county_map$id)

county_full <- left_join(county_map, values, by = "id")


p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat, fill = score, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
#p2 <- p1 + continuous_scale(palette="Blues")

p1 + labs(fill = "Subprime Population") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() + theme(legend.position = "bottom")
