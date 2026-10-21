library(ggplot2)  
library(maps)     
library(dplyr)   
library(mapdata)
library(readxl)
world_map <- map_data("world")
X1874_2023_chik <- read.csv("D://chikglobal_chinaSDM//chik//chik_occ.csv")
X1874_2023_chik<-X1874_2023_chik[, c(1, 2)]
ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_point(data = X1874_2023_chik,
             aes(x = Longitude, y = Latitude),
             size =1.0, alpha = 0.7,
             color = "#DC0000B2") +
  scale_color_viridis_c(option = "plasma") +
  coord_quickmap() +
  labs(title = "Global Map of CHIKF occurrence",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid = element_blank() 
  )
