library(ggplot2)  
library(maps)     
library(dplyr)   
library(mapdata)
library(readxl)
world_map <- map_data("world")
X1990_2025_Aedes_aegypti <- read_excel("D://chikglobal_chinaSDM//Mosquitoe//Aedes_aegypti//1990-2025_Aedes_aegypti.xlsx")
X1990_2025_Aedes_aegypti<-X1990_2025_Aedes_aegypti[, c(4, 5)]
ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_point(data = X1990_2025_Aedes_aegypti,
             aes(x = Longitude, y = Latitude),
             size = 1.0, alpha = 0.7,
             color = "#00A1D5") +
  scale_color_viridis_c(option = "plasma") +
  coord_quickmap() +
  labs(title = "Global Map of Ae.Aegypti occurrence",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid = element_blank()  
  )
