library(ggplot2)  
library(maps)    
library(dplyr)    
library(mapdata)
library(readxl)
world_map <- map_data("world")
X1990_2025_Aedes_albopictus <- read_excel("D://chikglobal_chinaSDM//Mosquitoe//Aedes_albopictus//species_records_clean.xlsx")
X1990_2025_Aedes_albopictus<-X1990_2025_Aedes_albopictus[, c(1, 2)]
ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_point(data = X1990_2025_Aedes_albopictus,
             aes(x = Longitude, y = Latitude),
             size =1.0, alpha = 0.7,
             color = "#DC0000B2") +
  scale_color_viridis_c(option = "plasma") +
  coord_quickmap() +
  labs(title = "Global Map of Ae.albopictus occurrence",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid = element_blank()  
  )
