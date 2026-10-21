library(terra)
library(ggplot2)
library(tidyterra)

path_pre  <- "F://chikglobal_chinaSDM//chik//Result//future//chik_ssp585_2050//1_chik_ssp585_2050__Distribution_Prediction.tif"
path_post <- "D://Integrated vector management model//Chikv//result//Medium//ssp585-2050//chik_Medium_ssp585-2050_Distribution_Prediction.tif"
r_pre  <- rast(path_pre)
r_post <- rast(path_post)
print(r_pre)
r_post_aligned <- resample(r_post, r_pre, method = "bilinear")

r_reduction <- r_pre - r_post_aligned

names(r_reduction) <- "Reduction_Value"
writeRaster(r_reduction, "D://Integrated vector management model//Chikv//result//Risk_Reduction_Map//SSP585-2050//chik_Medium_SSP585-2050_Risk_Reduction_Map.tif", overwrite=TRUE)

ggplot() +
  geom_spatraster(data = r_reduction) +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 0,
    na.value = "transparent",
    name = "Difference\n(Pre - Post)"
  ) +
  coord_sf(crs = 4326) +
  theme_minimal() +
  labs(
    title = "Chikungunya Distribution Change (2030)",
    subtitle = "SSP126 vs Medium Integrated Vector Management"
  )

