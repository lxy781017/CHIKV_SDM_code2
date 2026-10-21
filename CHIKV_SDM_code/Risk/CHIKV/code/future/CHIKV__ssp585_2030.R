install.packages("devtools")
library(devtools)
devtools::install_github("cjcarlson/embarcadero")
library(embarcadero)
library(raster)
library(readxl)
setwd("D://chikglobal_chinaSDM//chik//vari//future//ssp585_2030")
set.seed(123)

#Section 1: Loading Variable Data
env <- raster::stack("Bio8_ssp585_2021-2040.tif",
                     "Bio9_ssp585_2021-2040.tif",
                     "Bio18_ssp585_2021-2040.tif",
                     "Cd_2020.tif",
                     "NDVI_2025.tif",
                     "1_Aedes_aegypti_ssp585_2030_present_Distribution_Prediction.tif",
                     "1_Aedes_albopictus_ssp585_2030_Distribution_Prediction.tif")

names(env) <- c( "Bio8", "Bio9","Bio18", 
                 "Cd","NDVI",
                "Aedes_aegypti","Aedes_albopictus")

#Section 2: Generated BART model
#Section 2.1: Generated pseudo-absence points at a 1:1 ratio with presence points
absence_points <- randomPoints(env, n =nrow(pres_env))  
absence_env <- extract(env, absence_points) 

presence_data <- data.frame(pres_env) 
presence_data$presence <- 1  

absence_data <- data.frame(absence_env)  
absence_data$presence <- 0  

all_data <- rbind(presence_data, absence_data)

#Section 2.2: Generated BART model
bart_model <-  bart.step(x.data = all_data[, selected_vars],
                         y.data = all_data$presence,
                         full = TRUE,
                         quiet = TRUE)
summary(bart_model)
#Section 3: Extract environmental data for each point
pres_env <- raster::extract(x = env,y = pts.sp1[,1:2])
pres_env <- na.omit(pres_env) 
prediction_env <- stack(
  env[["Bio8"]], 
  env[["Bio9"]], 
  env[["Bio18"]],
  env[["NDVI"]],
  env[["Cd"]],
  env[["Aedes_aegypti"]],
  env[["Aedes_albopictus"]]
)
species_Global <- predict(object = bart_model,
                          x.layers = prediction_env,
                          splitby = 20,
                          quantiles = c(0.025, 0.975),
                          quiet = TRUE)

col_counts <- sapply(prediction_env, function(x) ncol(x))
abnormal_indices <- which(col_counts != col_counts[1])
lapply(prediction_env[abnormal_indices], head)

plot(species_Global[[1]],
     box = FALSE,
     axes = FALSE,
     main = 'chik',
     zlim = c(0,1),
     axis.args=list(at=pretty(0:1), labels=pretty(0:1)),
     legend.args=list(text='Probability', side=2, line=1.3))

points(pts.sp1[!is.na(raster::extract(species_Global[[1]], SpatialPoints(pts.sp1[,1:2]))),],
       col = 'black',
       pch = 16,
       cex = 0.3)
#Result 1：Output distribution probability map 
raster::writeRaster(species_Global$layer.1,filename = "D://chikglobal_chinaSDM//chik//Result//chik_ssp585_2030//1_chik_ssp_ssp585_2030_present_Distribution_Prediction.tif",format = "GTiff")

#Result 2：Output binary distribution probability map
threshold <-0.41
binary_map <- species_Global$layer.1 > threshold
binary_map <- as.integer(binary_map)  
raster::writeRaster(binary_map,filename = "D://chikglobal_chinaSDM//chik//Result//chik_ssp_ssp585_2030//2_chik_ssp_ssp585_2030_present_binary_Prediction.tif",format = "GTiff")

#Result 3：Output Uncertainty map
raster::writeRaster(species_Global[[3]]- species_Global[[2]],filename = "D://chikglobal_chinaSDM//chik//Result//chik_ssp_ssp585_2030//3_chik_ssp_ssp585_2030_present_uncertainty_Prediction.tif",format = "GTiff")

#Result 4：Output varimp
varimp(model = bart_model,plots = TRUE)
png("D://chikglobal_chinaSDM//chik//Result//chik_ssp_ssp585_2030//4_chik_ssp_ssp585_2030_variable_importance_plot.tif", width = 800, height = 600)
varimp(model = bart_model, plots = TRUE)
dev.off()

#Section 6:Clip the map to the China region
library(raster)
library(sf)
china_boundary <- st_read("D://chikglobal_chinaSDM//202405更新_2024年省市县三级行政区划数据（审图号：GS（2024）0650号）//shp格式的数据（调整过行政区划代码，补全省市县信息）//省.shp")
ten_dash_line <- st_read("D://chikglobal_chinaSDM//202405更新_2024年省市县三级行政区划数据（审图号：GS（2024）0650号）//shp格式的数据（调整过行政区划代码，补全省市县信息）//十段线.shp") 
global_raster <- raster("D://chikglobal_chinaSDM//chik//Result//chik_ssp245_2030//1_chik_ssp585_2030_present_Distribution_Prediction.tif")
crs(global_raster) <- "+proj=longlat +datum=WGS84"  
target_crs <- "+proj=longlat +datum=WGS84"  
china_boundary <- st_transform(china_boundary, target_crs)
ten_dash_line <- st_transform(ten_dash_line, target_crs)
china_raster_complete <- mask(crop(global_raster, china_boundary), china_boundary)
plot(china_raster_complete, 
     main = "CHIKV in China",
     axes = FALSE,  
     box = FALSE)   
plot(china_boundary$geometry, add = TRUE, border = "darkgray", lwd = 0.5)  
plot(ten_dash_line$geometry, add = TRUE, col = "red", lwd = 2)  


