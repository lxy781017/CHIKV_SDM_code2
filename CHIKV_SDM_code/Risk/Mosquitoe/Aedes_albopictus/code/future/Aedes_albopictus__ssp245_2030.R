install.packages("devtools")
library(devtools)
devtools::install_github("cjcarlson/embarcadero")
library(embarcadero)
library(raster)
library(readxl)
setwd("D://chikglobal_chinaSDM//Mosquitoe//Aedes_albopictus//Variables//ssp245_2030")
set.seed(123)

#Section 1: Loading Variable Data
env <- raster::stack("Bio2_ssp245_2021-2040.tif",
                     "Bio9_ssp245_2021-2040.tif",
                     "Bio14_ssp245_2021-2040.tif",
                     "Bio18_ssp245_2021-2040.tif",
                     "Bio19_ssp245_2021-2040.tif",
                     "2019GDP.tif",
                     "Cd_2020.tif",
                     "PH_1985.tif")

names(env) <- c( "Bio2", "Bio9","Bio14", "Bio18", "Bio19", 
                 "Cd","PH")

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
  env[["Bio2"]], 
  env[["Bio9"]], 
  env[["Bio14"]],
  env[["Bio18"]],
  env[["Bio19"]],
  env[["GDP"]],
  env[["Cd"]],
  env[["PH"]]
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
     main = 'Aedes_albopictus',
     zlim = c(0,1),
     axis.args=list(at=pretty(0:1), labels=pretty(0:1)),
     legend.args=list(text='Probability', side=2, line=1.3))

points(pts.sp1[!is.na(raster::extract(species_Global[[1]], SpatialPoints(pts.sp1[,1:2]))),],
       col = 'black',
       pch = 16,
       cex = 0.3)
#Result 1：Output distribution probability map 
raster::writeRaster(species_Global$layer.1,filename = "D://chikglobal_chinaSDM//Mosquitoe//Aedes_albopictus//Result//Aedes_albopictus_ssp245_2030//1_Aedes_albopictus_ssp245_2030_present_Distribution_Prediction.tif",format = "GTiff")

#Result 2：Output binary distribution probability map
threshold <-0.40
binary_map <- species_Global$layer.1 > threshold
binary_map <- as.integer(binary_map)  
raster::writeRaster(binary_map,filename = "D://chikglobal_chinaSDM//Mosquitoe//Aedes_albopictus//Result//Aedes_albopictus_ssp245_2030//2_Aedes_albopictus_ssp245_2030_present_binary_Prediction.tif",format = "GTiff")

#Result 3：Output Uncertainty map
raster::writeRaster(species_Global[[3]]- species_Global[[2]],filename = "D://chikglobal_chinaSDM//Mosquitoe//Aedes_albopictus//Result//Aedes_albopictus_ssp245_2030//3_Aedes_albopictus_ssp245_2030_present_uncertainty_Prediction.tif",format = "GTiff")

#Result 4：Output varimp
varimp(model = bart_model,plots = TRUE)
png("D://chikglobal_chinaSDM//Mosquitoe//Aedes_albopictus//Result//Aedes_albopictus_ssp245_2030//4_Aedes_albopictus_ssp245_2030_variable_importance_plot.tif", width = 800, height = 600)
varimp(model = bart_model, plots = TRUE)
dev.off()


