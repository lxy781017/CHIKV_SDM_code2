library(parallel)
library(terra)
terraOptions(
  memfrac  = 0.7,             
  todisk   = TRUE,            
  progress = 1,     
  tempdir = "D://R_temp//RtmpaMtEPI"
)

Sys.setenv(
  GDAL_NUM_THREADS = "ALL_CPUS",
  OMP_NUM_THREADS  = detectCores()
)

wopt <- list(
  datatype = "FLT4S",                       
  NAflag   = -9999,
  gdal     = c("COMPRESS=LZW", "PREDICTOR=2",
               "BIGTIFF=YES", "NUM_THREADS=ALL_CPUS")
)

library(terra)
pop_raster <- rast("F://chikglobal_chinaSDM//pop_count//pop_china.tif")  
risk_raster <- rast("D://Integrated vector management model//Chikv//result//Risk_Reduction_Map//current//chik_High_current_Risk_Reduction_Map.tif")     

if (!compareGeom(pop_raster, risk_raster, stopOnError = FALSE)) {
  risk_raster <- resample(risk_raster, pop_raster, method = "bilinear")  
  risk_raster <- mask(risk_raster, pop_raster)  
}

risk_pop_raster <- pop_raster * risk_raster  
total_risk_pop <- global(risk_pop_raster, sum, na.rm = TRUE)  
print(paste("Total count：", total_risk_pop[1, 1], "person"))
plot1 <- plot(risk_pop_raster, main = "Spatial distribution of at-risk populations")  

high_risk_mask <- risk_raster >= 0.41 
high_risk_pop_raster <- pop_raster * high_risk_mask  # TRUE=1，FALSE=0
total_high_risk_pop <- global(high_risk_pop_raster, sum, na.rm = TRUE)
print(paste("High risk count：", total_high_risk_pop[1, 1], "人"))
plot2 <- plot(high_risk_pop_raster, main = "Spatial distribution of Low-risk populations")  

output_path <- "D://Integrated vector management model//Chikv//result//RiskPop_After"  

if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

writeRaster(
  x =risk_pop_raster, 
  filename = paste0(output_path, "RiskPop_After_High_current_1.tif"),  
  filetype = "GTiff",  
  overwrite = TRUE  
)


