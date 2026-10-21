library(terra)
library(parallel)
library(terra)
# =============== Setp 1.Simulation of Vector Control Scenarios ===================
# 1.Data Retrieval of Harmonized Mosquito Layers
setwd("G://China_chik//output//aligned")
fp_aeg   <- "aegypti_base_aligned.tif"
fp_alb   <- "albo_base_aligned.tif"
fp_urban <- "urban_base_aligned.tif"
fp_rice  <- "rice_base_aligned.tif"

out_dir <- "G://China_chik//output//model"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

r_aeg  <- rast(fp_aeg)    
r_alb  <- rast(fp_alb)    
w_urban <- rast(fp_urban) 
w_rice  <- rast(fp_rice)  

logit  <- function(p) log(p / (1 - p))  
expit  <- function(z) 1 / (1 + exp(-z)) 
clamp01 <- function(r) clamp(r, 0, 1, values = TRUE) 

eps <- 1e-6 
r_aeg <- clamp(r_aeg, eps, 1 - eps, values = TRUE) 
r_alb <- clamp(r_alb, eps, 1 - eps, values = TRUE) 
w_urban <- clamp01(w_urban)
w_rice  <- clamp01(w_rice)

# 2.Parameterization and Model Inputs
#coverage
cmax_SR_city  <- 0.70    
cmax_SR_rural <- 0.35   

cmax_LV_rice  <- 0.40  
cmax_LV_else  <- 0.80   

cmax_AD_city  <- 1  
cmax_AD_rural <- 0.5  

# Technical Efficacy
eta <- list(
  SR = c(aeg = 0.72, alb = 0.78), 
  LV = c(aeg = 0.78, alb = 0.79), 
  AD = c(aeg = 0.73, alb = 0.78) 
)


# 3.Construction of Spatial Coverage Rasters
w_rural <- clamp01(1 - w_urban)

c_SR <- cmax_SR_city * w_urban + cmax_SR_rural * w_rural
c_SR <- clamp01(c_SR)

w_else <- clamp01(1 - w_rice)
c_LV <- cmax_LV_rice * w_rice + cmax_LV_else * w_else
c_LV <- clamp01(c_LV)

c_AD <- cmax_AD_city * w_urban + cmax_AD_rural * w_rural
c_AD <- clamp01(c_AD * ad_season_factor)

writeRaster(c_SR, file.path(out_dir, "coverage_SR.tif"), overwrite = TRUE, NAflag = -9999)
writeRaster(c_LV, file.path(out_dir, "coverage_LV.tif"), overwrite = TRUE, NAflag = -9999)
writeRaster(c_AD, file.path(out_dir, "coverage_AD.tif"), overwrite = TRUE, NAflag = -9999)

# 4.Synthesis of Composite Risk Reduction
one <- rast(r_aeg); values(one) <- 1 

rho_aeg <- one
rho_aeg <- rho_aeg * (1 - eta$SR["aeg"] * c_SR)
rho_aeg <- rho_aeg * (1 - eta$LV["aeg"] * c_LV)
rho_aeg <- rho_aeg * (1 - eta$AD["aeg"] * c_AD)
rho_aeg <- clamp01(1 - rho_aeg)

rho_alb <- one
rho_alb <- rho_alb * (1 - eta$SR["alb"] * c_SR)
rho_alb <- rho_alb * (1 - eta$LV["alb"] * c_LV)
rho_alb <- rho_alb * (1 - eta$AD["alb"] * c_AD)
rho_alb <- clamp01(1 - rho_alb)

writeRaster(rho_aeg, file.path(out_dir, "rho_aegypti.tif"), overwrite = TRUE, NAflag = -9999) 
writeRaster(rho_alb, file.path(out_dir, "rho_albopictus.tif"), overwrite = TRUE, NAflag = -9999)

# 5.odds-scaling
rho_aeg <- terra::rast("G://China_chik//output//model//rho_aegypti.tif")
rho_alb <- terra::rast("G://China_chik//output//model//rho_albopictus.tif")

Pp_aeg <- expit(logit(r_aeg) + log(1 - rho_aeg))
Pp_alb <- expit(logit(r_alb) + log(1 - rho_alb))

Pp_aeg <- clamp01(Pp_aeg)
Pp_alb <- clamp01(Pp_alb)

writeRaster(Pp_aeg, file.path(out_dir, "aegypti_control.tif"), overwrite = TRUE, NAflag = -9999)
writeRaster(Pp_alb, file.path(out_dir, "albo_control.tif"),   overwrite = TRUE, NAflag = -9999)

 # 6. quality inspection
qc <- function(r, nm){
  s <- global(r, c("min","mean","max"), na.rm = TRUE)
  cat(sprintf("%-18s  min/mean/max = %.4f / %.4f / %.4f\n",
              nm, s[1,1], s[1,2], s[1,3]))
}

qc(c_SR, "c_SR"); qc(c_LV, "c_LV"); qc(c_AD, "c_AD")

qc(rho_aeg, "rho_aeg"); qc(rho_alb, "rho_alb")

qc(r_aeg, "P_aeg");  qc(Pp_aeg, "Pp_aeg")
qc(r_alb, "P_alb");  qc(Pp_alb, "Pp_alb")

writeRaster(Pp_aeg - r_aeg, file.path(out_dir, "deltaP_aeg.tif"), overwrite=TRUE, NAflag=-9999)
writeRaster(Pp_alb - r_alb, file.path(out_dir, "deltaP_alb.tif"), overwrite=TRUE, NAflag=-9999)


# =============== Setp 2.Cost-Benefit Analysis ===================
library(terra)
library(dplyr)
library(ggplot2)

setwd("G://China_chik//output//aligned")
out_dir <- "F://benefit"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 1.Data Loading
cat(">>> Loading Rasters...\n")
r_risk_base <- rast("F://benefitvari//chik_585-20501.tif")         
r_risk_ctrl <- rast("F://benefitvari//chik_Medium_ssp585-20501.tif") 

r_effort_coverage_SR  <- rast("D://Integrated vector management model//Mosquitoe//result//Coverage//Medium//coverage_Medium_SR.tif") 
r_effort_coverage_LV  <- rast("D://Integrated vector management model//Mosquitoe//result//Coverage//Medium//coverage_Medium_LV.tif") 
r_effort_coverage_AD  <- rast("D://Integrated vector management model//Mosquitoe//result//Coverage//Medium//coverage_Medium_AD.tif") 
r_effort <- (r_effort_coverage_SR+r_effort_coverage_LV+r_effort_coverage_AD )/3

r_pop <- rast("D://Integrated vector management model//Chikv//vari//pop_base_aligned.tif")   
r_smod <- rast("D://Integrated vector management model//Chikv//vari//ghsl_smod_aligned.tif") 

r_effort <- clamp(r_effort, 0, 1)  
r_pop <- subst(r_pop, NA, 0)  

 
# 2.Parameters Definition
TH <- 0.41 
cat(">>> Defining Zones based on GHSL...\n")
mask_city  <- r_smod == 30
mask_peri  <- (r_smod >= 21) & (r_smod <= 23) 
mask_rural <- r_smod <= 13

# 3.Calculating Benefit & Cost

cat(">>> Calculating Cost and Benefit...\n")
mask_success <- (r_risk_base > TH) & (r_risk_ctrl <= TH)
r_pop_protected <- mask(r_pop, mask_success, maskvalues=FALSE, updatevalue=0)
r_area_km2 <- cellSize(r_risk_base, unit="km")
r_cost_km2 <- r_effort * r_area_km2
cat(">>> Running Zonal Statistics...\n")

calc_population_protection_yield_per_treated_area <- function(zone_mask, zone_name) {
  pop_val <- global(mask(r_pop_protected, zone_mask, maskvalues=FALSE), "sum", na.rm=TRUE)[1,1]
  cost_val <- global(mask(r_cost_km2, zone_mask, maskvalues=FALSE), "sum", na.rm=TRUE)[1,1]
  efficiency <- pop_val / cost_val
  return(data.frame(Zone=zone_name, Benefit_Pop=pop_val, Cost_Area=cost_val,Population_protection_yield_per=efficiency))
}

df_city  <- calc_population_protection_yield_per_treated_area(mask_city,  "Urban Centre")
df_peri  <- calc_population_protection_yield_per_treated_area(mask_peri,  "Peri-urban")
df_rural <- calc_population_protection_yield_per_treated_area(mask_rural, "Rural")

final_stats <- rbind(df_city, df_peri, df_rural)
print(final_stats)

write.csv(final_stats, file.path(out_dir, "Population_protection_yield_per_Table_Final_Medium_ssp585_2050.csv"), row.names=FALSE)

# 4.Visualization
cat(">>> Generating Plots...\n")

final_stats$Zone <- factor(final_stats$Zone, levels=c("Urban Centre", "Peri-urban", "Rural"))

p_population_protection_yield_per_treated_area <- ggplot(final_stats, aes(x=Zone, y=ROI, fill=Zone)) +
  geom_bar(stat="identity", width=0.6, color="black", alpha=0.8) +
  geom_text(aes(label=sprintf("%.1f", ROI)), vjust=-0.5, size=5) +
  scale_fill_manual(values=c("#E41A1C", "#FF7F00", "#4DAF4A")) + 
  labs(title = "Cost-Effectiveness of Vector Control by Settlement Type",
       subtitle = "Peri-urban areas show the highest population protection per unit area",
       y = "Efficiency (Persons Protected / km² Intervention)",
       x = "") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12))

ggsave(file.path(out_dir, "Population_protection_yield_per_Table_Final_Medium_ssp585_2050.png"), p_population_protection_yield_per_treated_area, width=6, height=5, dpi=300)


r_zones_raw <- mask(r_smod, mask_success, maskvalues=FALSE, updatevalue=NA)
m <- matrix(c(0,  19, 1,   
              20, 29, 2,   
              29, 30, 3),  
            ncol=3, byrow=TRUE)

r_zones_class <- classify(r_zones_raw, m)

levels(r_zones_class) <- data.frame(id=c(1, 2, 3), 
                                    label=c("Rural", "Peri-urban", "Urban Centre"))

writeRaster(r_zones_class, file.path(out_dir, "Map_Stratified_Intervention_Zones_Medium_ssp585_2050.tif"), overwrite=TRUE)

my_colors <- c("#4DAF4A", "#FF7F00", "#E41A1C") 

png(file.path(out_dir, "Map_Stratified_Visual_Medium_ssp585_2050.png"), width=2000, height=1800, res=300)

plot(r_zones_class, 
     col = my_colors,
     type = "classes",   
     levels = c("Rural", "Peri-urban", "Urban Centre"),
     main = "Priority Intervention Zones by Settlement Type",
     plg = list(title = "Zone Type", title.cex=0.8), 
     mar = c(2, 2, 2, 2), 
     axes = FALSE)        

dev.off() 

if(require("tidyterra") & require("ggplot2")) {
  
  p_map <- ggplot() +
    geom_spatraster(data = r_zones_class) +
    scale_fill_manual(values = my_colors, 
                      na.value = "transparent", 
                      name = "Zone Type",
                      labels = c("Rural", "Peri-urban", "Urban Centre")) +
    labs(title = "Spatial Distribution of Cost-Effective Intervention Zones",
         subtitle = "Areas where intervention reduces risk from >0.5 to <=0.5") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold", size=14),
          legend.position = "bottom")
  
  ggsave(file.path(out_dir, "Population_protection_yield_per_Table_Final_Medium_ssp585_2050.png"), p_map, width=8, height=6, dpi=300)
}

savehistory(file = )

