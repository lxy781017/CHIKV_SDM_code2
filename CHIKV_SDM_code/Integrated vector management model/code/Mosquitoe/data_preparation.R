######## ====== 1.Data Standardization(0–1) ============================================############
library(terra)
setwd("G://China_chik//data//mosquitos_current_middle//")
fp_urban_raw <- "urban_2015_China.tif"  
fp_rice_raw  <- "Rice_Base.tif" 
out_dir <- "G://China_chik//data//mosquitos_current_middle//"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

r_urban <- rast(fp_urban_raw)
r_rice  <- rast(fp_rice_raw)
urban_w <- clamp(r_urban / 0.699837, lower = 0, upper = 1, values = TRUE)

writeRaster(
  urban_w,
  file.path(out_dir, "urban_weight_0_1.tif"),
  overwrite = TRUE,
  NAflag = -9999
)

rice_log <- log1p(r_rice) 
qs <- global(
  rice_log,
  fun = quantile,
  probs = c(0.02, 0.98),
  na.rm = TRUE,
  size = 1e6   
)

q_low  <- as.numeric(qs[1, 1])
q_high <- as.numeric(qs[1, 2])


rice_w <- (rice_log - q_low) / (q_high - q_low)
rice_w <- clamp(rice_w, lower = 0, upper = 1, values = TRUE)

writeRaster(
  rice_w,
  file.path(out_dir, "rice_weight_0_1.tif"),
  overwrite = TRUE,
  NAflag = -9999
)


# ================= 2.Raster alignment and quality assurance =================
fp_aeg_base <- "D://Integrated vector management model//Mosquitoe//data//SSP585-2050//ae_china_585-2050.tif"
fp_alb_base <- "D://Integrated vector management model//Mosquitoe//data//SSP585-2050//alb_china_585-2050.tif"
fp_urban_base <- "D://Integrated vector management model//Mosquitoe//data//SSP585-2050//urban_base_aligned.tif"
fp_rice_base <- "D://Integrated vector management model//Mosquitoe//data//SSP585-2050//rice_base_aligned.tif"

out_dir <- "D://Integrated vector management model//Mosquitoe//cut + align//SSP585-2050"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

r_aeg  <- rast(fp_aeg_base)
r_alb  <- rast(fp_alb_base)
r_urban  <- rast(fp_urban_base)
r_rice <- rast(fp_rice_base)
ref <- r_aeg
align_to_ref <- function(r, ref, method = "bilinear") {
  if (is.lonlat(ref)) {
    terra::project(r, ref, method = method)
  } else {
    terra::warp(r, ref, method = method)
  }
}

r_alb_a <- align_to_ref(r_alb, ref, "bilinear")
r_uw_a  <- align_to_ref(r_urban,  ref, "bilinear")
r_rw_a  <- align_to_ref(r_rice,  ref, "bilinear")

writeRaster(r_aeg,  file.path(out_dir, "aegypti_SSP585-2050_aligned.tif"),
            overwrite = TRUE, NAflag = -9999)
writeRaster(r_alb_a,file.path(out_dir, "albo_SSP585-2050_aligned.tif"),
            overwrite = TRUE, NAflag = -9999)
writeRaster(r_uw_a, file.path(out_dir, "urban_base_aligned.tif"),
            overwrite = TRUE, NAflag = -9999)
writeRaster(r_rw_a, file.path(out_dir, "rice_base_aligned.tif"),
            overwrite = TRUE, NAflag = -9999)

qc_tbl <- rbind(
  data.frame(layer = "aegypti_SSP585-2050_aligned.tif", crs = crs(r_aeg) == crs(ref),
             res_ok = all(res(r_aeg) == res(ref)), ext_ok = all(ext(r_aeg) == ext(ref))),
  data.frame(layer = "albo_SSP585-2050_aligned.tif",     crs = crs(r_alb_a) == crs(ref),
             res_ok = all(res(r_alb_a) == res(ref)), ext_ok = all(ext(r_alb_a) == ext(ref))),
  data.frame(layer = "urban_base_aligned.tif",  crs = crs(r_uw_a) == crs(ref),
             res_ok = all(res(r_uw_a) == res(ref)), ext_ok = all(ext(r_uw_a) == ext(ref))),
  data.frame(layer = "rice_base_aligned.tif",   crs = crs(r_rw_a) == crs(ref),
             res_ok = all(res(r_rw_a) == res(ref)), ext_ok = all(ext(r_rw_a) == ext(ref)))
)

print(qc_tbl)

check_01 <- function(r, nm) {
  s <- global(r, fun = c("min","mean","max"), na.rm = TRUE)
  cat(sprintf("%-20s min/mean/max = %.4f / %.4f / %.4f\n",
              nm, s[1,1], s[1,2], s[1,3]))
}
check_01(r_aeg,  "aegypti_SSP585-2050_aligned.tif")
check_01(r_alb_a,"albo_SSP585-2050_aligned.tif")
check_01(r_uw_a, "urban_base_aligned.tif")
check_01(r_rw_a, "rice_base_aligned.tif")

