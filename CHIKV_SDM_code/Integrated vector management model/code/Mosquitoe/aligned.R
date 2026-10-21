library(terra)
library(parallel)

terraOptions(
  memfrac  = 0.7,              
  todisk   = TRUE,            
  progress = 1,      
  tempdir = "G://China_chik//output//temp"
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
setwd("G://China_chik//")
###########################
r_aeg  <- rast("aligned/aegypti_base_aligned.tif")
###########################
r_aeg  <- rast("output//aligned/aegypti_base_aligned.tif")
###########################
r_aeg  <- rast("output//aligned/aegypti_base_aligned.tif")
r_alb_a <- rast("output//aligned/albo_base_aligned.tif")
r_pop_a  <- rast("output//aligned/pop_base_aligned.tif")
r_uw_a   <- rast("output//aligned/urban_base_aligned.tif")
r_rw_a   <- rast("output//aligned/rice_base_aligned.tif")
r_risk_a <- rast("output//aligned/R_base_aligned.tif")
# -----------------------------------------------------------------------------------------------------
qc_tbl <- rbind(
  data.frame(layer = "aegypti_base", crs = crs(r_aeg) == crs(ref),
             res_ok = all(res(r_aeg) == res(ref)), ext_ok = all(ext(r_aeg) == ext(ref))),
  data.frame(layer = "alb_base",     crs = crs(r_alb_a) == crs(ref),
             res_ok = all(res(r_alb_a) == res(ref)), ext_ok = all(ext(r_alb_a) == ext(ref))),
  data.frame(layer = "pop_base",     crs = crs(r_pop_a) == crs(ref),
             res_ok = all(res(r_pop_a) == res(ref)), ext_ok = all(ext(r_pop_a) == ext(ref))),
  data.frame(layer = "urban_base",  crs = crs(r_uw_a) == crs(ref),
             res_ok = all(res(r_uw_a) == res(ref)), ext_ok = all(ext(r_uw_a) == ext(ref))),
  data.frame(layer = "rice_base",   crs = crs(r_rw_a) == crs(ref),
             res_ok = all(res(r_rw_a) == res(ref)), ext_ok = all(ext(r_rw_a) == ext(ref)))
)
# -----------------------------------------------------------------------------------------------------
ref <- r_aeg

qc_tbl <- rbind(
  data.frame(layer = "aegypti_base", crs = crs(r_aeg) == crs(ref),
             res_ok = all(res(r_aeg) == res(ref)), ext_ok = all(ext(r_aeg) == ext(ref))),
  data.frame(layer = "alb_base",     crs = crs(r_alb_a) == crs(ref),
             res_ok = all(res(r_alb_a) == res(ref)), ext_ok = all(ext(r_alb_a) == ext(ref))),
  data.frame(layer = "pop_base",     crs = crs(r_pop_a) == crs(ref),
             res_ok = all(res(r_pop_a) == res(ref)), ext_ok = all(ext(r_pop_a) == ext(ref))),
  data.frame(layer = "urban_base",  crs = crs(r_uw_a) == crs(ref),
             res_ok = all(res(r_uw_a) == res(ref)), ext_ok = all(ext(r_uw_a) == ext(ref))),
  data.frame(layer = "rice_base",   crs = crs(r_rw_a) == crs(ref),
             res_ok = all(res(r_rw_a) == res(ref)), ext_ok = all(ext(r_rw_a) == ext(ref)))
)
qc_tbl <- rbind(qc_tbl,
                data.frame(layer = "R_base", crs = crs(r_risk_a) == crs(ref),
                           res_ok = all(res(r_risk_a) == res(ref)),
                           ext_ok = all(ext(r_risk_a) == ext(ref))))
qc_tbl
###########################
r_aeg  <- rast("output//aligned/aegypti_base_aligned.tif")
r_alb_a <- rast("output//aligned/albo_base_aligned.tif")
r_uw_a   <- rast("output//aligned/urban_base_aligned.tif")
r_rw_a   <- rast("output//aligned/rice_base_aligned.tif")
# -----------------------------------------------------------------------------------------------
out_dir <- "G://China_chik//output//model"

library(terra)
setwd("G://China_chik//output//aligned")
# --------------------------------------------------------------------------------------------------------
fp_aeg   <- "aegypti_base_aligned.tif"
fp_alb   <- "albo_base_aligned.tif"
fp_urban <- "urban_base_aligned.tif"
fp_rice  <- "rice_base_aligned.tif"
# --------------------------------------------------------------------------------------------------------
out_dir <- "G://China_chik//output//model"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

r_aeg  <- rast(fp_aeg)    
r_alb  <- rast(fp_alb)    
w_urban <- rast(fp_urban) 
w_rice  <- rast(fp_rice)  
w_rice
w_urban
r_alb
r_aeg
fp_rice
