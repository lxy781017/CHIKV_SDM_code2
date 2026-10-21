library(terra)
library(dplyr)
library(ggplot2)
library(ggsci) 

set.seed(2024)

# 1. Data Acquisition and Preprocessing
Population_protection_yield_per_treated_area_path  <- "D://Integrated vector management model//Chikv//result//benefit//current//Map_Stratified_Intervention_Zones_Low.tif"
smod_path <- "D://Integrated vector management model//Chikv//vari//ghsl_smod_aligned.tif"
r_Population_protection_yield_per_treated_area <- rast(Population_protection_yield_per_path)
names(r_Population_protection_yield_per_treated_area) <- "Population_protection_yield_per_Value"
r_smod <- rast(smod_path)
r_smod_aligned <- resample(r_smod, r_Population_protection_yield_per_treated_area, method = "near")
names(r_smod_aligned) <- "SMOD_Code"
r_stack <- c(r_Population_protection_yield_per_treated_area, r_smod_aligned)

# 2. Spatial Sampling and Data Filtration
# ==============================================================================
df_sample <- spatSample(r_stack, size = 3000, method = "random", na.rm = TRUE, as.df = TRUE, xy = FALSE)

df_analysis <- df_sample %>%
  filter(!is.na(Population_protection_yield_per_treated_area_Value) & !is.na(SMOD_Code)) %>%
  mutate(Population_protection_yield_per_treated_area_Value = as.numeric(Population_protection_yield_per_treated_area_Value)) %>%
  filter(Population_protection_yield_per_treated_area_Value > -100) %>% 
  mutate(
    Zone = case_when(
      SMOD_Code == 30 ~ "Urban Centre",
      SMOD_Code >= 21 & SMOD_Code <= 23 ~ "Peri-urban",
      SMOD_Code <= 13 & SMOD_Code > 0   ~ "Rural", 
      TRUE ~ "Other"
    )
  ) %>%
  filter(Zone != "Other") %>%
  mutate(Zone = factor(Zone, levels = c("Rural", "Peri-urban", "Urban Centre")))

# 3. Box-and-whisker plots of ROI values
if(nrow(df_analysis) > 0) {
 
  rects <- data.frame(
    Zone = factor(c("Rural", "Peri-urban", "Urban Centre"), levels = c("Rural", "Peri-urban", "Urban Centre")),
    xmin = c(0.5, 1.5, 2.5),
    xmax = c(1.5, 2.5, 3.5),
    ymin = -Inf, 
    ymax = Inf
  )

  my_colors <- c("Rural" = "#00A087FF", 
                 "Peri-urban" = "#F39B7FFF", 
                 "Urban Centre" = "#E64B35FF")
  bg_colors <- c("Rural" = "#E0F3DB", 
                 "Peri-urban" = "#FEE0D2", 
                 "Urban Centre" = "#FDE0DD")
  
  p_beautiful <- ggplot() +
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Zone), 
              alpha = 0.2, inherit.aes = FALSE) + 
    geom_jitter(data = df_analysis, aes(x = Zone, y = Population_protection_yield_per_Value, color = Zone), 
                width = 0.25, size = 1.2, alpha = 0.5) +
    geom_violin(data = df_analysis, aes(x = Zone, y = Population_protection_yield_per_Value, color = Zone),
                fill = "white", alpha = 0.6, trim = FALSE, size = 0.8) +
    geom_boxplot(data = df_analysis, aes(x = Zone, y = Population_protection_yield_per_Value),
                 width = 0.15, fill = "white", color = "grey30", alpha = 0.9, outlier.shape = NA) +
    stat_summary(data = df_analysis, aes(x = Zone, y = Population_protection_yield_per_Value, group = 1),
                 fun = mean, geom = "line", linetype = "dashed", color = "grey40", size = 0.8) +
    stat_summary(data = df_analysis, aes(x = Zone, y = Population_protection_yield_per_Value),
                 fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
     scale_fill_manual(values = my_colors) +   
    scale_color_manual(values = my_colors) +  
    
    labs(
      title = "Efficiency across Urbanization Gradients",
      subtitle = paste("Sample Size: N =", nrow(df_analysis)),
      x = NULL,
      y = "Intervention Population_protection_yield_per_treated_area Value"
    ) +
    
    theme_classic(base_size = 14) +  
    theme(
      legend.position = "none",      
      panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"), # 仅保留横向网格
      axis.line = element_blank(),   
      axis.text.x = element_text(size = 12, face = "bold", color = "black", vjust = 5), # 调整X轴标签位置
      axis.ticks.x = element_blank(), 
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "grey50")
    )
  
  print(p_beautiful)
  
  ggsave("Beautiful_Boxplot_With_Background.png", p_beautiful, width = 8, height = 6, dpi = 300)
  
} else {
  stop("Absence of Valid Observations！")
}

# 4. Statistical Summary
summary_stats <- df_analysis %>%
  group_by(Zone) %>%
  summarise(
    Mean = mean(Population_protection_yield_per__treated_areaValue),
    Median = median(Population_protection_yield_per_treated_area_Value),
    SD = sd(Population_protection_yield_per_treated_area_Value),
    N = n()
  )
print(summary_stats)