install.packages("devtools")
library(devtools)
devtools::install_github("cjcarlson/embarcadero")
library(embarcadero)
library(raster)
library(readxl)
setwd("D://chikglobal_chinaSDM//chik//vari//current")
set.seed(123)

#Section 1: Loading Variable Data
#A total of 16 variables were grouped into 4 categories: 
#bioclimatic (7), socioeconomic (2), geographical variables (3), and species variables(4).
env <- raster::stack("wc2.1_bio_2.tif",
                     "wc2.1_bio_8.tif",
                     "wc2.1_bio_9.tif",
                     "wc2.1_bio_13.tif",
                     "wc2.1_bio_14.tif",
                     "wc2.1_bio_18.tif",
                     "wc2.1_bio_19.tif",
                     "Cd_2020.tif",
                     "2019GDP.tif",
                     "NDVI_2025.tif",
                     "pd_2020.tif",
                     "PH_1985.tif",
                     "pop_count_2022.tif",
                     "rp_2020.tif",
                     "1__Aedes_aegypti_1_1_present_Distribution_Prediction.tif",
                     "1_Aedes_albopictus_1_1_present_Distribution_Prediction.tif")

names(env) <- c( "Bio2","Bio8", "Bio9", "Bio13","Bio14","Bio18", "Bio19", 
                 "Cd","GDP", 
                 "NDVI","pd","PH","pop","rp",
                 "Aedes_aegypti","Aedes_albopictus")

#Section 2: Loading  CHIKV occurrence Data
setwd("D://chikglobal_chinaSDM//chik")
species_records <- read_csv("chik_occ.csv")
species_records <- na.omit(species_records)
species_records <-  species_records[, c(1,2)]
species_records_df <- as.data.frame(species_records)
species_records_df <- na.omit(species_records_df)
library(writexl)

# Export the data frame.
write_xlsx(
  x =species_records_df, 
  path = "D://chikglobal_chinaSDM//chik//species_records_clean.xlsx"
)
species_records <- read_excel("species_records_clean1.xlsx")#Handling data of points not covered by the raster via GIS
library(stringr)
species_records[, 1] <- as.numeric(as.character(unlist(species_records[, 1])))
species_records[, 2] <- as.numeric(as.character(unlist(species_records[, 2])))

#Section 2.1: Coordinate point purification
mod <- SpatialPointsDataFrame(coords = species_records[,1:2],data = species_records)
names(mod@data) <- 'Presence'

#Transform the results back into coordinate points.
tmp <- rasterize(x = mod,y = env[[1]],,field = "Presence",fun = "min")
pts.sp1 <- rasterToPoints(tmp,
                          fun = function(x){
                            x>0
                          })
nrow(pts.sp1) #Remained 837 points

#Section 2.2: Extract environmental data for each point
pres_env <- raster::extract(x = env,y = pts.sp1[,1:2])
pres_env <- na.omit(pres_env) #Remained 723 points

#Section 3: Selection of environmental variables
library(usdm)

#Section 3.1: Removing multicollinearity via usdm::vifstep
vif_result <- vifstep(pres_env, th = 2)
vif_result@results

#Section 3.2: Correlation coefficients of the filtered variables were displayed using a correlation matrix plot.
selected_vars <- c("Bio8","Bio9","Bio13","Bio18","Bio19",
                   "POP","GDP",
                   "PH","Rp","NDVI",
                   "Pd","Cd",
                   "Aedes_aegypti", "Aedes_albopictus")
selected_env <- pres_env[, selected_vars]
cor_matrix <- cor(selected_env, method = "spearman")
library(corrplot)
corrplot(cor_matrix, method = "color", 
         type = "upper",   
         tl.cex = 0.8,    
         number.cex = 0.6, 
         col = colorRampPalette(c("#ABCD72B2", "white", "#F7A6AC"))(200),  
         tl.col = "black", 
         addCoef.col = "black", 
         diag = FALSE)    

#Eliminate highly collinear variables(Spearman correlation coefficient<0.7)
selected_vars <- c("Bio8","Bio9","Bio18","GDP","pop",
                   "NDVI",
                   "Cd",
                   "Aedes_aegypti", "Aedes_albopictus")
selected_env <- pres_env[, selected_vars]
cor_matrix <- cor(selected_env, method = "spearman")
library(corrplot)
corrplot(cor_matrix, method = "color", 
         type = "upper",   
         tl.cex = 0.8,     
         number.cex = 0.6,
         col = colorRampPalette(c("#ABCD72B2", "white", "#F7A6AC"))(200), 
         tl.col = "black", 
         addCoef.col = "black", 
         diag = FALSE)  

#Section 4: Generated BART model
#Section 4.1: Generated pseudo-absence points at a 1:5 ratio with presence points
absence_points <- randomPoints(env, n =5*nrow(pres_env))  
absence_env <- extract(env, absence_points) 

presence_data <- data.frame(pres_env) 
presence_data$presence <- 1  

absence_data <- data.frame(absence_env)  
absence_data$presence <- 0  

all_data <- rbind(presence_data, absence_data)

#Section 4.2: Partition into training and test sets
install.packages(c("BART", "caret", "pROC", "e1071","ggplot2"))
library(BART)       
library(caret)      
library(pROC)      
library(e1071)     
library(ggplot2)   
set.seed(123)  
trainIndex <- createDataPartition(all_data$presence, p = 0.7, list = FALSE)
train_data <- all_data[trainIndex, ]
test_data <- all_data[-trainIndex, ]
x_train <- train_data[, -17]  
y_train <- train_data$presence  
x_test <- test_data[, -17]
y_test <- test_data$presence

#Section 4.2.1: Generated BART model
set.seed(456)
bart_model1 <- bart(
  x.train = x_train, 
  y.train = as.numeric(y_train) - 1,  
  x.test = x_test,
  ntree = 50,          
  nskip = 100
) 
set.seed(456)

#Section 4.2.2: Identify the optimal classification threshold
y_pred_logit <- bart_model1$yhat.test  
y_pred_logit_mean <- apply(y_pred_logit, 2, mean)  
y_pred_prob_from_mean <- plogis(y_pred_logit_mean)  
valid_idx <- complete.cases(y_test) & complete.cases(y_pred_prob_from_mean)

y_test_clean <- y_test[valid_idx]
y_pred_prob_clean <- y_pred_prob_from_mean[valid_idx]

cat("The number of samples in the cleaned y_test：", length(y_test_clean), "\n")
cat("The number of samples in the cleaned predicted_prob：", length(y_pred_prob_clean), "\n")

calculate_tss <- function(threshold, actual, predicted_prob) {
  pred_class <- as.numeric(predicted_prob >= threshold)  
  cm <- confusionMatrix(
    data = factor(pred_class, levels = c(0, 1)),
    reference = factor(actual, levels = c(0, 1)),
    positive = "1"
  )
  sensitivity <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  return(sensitivity + specificity - 1) 
}

thresholds <- seq(0.1, 0.9, by = 0.01)  
tss_values <- sapply(thresholds, calculate_tss, 
                     actual = y_test_clean, 
                     predicted_prob = y_pred_prob_clean)

best_threshold <- thresholds[which.max(tss_values)]
cat(sprintf("optimal threshold: %.2f\n", best_threshold))

#Section 4.2.3: Classify probabilities into binary classes with a cut-off threshold of 0.41
y_pred_class <- factor(ifelse(y_pred_prob_clean > 0.41, 1, 0), levels = c(0, 1))
y_test <- factor(y_test, levels = c("0", "1"))
y_pred_class <- factor(y_pred_class, levels = c("0", "1"))
valid_idx <- complete.cases(y_test)  
y_test <- y_test[valid_idx]          
y_pred_class <- y_pred_class[valid_idx]  
y_pred_prob_clean <- y_pred_prob_clean[valid_idx] 

true_pos_count <- sum(as.numeric(y_test) - 1 == 1)  
true_neg_count <- sum(as.numeric(y_test) - 1 == 0) 
cat("The count of true positives (TP) in the testing set:", true_pos_count, "\n")
cat("The count of true positives (FP) in the testing set:", true_neg_count, "\n")
if (true_pos_count == 0) {
  warning("Due to the absence of true positive samples in the test set, AUPRC is not computable！")
  auprc_value <- NA 
} else {
  
  length(y_test)
  length(y_pred_prob_clean)
  length(y_pred_class)
  
  #Section 4.2.4: Confusion matrix
  conf_mat <- confusionMatrix(y_pred_class, y_test, positive = "1")
  print("Confusion matrix:")
  print(conf_mat$table)
  
  tp <- conf_mat$table[2, 2]  
  tn <- conf_mat$table[1, 1]  
  fp <- conf_mat$table[2, 1] 
  fn <- conf_mat$table[1, 2] 
  
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))  
  recall <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))     
  f1_score <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
  
  #Section 4.2.5: Calculate AUC and visualize the ROC curve
  roc_obj <- roc(as.numeric(y_test) - 1, y_pred_prob_clean) 
  
  if (!require("pROC")) install.packages("pROC")
  if (!require("pracma")) install.packages("pracma")
  library(pROC)
  library(pracma)
  
  pr_obj <- coords(roc_obj, x = "all", ret = c("recall", "precision"))
  pr_obj$precision[is.na(pr_obj$precision)] <- 0
  pr_df <- unique(data.frame(recall = pr_obj$recall, precision = pr_obj$precision)) 
  pr_df <- pr_df[order(pr_df$recall), ]  
  if (pr_df$recall[1] > 0) {
    pr_df <- rbind(data.frame(recall = 0, precision = 1), pr_df)  
  }
  if (pr_df$recall[nrow(pr_df)] < 1) {
    pr_df <- rbind(pr_df, data.frame(recall = 1, precision = true_pos_count / length(y_test)))  
  }
  
  auprc_value <- trapz(pr_df$recall, pr_df$precision)
}

#Section 4.2.6: Generate the evaluation metrics
cat("\n evaluation metrics:\n")
cat(sprintf("Accuracy: %.4f\n", accuracy))
cat(sprintf("Precision: %.4f\n", precision))
cat(sprintf("Recall: %.4f\n", recall))
cat(sprintf("F1-Score: %.4f\n", f1_score))
cat(sprintf("AUC: %.4f\n", auc_value))
cat(sprintf("AUPRC: %.4f\n", ifelse(is.na(auprc_value), 0, auprc_value)))  

#Section 4.2.7: Plot the ROC curve
roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities, 
  tpr = roc_obj$sensitivities      
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "ROC",
    x = "FPR",
    y = "TPR",
    subtitle = sprintf("AUC = %.4f", auc_value)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
