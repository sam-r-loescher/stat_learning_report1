library(dplyr)
library(tidyverse)
library(ggplot2)
library(haven)
library(cluster)
library(viridis)

setwd("C:/Users/Owner/STAT_LEARNING_REPORT1/report1/cash_transfers_dataset")

# ========================================
# LOAD AND PREPARE DATA
# ========================================

data_old <- haven::read_dta("p1y2_endline_field_hh_PUBLIC.dta")
data_2 <- data_old %>%
  mutate(
    happy_numeric = as.numeric(happy_likert),
    edu_numeric = as.numeric(resp_edu),
    ration_status = as.numeric(rationcard)
  ) %>%
  select(uid, happy_numeric, edu_numeric, ration_status) %>%
  filter(!is.na(happy_numeric), !is.na(edu_numeric), !is.na(ration_status))

p1y1_end_anth_old <- haven::read_dta("p1y1_endline_anthropometrics.dta")
p1y1_end_anth <- p1y1_end_anth_old %>%
  mutate(whz_numeric = as.numeric(whz)) %>%
  select(uid, whz_numeric) %>%
  filter(!is.na(whz_numeric)) %>%
  distinct(uid, .keep_all = TRUE)

data_final <- data_2 %>%
  inner_join(p1y1_end_anth, by = "uid") %>%
  mutate(
    ration_status = factor(ration_status,
                          levels = c(1, 2, 3),
                          labels = c("APL", "BPL", "AAY"))
  ) %>%
  filter(!is.na(ration_status))

cat("Final sample size:", nrow(data_final), "\n")
cat("Ration card status distribution:\n")
print(table(data_final$ration_status))
cat("\nWHZ summary by ration status:\n")
print(data_final %>% group_by(ration_status) %>% 
      summarise(mean_whz = mean(whz_numeric), 
                sd_whz = sd(whz_numeric),
                n = n()))

<<<<<<< HEAD
dat = data
K = 5
target_edu = 8
target_happy = 4
my_new_knn <- function(
    K, target_edu, target_happy, weights = c(1,1), dat
){
=======
# ========================================
# K-NN FUNCTION
# ========================================

my_knn <- function(K, target_edu, target_happy, target_ration, dat){
>>>>>>> 51c504aea8a14d9cc4879058da8368d38e6e0e84
  tmp <- tibble(
    edu = c(target_edu, dat$edu_numeric),
    happy = c(target_happy, dat$happy_numeric),
    ration = factor(c(as.character(target_ration), as.character(dat$ration_status)))
  )
<<<<<<< HEAD
 # dist_matrix <- daisy(tmp, metric = "gower") %>% as.matrix()
  #dist_vector <- dist_matrix[,1, drop = T]
=======
  
  dist_matrix <- cluster::daisy(tmp, metric = "gower") %>% as.matrix()
  dist_vector <- dist_matrix[, 1][-1]
  
  K_adjusted <- min(K, sum(!is.na(dist_vector)))
>>>>>>> 51c504aea8a14d9cc4879058da8368d38e6e0e84
  
  #dat %>%
   # ungroup %>%
    #mutate(
     # diff = dist_vector[-1]
    #) %>%
    #arrange(diff) %>%
    #slice(1:K) %>%
    #dplyr::summarise(
    #  pred = mean(whz_numeric, na.rm = TRUE)) %>%
    #dplyr::pull(pred)
  
  
  gower_dist <- as.matrix(cluster::daisy(
    tmp, metric = "gower", weights = weights
  ))[1,-1]
  
  # now the usual knn process
  dat %>%
<<<<<<< HEAD
    ungroup %>%
    mutate(
      dist = gower_dist
    ) %>%
    arrange(dist) %>%
    slice(1:K) %>%
    dplyr::summarise(
      pred = mean(whz_numeric, na.rm = TRUE)) %>%
    dplyr::pull(pred)
=======
    mutate(diff = dist_vector) %>%
    arrange(diff) %>%
    slice(1:K_adjusted) %>%
    summarise(pred = mean(whz_numeric, na.rm = TRUE)) %>%
    pull(pred)
>>>>>>> 51c504aea8a14d9cc4879058da8368d38e6e0e84
}

# ========================================
# CREATE PREDICTION GRID
# ========================================

edu_vector <- seq(min(data_final$edu_numeric), max(data_final$edu_numeric), by = 1)
happy_vector <- seq(min(data_final$happy_numeric), max(data_final$happy_numeric), by = 1)
ration_vector <- levels(data_final$ration_status)

grid <- expand.grid(
  edu = edu_vector,
  happy = happy_vector,
  ration = ration_vector,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

<<<<<<< HEAD
## use a for loop
pred_matrix <- matrix(NA, nrow = nrow(grid), 1)
for(i in 1:nrow(grid)){
  pred_matrix[i,] <- my_new_knn(
    K = 5,
    target_edu = grid[i,1],
    target_happy = grid[i,2],
    dat = data
  )}
################### testing ###############################################################################################
#set.seed(09302025)
training_ndx <- sample(1:nrow(data), size = round(.7*nrow(data))) %>% sort()
testing_ndx <- c(1:nrow(data))[-training_ndx]
training_df <- data %>% ungroup %>% slice(training_ndx)
testing_df <- data %>% ungroup %>% slice(testing_ndx)
testing_ndx
=======
cat("\nRunning K-NN predictions (K=50) for", nrow(grid), "combinations...\n")

# ========================================
# RUN PREDICTIONS
# ========================================

pred_vec <- numeric(nrow(grid))
for(i in seq_len(nrow(grid))){
  if(i %% 50 == 0) cat("Processed", i, "of", nrow(grid), "\n")
  pred_vec[i] <- my_knn(
    K = 50,
    target_edu = grid$edu[i],
    target_happy = grid$happy[i],
    target_ration = grid$ration[i],
    dat = data_final
  )
}

p1_df <- grid %>% mutate(pred = pred_vec)

# ========================================
# GENERATE PLOTS
# ========================================

# Plot 1: Faceted heatmap
p1 <- ggplot(p1_df, aes(x = edu, y = happy, fill = pred)) +
  geom_raster() +
  scale_fill_viridis_c(option = "plasma", 
                       name = "Predicted\nWHZ Score",
                       na.value = "grey50") +
  facet_wrap(~ration, ncol = 3) +
  labs(
    title = "Predicted Child WHZ Score by Education, Happiness, and Poverty Level",
    subtitle = "K-NN predictions (K=50) across ration card categories",
    x = "Respondent Education Level",
    y = "Happiness Level (Likert Scale)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "right"
  )

print(p1)
ggsave("knn_heatmap_comparison.png", p1, width = 12, height = 4, dpi = 300)

# Plot 2: Education effect by ration card status
p2 <- p1_df %>%
  ggplot(aes(x = edu, y = pred, color = ration, fill = ration)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, alpha = 0.15, span = 0.75) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  theme_bw() +
  labs(
    title = "Predicted WHZ Score vs. Education Level by Poverty Category",
    subtitle = "Smoothed trends showing differences across ration card types",
    x = "Respondent Education Level",
    y = "Predicted Child WHZ Score",
    color = "Ration Card",
    fill = "Ration Card"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "right"
  )

print(p2)
ggsave("knn_education_trend.png", p2, width = 9, height = 6, dpi = 300)

# Plot 3: Happiness effect by ration card status
p3 <- p1_df %>%
  ggplot(aes(x = happy, y = pred, color = ration, fill = ration)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, alpha = 0.15, span = 0.75) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  theme_bw() +
  labs(
    title = "Predicted WHZ Score vs. Happiness Level by Poverty Category",
    subtitle = "Smoothed trends showing differences across ration card types",
    x = "Happiness Level (Likert Scale)",
    y = "Predicted Child WHZ Score",
    color = "Ration Card",
    fill = "Ration Card"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "right"
  )

print(p3)
ggsave("knn_happiness_trend.png", p3, width = 9, height = 6, dpi = 300)

# ========================================
# SUMMARY STATISTICS
# ========================================

cat("\n=== SUMMARY STATISTICS ===\n")
summary_stats <- p1_df %>%
  group_by(ration) %>%
  summarise(
    mean_pred_whz = mean(pred, na.rm = TRUE),
    sd_pred_whz = sd(pred, na.rm = TRUE),
    min_pred_whz = min(pred, na.rm = TRUE),
    max_pred_whz = max(pred, na.rm = TRUE),
    n_predictions = n()
  )
print(summary_stats)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("APL = Above Poverty Line | BPL = Below Poverty Line | AAY = Antyodaya (poorest)\n")
cat("Plots saved to:", getwd(), "\n")
>>>>>>> 51c504aea8a14d9cc4879058da8368d38e6e0e84

# fit our KNN algorithm for multiple values of K
# predict the values of our testing data set for each value of K
# see which one does best
grid <- testing_df %>%
  ungroup %>%
  dplyr::select(edu = edu_numeric, happy = happy_numeric)

K = K_values[k]
K_values <- 2:7
#dat = training_df
pred_matrix <- matrix(NA, nrow(grid), ncol = length(K_values))
for(k in 1:length(K_values)){
  for(i in 1:nrow(grid)){
    pred_matrix[i,k] <- my_new_knn(
      K = K_values[k], 
      as.numeric(grid[i,1]),
      as.numeric(grid[i,2]),
      weights = c(1,1),
      training_df
    )
  }
}

pred_tbl <- grid %>%
  mutate(
    truth = testing_df$whz_numeric,
    `2` = pred_matrix[,1],
    `3` = pred_matrix[,2],
    `4` = pred_matrix[,3],
    `5` = pred_matrix[,4],
    `6` = pred_matrix[,5],
    `7` = pred_matrix[,6]
  ) %>%
  pivot_longer(`2`:`7`, names_to = "K", values_to = "pred")

# visualize
pred_tbl %>%
  ggplot() + 
  geom_point(aes(x = edu, y = happy, col = pred)) +
  facet_wrap(~ K) +
  theme_bw() +
  theme(legend.position = "bottom")

# compute our loss function

## attempt 2
  pred_tbl %>%
    mutate(SE = (truth - pred)^2) %>%
    group_by(K)%>%
    summarise(mse = mean(SE))
  #, .groups = "drop"
  #arrange((mse))
  

mean("Nickelback" == testing_df$artists)





<<<<<<< HEAD



=======
# ========================================
# K-NN WITH CROSS-VALIDATION TO FIND OPTIMAL K
# ========================================

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(haven)
library(cluster)

setwd("C:/Users/Owner/STAT_LEARNING_REPORT1/report1/cash_transfers_dataset")

# LOAD DATA (same as before)
data_old <- haven::read_dta("p1y2_endline_field_hh_PUBLIC.dta")
data_2 <- data_old %>%
  mutate(
    happy_numeric = as.numeric(happy_likert),
    edu_numeric = as.numeric(resp_edu),
    ration_status = as.numeric(rationcard)
  ) %>%
  select(uid, happy_numeric, edu_numeric, ration_status) %>%
  filter(!is.na(happy_numeric), !is.na(edu_numeric), !is.na(ration_status))

p1y1_end_anth_old <- haven::read_dta("p1y1_endline_anthropometrics.dta")
p1y1_end_anth <- p1y1_end_anth_old %>%
  mutate(whz_numeric = as.numeric(whz)) %>%
  select(uid, whz_numeric) %>%
  filter(!is.na(whz_numeric)) %>%
  distinct(uid, .keep_all = TRUE)

data_final <- data_2 %>%
  inner_join(p1y1_end_anth, by = "uid") %>%
  mutate(
    ration_status = factor(ration_status,
                          levels = c(1, 2, 3),
                          labels = c("APL", "BPL", "AAY"))
  ) %>%
  filter(!is.na(ration_status))

cat("Dataset size:", nrow(data_final), "\n\n")

# K-NN FUNCTION FOR CV
knn_predict_one <- function(test_idx, K, dat){
  test_row <- dat[test_idx, ]
  train_data <- dat[-test_idx, ]
  
  tmp <- tibble(
    edu = c(test_row$edu_numeric, train_data$edu_numeric),
    happy = c(test_row$happy_numeric, train_data$happy_numeric),
    ration = factor(c(as.character(test_row$ration_status), 
                     as.character(train_data$ration_status)))
  )
  
  dist_matrix <- cluster::daisy(tmp, metric = "gower") %>% as.matrix()
  dist_vector <- dist_matrix[, 1][-1]
  
  K_adjusted <- min(K, length(dist_vector))
  
  train_data %>%
    mutate(diff = dist_vector) %>%
    arrange(diff) %>%
    slice(1:K_adjusted) %>%
    summarise(pred = mean(whz_numeric, na.rm = TRUE)) %>%
    pull(pred)
}

# CROSS-VALIDATION
set.seed(218)  # For reproducibility

# Test K values from 3 to 50
k_values <- c(3, 5, 10, 20, 30, 40, 50, 60, 80, 100)

# Use 5-fold cross-validation on a sample for speed
n_sample <- min(500, nrow(data_final))  # Use 500 observations for CV
sample_indices <- sample(1:nrow(data_final), n_sample)
cv_data <- data_final[sample_indices, ]

# Create 5 folds
cv_data$fold <- sample(rep(1:5, length.out = n_sample))

cat("Running 5-fold cross-validation...\n")
cat("Testing K values:", paste(k_values, collapse = ", "), "\n\n")

cv_results <- tibble()

for(k in k_values){
  cat("Testing K =", k, "...\n")
  
  fold_errors <- numeric(5)
  
  for(fold in 1:5){
    test_set <- cv_data %>% filter(fold == !!fold)
    train_set <- cv_data %>% filter(fold != !!fold)
    
    predictions <- numeric(nrow(test_set))
    
    for(i in 1:nrow(test_set)){
      test_row <- test_set[i, ]
      
      tmp <- tibble(
        edu = c(test_row$edu_numeric, train_set$edu_numeric),
        happy = c(test_row$happy_numeric, train_set$happy_numeric),
        ration = factor(c(as.character(test_row$ration_status), 
                         as.character(train_set$ration_status)))
      )
      
      dist_matrix <- cluster::daisy(tmp, metric = "gower") %>% as.matrix()
      dist_vector <- dist_matrix[, 1][-1]
      
      K_adjusted <- min(k, length(dist_vector))
      
      predictions[i] <- train_set %>%
        mutate(diff = dist_vector) %>%
        arrange(diff) %>%
        slice(1:K_adjusted) %>%
        summarise(pred = mean(whz_numeric, na.rm = TRUE)) %>%
        pull(pred)
    }
    
    # Calculate MSE for this fold
    fold_errors[fold] <- mean((test_set$whz_numeric - predictions)^2)
  }
  
  # Store average MSE across folds
  cv_results <- bind_rows(cv_results, 
                          tibble(K = k, 
                                MSE = mean(fold_errors),
                                SE = sd(fold_errors) / sqrt(5)))
}

# RESULTS
cat("\n=== CROSS-VALIDATION RESULTS ===\n")
print(cv_results %>% arrange(MSE))

optimal_k <- cv_results %>% arrange(MSE) %>% slice(1) %>% pull(K)
cat("\nOptimal K:", optimal_k, "\n")

# PLOT RESULTS
p_cv <- ggplot(cv_results, aes(x = K, y = MSE)) +
  geom_line(color = "#440154FF", size = 1) +
  geom_point(size = 3, color = "#440154FF") +
  geom_errorbar(aes(ymin = MSE - SE, ymax = MSE + SE), 
                width = 2, color = "#440154FF", alpha = 0.5) +
  geom_vline(xintercept = optimal_k, linetype = "dashed", 
             color = "red", size = 1) +
  annotate("text", x = optimal_k, y = max(cv_results$MSE), 
           label = paste("Optimal K =", optimal_k), 
           hjust = -0.1, color = "red", fontface = "bold") +
  scale_x_continuous(breaks = k_values) +
  theme_bw() +
  labs(
    title = "Cross-Validation: Finding Optimal K",
    subtitle = "5-fold CV on 500 randomly sampled observations",
    x = "K (Number of Nearest Neighbors)",
    y = "Mean Squared Error (MSE)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(p_cv)
ggsave("optimal_k_selection.png", p_cv, width = 10, height = 6, dpi = 300)

cat("\nPlot saved to:", getwd(), "\n")

# ========================================
>>>>>>> 51c504aea8a14d9cc4879058da8368d38e6e0e84
