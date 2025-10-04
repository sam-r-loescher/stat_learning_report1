#$$
  
  #\hat{y}(\mathbf{x}) = \arg\max_{c \in \mathcal{C}} 
  #   \sum_{i=1}^{n} \mathbf{1}\!\left(y_i = c\right) \cdot \mathbf{1}\!\left(i \in #N_k(\mathbf{x})\right)
  
# $$

library(dplyr)
library(tidyverse)
library(ggplot2)
library(haven)
library(cluster)
library(viridis)

##### filtering ####### ####### ####### ####### ####### ####### ####### #######


data_old <- read_dta("p1y2_endline_field_hh_PUBLIC.dta")
data_2 <- data_old%>%
  mutate(happy_numeric = as.numeric(happy_likert)) %>%
  mutate(edu_numeric = as.numeric(resp_edu)) %>%
  dplyr::select(uid, happy_numeric, edu_numeric) %>%
  filter(!is.na(happy_numeric))%>%
  filter(!is.na(edu_numeric))


p1y1_end_anth_old <- read_dta("p1y1_endline_anthropometrics.dta")
p1y1_end_anth <- p1y1_end_anth_old%>%mutate(whz_numeric = as.numeric(whz)) %>%
  dplyr::select(uid, whz_numeric) %>%
  filter(!is.na(whz_numeric))%>%
  distinct(uid, .keep_all = TRUE)


  data <- left_join(data_2, p1y1_end_anth, by = "uid")
  #####new filtering/joining ########
  
  data_old <- haven::read_dta("p1y2_endline_field_hh_PUBLIC.dta")
  data_2 <- data_old %>%
    mutate(
      happy_numeric = as.numeric(happy_likert),
      edu_numeric = as.numeric(resp_edu),
      ration_status = as.numeric(rationcard)
    ) %>%
    dplyr::select(uid, happy_numeric, edu_numeric, ration_status) %>%
    filter(!is.na(happy_numeric), !is.na(edu_numeric), !is.na(ration_status))
  
  p1y1_end_anth_old <- haven::read_dta("p1y1_endline_anthropometrics.dta")
  p1y1_end_anth <- p1y1_end_anth_old %>%
    mutate(whz_numeric = as.numeric(whz)) %>%
    dplyr::select(uid, whz_numeric) %>%
    filter(!is.na(whz_numeric)) %>%
    distinct(uid, .keep_all = TRUE)
  
  data <- data_2 %>%
    inner_join(p1y1_end_anth, by = "uid") %>%
    mutate(
      ration_status = factor(ration_status,
                             levels = c(1, 2, 3),
                             labels = c("APL", "BPL", "AAY"))
    ) %>%
    filter(!is.na(ration_status))
  
  
  
##### function ####### ####### ####### ####### ####### ####### ####### #######

dat = data
K = 5
target_edu = 8
target_happy = 4
target_ration = "APL"
my_new_knn <- function(
    K, target_edu, target_happy, target_ration, weights = c(1,1,1), dat
){
  tmp <- tibble(
    edu = c(target_edu, dat$edu_numeric),
    happy = c(target_happy, dat$happy_numeric),
    ration = factor(c(as.character(target_ration), as.character(dat$ration_status)))
  )
 # dist_matrix <- daisy(tmp, metric = "gower") %>% as.matrix()
  #dist_vector <- dist_matrix[,1, drop = T]
  
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
    ungroup %>%
    mutate(
      dist = gower_dist
    ) %>%
    arrange(dist) %>%
    slice(1:K) %>%
    dplyr::summarise(
      pred = mean(whz_numeric, na.rm = TRUE)) %>%
    dplyr::pull(pred)
}


# loop through a grid and make predictions
edu_vector <- seq(
  min(as.numeric(data$edu_numeric), na.rm = T),
  max(as.numeric(data$edu_numeric), na.rm = T), 
  by = 1
)
edu_vector


happy_vector <- seq(
  min(as.numeric(data$happy_numeric), na.rm = T),
  max(as.numeric(data$happy_numeric), na.rm = T), 
  by = 1
)
happy_vector

ration_vector <- levels(data$ration_status)


grid <- expand.grid(
  edu = edu_vector,
  happy = happy_vector,
  ration = ration_vector,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

## use a for loop
pred_matrix <- matrix(NA, nrow = nrow(grid), 1)
for(i in 1:nrow(grid)){
  pred_matrix[i,] <- my_new_knn(
    K = 5,
    target_edu = grid[i,1],
    target_happy = grid[i,2],
    target_ration = grid[i,3],
    dat = data
  )}
################### testing ###############################################################################################
#set.seed(09302025)
data_subset <- data %>% sample_n(400)
training_ndx <- sample(1:nrow(data_subset), size = round(.7*nrow(data_subset))) %>% sort()
testing_ndx <- c(1:nrow(data_subset))[-training_ndx]
training_df <- data_subset %>% ungroup %>% slice(training_ndx)
testing_df <- data_subset %>% ungroup %>% slice(testing_ndx)
testing_ndx

# fit our KNN algorithm for multiple values of K
# predict the values of our testing data set for each value of K
# see which one does best
grid <- testing_df %>%
  ungroup %>%
  dplyr::select(edu = edu_numeric, happy = happy_numeric, ration = ration_status)

# K = K_values[k]
K_values <- c(3,5,10,20,40,60,80,100)
#dat = training_df
pred_matrix <- matrix(NA, nrow(grid), ncol = length(K_values))
for(k in 1:length(K_values)){
  message(paste0("Starting loop, k = ", K_values[k]))
  data_subset <- data %>% sample_n(400)
  pb <- txtProgressBar(
    min = 0, max = nrow(grid), initial = 0, char = "=",
    width = NA, title, label, style = 1, file = ""
  )
  
  for(i in 1:nrow(grid)){
    pred_matrix[i,k] <- my_new_knn(
      K = K_values[k], 
      as.numeric(grid[i,1]),
      as.numeric(grid[i,2]),
      as.numeric(grid[i,3]),
      weights = c(1,1,1),
      training_df
    )
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

pred_tbl <- grid %>%
  mutate(
    truth = testing_df$whz_numeric,
    `3` = pred_matrix[,1],
    `5` = pred_matrix[,2],
    `10` = pred_matrix[,3],
    `20` = pred_matrix[,4],
    `40` = pred_matrix[,5],
    `60` = pred_matrix[,6],
    `80` = pred_matrix[,7],
    `100` = pred_matrix[,8]
    
  ) %>%
  pivot_longer(`3`:`100`, names_to = "K", values_to = "pred")

# compute our loss function
## attempt 2
pred <- pred_tbl %>%
  mutate(SE = (truth - pred)^2) %>%
  group_by(K)%>%
  summarise(mse = mean(SE))%>%
  arrange(mse)
  

##  ################# visualizing ################# ################# ################# ################# #################
data_subset %>%
  ggplot() + 
  geom_point(aes(y = happy_numeric, x = edu_numeric, col = whz_numeric)) +
  theme_bw() +
  viridis::scale_color_viridis(option = "magma")


p1 <- grid %>%
  mutate(pred = c(pred_matrix)) %>%
  ggplot() + 
  geom_raster(aes(x = edu, y = happy, fill = pred)) +
  scale_fill_viridis_c()
p1

p2 <- grid %>%
  mutate(pred = c(pred_matrix)) %>%
  ggplot(aes(x = edu_numeric, y =  pred)) + 
  geom_point(col = "black") +
  geom_smooth()+
  theme_bw()
p2


# Aggregate CV results (MSE and its standard error) and prep plotting vars
cv_results <- pred_tbl %>%
  mutate(SE_i = (truth - pred)^2) %>%                 # per-row squared error
  group_by(K) %>%
  summarise(
    MSE = mean(SE_i, na.rm = TRUE),
    SE  = sd(SE_i,  na.rm = TRUE) / sqrt(n())         # standard error of the mean SE
  ) %>%
  ungroup() %>%
  mutate(K = as.integer(K)) %>%                       # make K numeric for a continuous x-axis
  arrange(K)

optimal_k <- cv_results$K[which.min(cv_results$MSE)]
k_values  <- cv_results$K

# PLOT RESULTS
p_cv <- ggplot(cv_results, aes(x = K, y = MSE)) +
  geom_line(color = "#440154FF", size = 1) +
  geom_point(size = 3, color = "#440154FF") +
  geom_errorbar(aes(ymin = MSE - SE, ymax = MSE + SE),
                width = 2, color = "#440154FF", alpha = 0.5) +
  geom_vline(xintercept = optimal_k, linetype = "dashed",
             color = "red", size = 1) +
  annotate("text",
           x = optimal_k,
           y = max(cv_results$MSE, na.rm = TRUE),
           label = paste("Optimal K =", optimal_k),
           hjust = -0.1, vjust = 1.1,
           color = "red", fontface = "bold") +
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
p_cv

