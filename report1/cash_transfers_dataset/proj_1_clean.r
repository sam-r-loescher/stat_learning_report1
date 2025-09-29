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
##### function ####### ####### ####### ####### ####### ####### ####### #######

dat = data
K = 5
target_edu = 8
target_happy = 4
my_new_knn <- function(
    K, target_edu, target_happy, dat
){
  tmp <- tibble(
    edu = c(target_edu, dat$edu_numeric),
    happy = c(target_happy, dat$happy_numeric)
  )
  dist_matrix <- daisy(tmp, metric = "gower") %>% as.matrix()
  dist_vector <- dist_matrix[,1, drop = T]
  
  dat %>%
    ungroup %>%
    mutate(
      diff = dist_vector[-1]
    ) %>%
    arrange(diff) %>%
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

grid <- expand.grid(
  edu = edu_vector,
  happy = happy_vector
)

## use a for loop
pred_matrix <- matrix(NA, nrow = nrow(grid), 1)
for(i in 1:nrow(grid)){
  pred_matrix[i,] <- my_new_knn(
    K = 5,
    target_edu = grid[i,1],
    target_happy = grid[i,2],
    dat = data
  )}



##  ################# visualizing ################# ################# ################# ################# #################

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

