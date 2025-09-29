library(dplyr)
library(tidyverse)
library(ggplot2)
library(haven)
data_old <- read_dta("p1y2_endline_field_hh_PUBLIC.dta")

data%>%
mutate(
  parent_edu = a8_1_resp_edu + a8_2_hus_edu
)


data%>%
ggplot(aes(x = resp_edu, y = happy_likert))+
  geom_violin()


data %>%
  ggplot(aes(x = resp_edu, y = happy_likert)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6)


# counts of each (x, y) combination
data %>%
  ggplot(aes(x = resp_edu, y = happy_likert)) +
  geom_count()


data %>%
  ggplot(aes(x = resp_edu, y = happy_likert)) +
  geom_bin2d()


data %>%
  ggplot(aes(x = a8_2_hus_edu, y = a1_hh_size)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

data%>%
  mutate(
    both_edu = resp_edu + father_edu
  )


data %>%
  ggplot(aes(x = resp_edu, y = happy_likert)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


#h2_mkt_distance


#a8_1_resp_edu
#a8_2_hus_edu

## main ones are in rows 250-300

## maybe just like throw in happiness, depression, 





######## applying knn########



data <- data_old%>%
  mutate(happy_numeric = as.numeric(happy_likert)) %>%
  mutate(edu_numeric = as.numeric(resp_edu)) %>%
  dplyr::select(happy_numeric, edu_numeric) %>%
  filter(!is.na(happy_numeric))%>%
  filter(!is.na(edu_numeric))
  




# new knn function
library(cluster)
dat = data
K = 5
target_edu = 8
my_new_knn <- function(
    K, target_edu, dat
){
  # function to conduct KNN for happiness dat
  ## create temp dataframe to compute Gower distance
  tmp <- tibble(
    edu = c(target_edu, dat$edu_numeric)
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
                   pred = mean(happy_numeric, na.rm = TRUE)) %>%
    dplyr::pull(pred)
  #dat %>%
    #ungroup %>%
    #mutate(
     # diff = dist_vector[-1]
   # ) %>%
    #arrange(diff) %>%
    #slice(1:K) %>%
   # summarise(pred = mean(happy_likert, na.rm = T))
    #slice(1) %>% # tie-breaker (choose the first one alphabetically)
    #pull(happy_likert)
}



# loop through a grid and make predictions
edu_vector <- seq(
  min(as.numeric(data$edu_numeric), na.rm = T),
  max(as.numeric(data$edu_numeric), na.rm = T), 
  by = 1
)
edu_vector


grid <- expand.grid(
  edu_numeric = edu_vector
)


## use a for loop
pred_matrix <- matrix(NA, nrow = nrow(grid), 1)
for(i in 1:nrow(grid)){
  pred_matrix[i,] <- my_new_knn(
    K = 5,
    target_edu = grid[i,1], 
    dat = data
  )
  
  # print progress
  print(i)
}

p1 <- grid %>%
  mutate(pred = c(pred_matrix)) %>%
  ggplot() + 
  geom_tile(aes(x = edu_numeric, fill = pred), col = "black") +
  theme_bw()
p1

 
p2 <- grid %>%
  mutate(pred = c(pred_matrix)) %>%
  ggplot(aes(x = edu_numeric, y =  pred)) + 
  geom_point(col = "black") +
  geom_smooth()+
  theme_bw()
  
p2



