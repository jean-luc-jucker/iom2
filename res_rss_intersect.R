getwd()
library(readxl)
library(tidyverse)


# RES 
res <- readRDS('data_clean/res_slim.rds')%>% 
  mutate(across(where(is.character), as.factor))
dim(res) # 1,917 x 23

# RSS (+ Mimosa)
rss  <- readRDS('data_clean/rss_slim_recoded.rds')%>% 
  mutate(across(where(is.character), as.factor))
dim(rss) # 1,385 x 21

# Training (RES + Mimosa)
training <- readRDS('data_clean/res_training_slim.rds')%>% 
  mutate(across(where(is.character), as.factor))
dim(training) # 1,852 x 28

# IDs
res$MimosaID
rss$ID
training$ID_1


# Intersect RES-Training
length(intersect(res$MimosaID, training$ID_1)) # 1,688

# Intersect RES-RSS
length(intersect(res$MimosaID, rss$ID)) # 78

# Intersect RSS-Training
length(intersect(rss$ID, training$ID_1)) # 75

# Common to all
length(intersect(c(res$MimosaID, training$ID_1), rss$ID)) # 78




# NA
sum(is.na(res$MimosaID)) # 128
sum(is.na(rss$ID)) # 129
sum(is.na(training$ID_1)) # 132

# Drop NA
res <- res %>% drop_na(MimosaID) # 1,789 remaining
rss <- rss %>% drop_na(ID) # 1,256 remaining
training <- training %>% drop_na(ID_1) # 1,720 remaining

# Reassess after dropping NA

# Intersect RES-Training
length(intersect(res$MimosaID, training$ID_1)) # 1,687

# Intersect RES-RSS
length(intersect(res$MimosaID, rss$ID)) # 77

# Intersect RSS-Training
length(intersect(rss$ID, training$ID_1)) # 74

# Common to all
length(intersect(c(res$MimosaID, training$ID_1), rss$ID)) # 77

