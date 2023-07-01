getwd()
library(tidyverse)
library(ggplot2)
library(GGally)

# Load res object as df, and convert strings to factors
df <- readRDS('data_clean/rss_slim.rds')%>% 
  mutate(across(where(is.character), as.factor))
dim(df) # 1,385 x 34

# Discard unplottable variables
#df <- df %>% select(-c(MimosaID, InterviewDate))
#dim(df)

# Subset to RSS vars
df <- df %>% select(
  EconomicScore, SocialScore, PsychoSocialScore, CompositeScore
)

cor(df)

theme_set(theme_light())
ggpairs(df)

ggpair