# CreditGame_TRAIN data
# drop NAs
# subset 10,000 sampels

library("dplyr")

data <- read.csv("CreditGame_TRAIN.csv")

data <- data %>%
  filter(!is.na(AGE_D),
         ST_EMPL!='') 

set.seed(2021)
data <- data[sample(1:nrow(data), 10000),]

write.csv(data,'CC_data.csv' ,row.names = F)
