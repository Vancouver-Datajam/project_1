#Data Exploration
library(tidyverse)
library(magrittr)
library(caret)
library(ggplot2)
library(arrow)
library(forecast)

dat <- read_parquet("raw_data/Combined_MLW_MDMAP_TIDES_Data__All_Points_2015-2018.parquet")

#Filter for Canadian data
dat %<>% 
  filter(str_detect(.$CountryName_FromSource, ".*Canada.*"),
         !Year %in% c(2015),
         !(Year %in% 2016 & MonthNum < 6))


#Task is to forecast the number of total waste over time
#First visualize the current number of waste over time
#Group by month year and reduce dataset
month_year_dat <- dat %>% 
  dplyr::select(Location,
         TotalVolunteers,
         Month,
         Year,
         Totalltems_EventRecord:FishingOtherPlasticDebris2) %>% 
  dplyr::select(-c(COUNTRY:CONTINENT)) %>% 
  group_by(Month,Year) %>% 
  mutate(Totalltems_EventRecord = as.numeric(sum(Totalltems_EventRecord)),) %>% 
  na.omit() %>% 
  unique()

#Split data with 80% in the training set

train_waste <- month_year_dat[1:2400,] %>% 
  na.omit()

test_waste <- month_year_dat[2401:3012,] %>% 
  na.omit()

#Select certain variables


#Train
#APPROACH 1
model.lm <- lm(Totalltems_EventRecord~`Location`+
                    `Month`+
                   `Year`+
                  `TotalVolunteers`,
                  data=train_waste,
                  na.action=na.fail)
summary(model.lm)
# plot(model.lm)

#Forecast
predict.linear <- predict(model.lm,test_waste)

summary(predict.lm)
