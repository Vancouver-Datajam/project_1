#Data Exploration
library(tidyverse)
library(magrittr)
library(caret)
library(forecast)

library(arrow)
dat <- read_parquet("raw_data/Combined_MLW_MDMAP_TIDES_Data__All_Points_2015-2018.parquet")

#Filter for Canadian data
dat %<>% 
  filter(str_detect(.$CountryName_FromSource, ".*Canada.*"),
         !Year %in% c(2015),
         !(Year %in% 2016 & MonthNum < 6))

#Task is to forecast the number of plastics over time
#First visualize the current number of plastics over time

#Select only columns that count the number of plastics
plastic_dat <- dat %>% 
  dplyr::select(Location,
                TotalVolunteers,
                Month,
                Year,
                Totalltems_EventRecord:FishingOtherPlasticDebris2) %>% 
  dplyr::select(-c(COUNTRY:CONTINENT)) %>% 
  mutate(totalplastic = PCT_PlasticAndFoam*Totalltems_EventRecord) %>% 
  group_by(Month,Year) %>% 
  mutate(totalplastic = sum(totalplastic,na.omit)) %>% 
  na.omit() %>% 
  unique()


#Split data with 80% in the training set

train_plastic <- plastic_dat[1:2400,] %>% 
  na.omit

test_plastic <- plastic_dat[2401:3012,] %>% 
  na.omit

#Train
#APPROACH 1
model.lm <- lm(totalplastic~`Location`+
                 `Month`+
                 `Year`+
                 `TotalVolunteers`,
               data=train_plastic,
               na.action=na.fail)
summary(model.lm)
plot(model.lm)

#Forecast
forecast(model.lm,test_plastic)

