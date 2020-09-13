#Data Exploration
library(tidyverse)
library(magrittr)
library(caret)
library(ggplot2)
library(arrow)
library(forecast)
library(MASS)
library(lubridate)

dat <- read_csv("raw_data/_Combined_MLW,_MDMAP,_TIDES_Data__All_Points,_2015-2018.csv")

#Filter for Canadian data
dat %<>% 
  filter(str_detect(.$CountryName_FromSource, ".*Canada.*"),
         !Year %in% c(2015),
         !(Year %in% 2016 & MonthNum < 6))

#Change Location to Factor
dat$Location = as.factor(dat$Location)

dat$DateStandardized <- lubridate::ymd_hms(dat$DateStandardized)

#Task is to forecast the number of total waste over time
#First visualize the current number of waste over time
#Group by month year and reduce dataset
month_year_dat <- dat %>% 
  dplyr::select(Location,
         DateStandardized,
         TotalVolunteers,
         Month,
         Year,
         Totalltems_EventRecord:FishingOtherPlasticDebris2) %>% 
  dplyr::select(-c(COUNTRY:CONTINENT)) %>% 
  group_by(Month,Year) %>% 
  mutate(Totalltems_EventRecord = as.numeric(sum(Totalltems_EventRecord))) %>%
  dplyr::select(-Count_,
                -FishingOtherPlasticDebris2,
                -FishingGlowSticks2,
                -Soft_Sheets2,
                -SUM_HardSoftLollipopStick_EarBu,
                -SUM_HardOrSoft_PlasticBottleCap) %>% 
  na.omit() %>% 
  unique()



#Train
# #APPROACH 1
# model.lm <- lm(Totalltems_EventRecord~`Location`+
#                     `Month`+
#                    `Year`+
#                   `TotalVolunteers`,
#                   data=month_year_dat,
#                   na.action=na.fail)
# summary(model.lm)
# # plot(model.lm)
# 
# #Approach 2
# all_vars_model_waste <- lm(Totalltems_EventRecord~.,
#                      data=month_year_dat,
#                      na.action = na.fail)
# #Use stepwise multiple regression to determine the best model
# step_model_eval_waste <- stepAIC(all_vars_model_waste, direction= "both")
# step_model_eval_waste$anova
# #summarize the model determined by the stepAIC function to be the best fit
# summary(step_model_eval_waste)
# print(step_model_eval_waste$call)

#Approach 3
library(DataExplorer)
#Discrete correlation
cor_discrete <- plot_correlation(na.omit(month_year_dat),type="d")
print(cor_discrete)
#Continuos correlation
cor_cts <- plot_correlation(na.omit(month_year_dat),type="c")
print(cor_cts)

model_3_waste_data <- month_year_dat %>% 
  dplyr::select(DateStandardized,
    Totalltems_EventRecord
                ,TotalVolunteers,
                LAND_TYPE,
                LAND_RANK,
                Month,
                Year,
                Location)

model_3_waste_data <- unique(model_3_waste_data)

model_3_waste <- lm(Totalltems_EventRecord~.,
              data=model_3_waste_data, na.action = na.fail)

summary(model_3_waste)
varImp(model_3_waste)


#Use model 3 and plot the fitted values
waste_df <- data.frame(Date_Var = model_3_waste_data$DateStandardized,
                      Fitted=model_3_waste$fitted.values,
                      Actual=model_3_waste_data$Totalltems_EventRecord,
                      Location=model_3_waste_data$Location)


#Plot
plot_waste <- waste_df %>% ggplot(aes(Date_Var,Fitted)) + geom_line() +
  ggtitle("Estimated Waste")

# ggplotly(plot_waste) didn't work
