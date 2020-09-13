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
summary(dat)

dat$DateStandardized <- lubridate::ymd_hms(dat$DateStandardized)

#Select only columns that count the number of plastics
plastic_dat <- dat %>% 
  dplyr::select(Location,
                TotalVolunteers,
                DateStandardized,
                Month,
                Year,
                Totalltems_EventRecord:FishingOtherPlasticDebris2) %>% 
  dplyr::select(-c(COUNTRY:CONTINENT)) %>% 
  mutate(totalplastic = PCT_PlasticAndFoam*Totalltems_EventRecord) %>% 
  group_by(Month,Year) %>% 
  mutate(totalplastic = sum(totalplastic,na.rm=T)) %>% 
  dplyr::select(-Count_,
                -FishingOtherPlasticDebris2,
                -FishingGlowSticks2,
                -Soft_Sheets2,
                -SUM_HardSoftLollipopStick_EarBu,
                -SUM_HardOrSoft_PlasticBottleCap) %>% 
  na.omit() %>% 
  unique()

# #Train
# #APPROACH 1
# model_plastic.lm <- lm(totalplastic~.,
#                data=plastic_dat,
#                na.action=na.fail)
# 
# summary(model_plastic.lm)
# varImp(model_plastic.lm)
# # plot(model.lm)
# 
# #Approach 2
# all_vars_model_plastic <- lm(totalplastic~.,
#                      data=plastic_dat,
#                      na.action = na.fail)
# #Use stepwise multiple regression to determine the best model
# step_model_eval_plastic <- stepAIC(all_vars_model_plastic, direction= "both")
# step_model_eval_plastic$anova
# #summarize the model determined by the stepAIC function to be the best fit
# summary(step_model_eval_plastic)
# varImp(step_model_eval_plastic)
# print(step_model_eval_plastic$call)
# 
# 
# 
# 


#Approach 3
#Based on correlations and feature exploration
# calculate correlation matrix
library(DataExplorer)
#Discrete correlation
cor_discrete <- plot_correlation(na.omit(plastic_dat),type="d")
print(cor_discrete)
#Continuos correlation
cor_cts <- plot_correlation(na.omit(plastic_dat),type="c")
print(cor_cts)


model_3 <- lm(totalplastic~
                `TotalVolunteers` +
                LAND_TYPE +
                LAND_RANK +
                Month + 
                Year +
                Location+
                DateStandardized,
              data=plastic_dat, na.action = na.fail)

model_3_plastic_data <- plastic_dat %>% 
  dplyr::select(DateStandardized,
                totalplastic
                ,TotalVolunteers,
                LAND_TYPE,
                LAND_RANK,
                Month,
                Year,
                Location)

model_3_plastic_data <- unique(model_3_plastic_data)

model_3_plastic <- lm(totalplastic~.,
                    data=model_3_plastic_data, na.action = na.fail)

summary(model_3_plastic)
varImp(model_3_plastic)


#Use model 3 and plot the fitted values for 2018
plot_df <- data.frame(DateVar = model_3_plastic_data$DateStandardized,
                      Month=model_3_plastic_data$Month, 
                      Year=model_3_plastic_data$Year,
                      Fitted=model_3_plastic$fitted.values,
                      Actual=model_3_plastic_data$totalplastic)

#Plot
library(plotly)
plot_plastic <- plot_df %>% 
  ggplot(aes(DateVar,Fitted)) + 
  geom_line()

interacticve_plastic <- ggplotly(plot_plastic)

interacticve_plastic
#Linear Extrapolation
