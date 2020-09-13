library(readxl)
library(tidyverse)
library(caret)
library(rsample)
library(mlr)
library(ggplot2)
library(randomForest)
library(ggpubr)

setwd("C:/Users/Daniel Lee/project_1")

RawData <- read.csv("raw_data/_Combined_MLW,_MDMAP,_TIDES_Data__All_Points,_2015-2018.csv")

glimpse(RawData)
summary(RawData)

RawData$CountryName_FromSource <- as.factor(RawData$CountryName_FromSource)
SampleData <- RawData %>%
  filter(str_detect(CountryName_FromSource,"Canada"))
SampleData$CountryName_FromSource <- trimws(SampleData$CountryName_FromSource,which = "both")

names(SampleData)[1] <- "X"
SampleData <- SampleData %>%
  filter(Year != 2015) %>%
  filter(!(Year == 2016 & MonthNum < 7)) %>%
  select(c("X",
           "Y",
           "TotalVolunteers",
           "DateStandardized",
           "Month",
           "Year",
           "Totalltems_EventRecord",
           "TotalClassifiedItems_EC2020",
           "PCT_PlasticAndFoam",
           "PCT_Glass_Rubber_Lumber_Metal",
           "SUM_Hard_PlasticBeverageBottle",
           "SUM_Hard_OtherPlasticBottle",
           "SUM_HardOrSoft_PlasticBottleCap",
           "SUM_PlasticOrFoamFoodContainer",
           "SUM_PlasticOrFoamPlatesBowlsCup",
           "SUM_OtherHardPlastic",
           "SUM_HardSoft_PersonalCareProduc",
           "SUM_Soft_Bag",
           "SUM_Soft_WrapperOrLabel",
           "SUM_Soft_Straw",,
           "SUM_Soft_CigaretteButts",
           "SUM_Soft_StringRingRibbon",
           "SUM_FishingLineLureRope",
           "SUM_Foam_OtherPlasticDebris",
           "SUM_OtherPlasticDebris",,
           "DOW",
           "NAME",
           "Shape__Area",
           "Shape__Length"
  )) %>%
  filter(NAME != "")%>%
  filter(!is.na(TotalVolunteers))

SampleData$DOW <- as.factor(SampleData$DOW)
SampleData$Month <- as.factor(SampleData$Month)
SampleData$Year <- as.factor(SampleData$Year)
#SampleData <- SampleData %>%
  #mutate(SUM_Soft = SUM_Soft_Bag + SUM_Soft_CigaretteButts + SUM_Soft_OtherPlastic + SUM_Soft_Straw + SUM_Soft_StringRingRibbon + SUM_Soft_WrapperOrLabel)

set.seed(1)
sample_split <- initial_split(SampleData, prop = 0.75)
Sample_train <- training(sample_split)
Sample_test <- testing(sample_split)

RF <- randomForest(TotalVolunteers ~.,data = Sample_train,ntree = 500)

#ggplot(SampleData, aes(x = SUM_Soft_Bag, y = TotalVolunteers)) +
  #geom_point()

RFmodel <- randomForest(TotalVolunteers ~ ., data = Sample_train, ntree = 500, mtry = 9, nodesize = 5)

#Tune RF - ntrees: 500 or 1000, although we see tiny decrease of OOB error at 1000
oob.error.rate <- data.frame(RFmodel$mse)
oob.error.rate$Trees <- c(1:1000)
oob.error.plot <- oob.error.rate %>%
  ggplot(aes(x = Trees, y = RFmodel.mse)) +
  geom_line()
oob.error.plot
oob.values <- vector(length=10)

#Predict 
Prediction <- as.data.frame(predict(RFmodel,Sample_test))

Sample_test <- Sample_test %>%
PredictionData <- Sample_test %>%
  select(c("TotalVolunteers","DateStandardized"))

PredictionData <- cbind(PredictionData,Prediction)
names(PredictionData)[3] <- "PredictedVolunteers"

FinalPlot <- ggplot(PredictionData,aes(x = DateStandardized, y = TotalVolunteers)) +
  geom_point(size = 0.2)
FinalPlot

FinalPlot2 <- ggplot(PredictionData,aes(x = DateStandardized, y = PredictedVolunteers)) +
  geom_point(size = 0.2)

FinalPlot2

Plots <- ggarrange(FinalPlot,FinalPlot2)

Plots
