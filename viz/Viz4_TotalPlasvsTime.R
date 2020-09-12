library(tidyverse)
library(lubridate)

dat <- read.csv("_Combined_MLW,_MDMAP,_TIDES_Data__All_Points,_2015-2018.csv", stringsAsFactors = F)

newdat <- dat %>% 
  filter(ISO_CC == "CA") %>% 
  arrange(Year, MonthNum, Day)

# Every after June 2016
newdat <- newdat %>% 
  filter(DateOriginal>="2016/06/01 00:00:00") %>% 
  mutate(date = make_date(Year,MonthNum,Day)) %>% 
  select(CountryName_FromSource:SubCountry_L2_FromSource, Year, date, TotalClassifiedItems_EC2020) %>% 
  na.omit()
newdat$quarter <- quarter(newdat$date)
newdat <- newdat %>% unite(yrqtr, c(Year,quarter), remove=FALSE, sep="-")
#view(newdat)

## Idea: Barplot or line plot of total plastic over time (2016-2018 separate or together)
## ggplot to do this; plot quarterly year

myplot <- newdat %>% 
  group_by(yrqtr, TotalClassifiedItems_EC2020) %>% 
  ggplot(aes(x=yrqtr, y=TotalClassifiedItems_EC2020/1000)) + 
  geom_bar(fill="skyblue", stat ="identity") + 
  ggtitle("Total Number of Plastic Debris (per thousand) Recovered over Time") +
  xlab("Year (Quarterly)") + ylab("Number of Plastic Debris (per thousand)") +
  theme(plot.title = element_text(hjust = 0.5))
myplot + scale_y_continuous(breaks=seq(from=0,to=800,by=50))

