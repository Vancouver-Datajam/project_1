# version: 20200912
# datajam project 1

# setup ---------------

library(tidyverse)
library(arrow)
library(DataExplorer)
library(leaflet)
library(lubridate)


#folder
folder_input <- "raw_data"
folder_viz <- "viz"


# data import -------------------------


# csv
dat <-
  read_csv(
    file.path(
      folder_input,
      "_Combined_MLW,_MDMAP,_TIDES_Data__All_Points,_2015-2018.csv"
    )
  )


# # parquet
# dat_p <-
#   read_parquet(
#     file.path(
#       folder_input,
#       "Combined_MLW_MDMAP_TIDES_Data__All_Points_2015-2018.parquet"
#     )
#   )


# just canadian data only
# PS: discard the rest of data
dat <- 
  dat %>%
  filter(CountryName_FromSource == "Canada")



# EDA ---------------------------

glimpse(dat)

# # create quick report to visualize data
# create_report(dat, output_dir = "viz")



# mode dataset --------------------

# just 2018 (don't work)
test_2018 <-
  dat %>% 
  filter(Year == 2018) %>%
  group_by(X, Y) %>%
  tally()


# by sub country
count_sub_country <- 
  dat %>%
  filter(Year == 2018) %>%
  group_by(SubCountry_L2_FromSource) %>%
  tally()


# map -----------------




--------

# Great vancouver
leaflet() %>%
  addTiles() %>%
  addMarkers(
    data =
      dat %>% filter(SubCountry_L2_FromSource == "Greater Vancouver" &
                       Year == 2018),
    lng = ~ X,
    lat = ~ Y,
    label = ~ paste0(ymd_hms(DateOriginal), ": ", Totalltems_EventRecord, " items"),
    clusterOptions = markerClusterOptions()
  )
  
  
  
  
  
  
  
