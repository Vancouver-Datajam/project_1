# version: 20200912
# datajam project 1

# setup ---------------

library(tidyverse)
library(arrow)
library(DataExplorer)
library(leaflet)


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

# create quick report to visualize data
create_report(dat, output_dir = "viz")



# same coordinate --------------------

check <-
  dat %>% 
  filter(Year == 2018) %>%
  group_by(X, Y) %>%
  tally()


# map -------------------------


leaflet() %>%
  addTiles() %>%
  addMarkers(data = dat, 
             lng = ~X, 
             lat = ~Y)
  
  
  
  
  
  
  
