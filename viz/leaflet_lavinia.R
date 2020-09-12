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

# group data by subcountry
by_subcountry <-
  dat %>%
  group_by(SubCountry_L2_FromSource, Year) %>%
  summarise(
    across(c(X, Y, PCT_PlasticAndFoam),
           mean),
    across(c(Totalltems_EventRecord, TotalClassifiedItems_EC2020),
           sum)
  ) %>%
  mutate(Is_AboveMean = Totalltems_EventRecord > 7231)



# color
pal <- colorFactor(c("blue", "red"), domain = c(FALSE, TRUE))



# regional map
leaflet() %>%
  addTiles() %>%
  addCircles(
    data = 
      by_subcountry %>%
      filter(Year == 2018),
    group = "2018",
    lng = ~ X,
    lat = ~ Y,
    radius = ~Totalltems_EventRecord / 50,
    color = ~ pal(Is_AboveMean),
    popup = ~ paste0(
      "<b><font size = 4>",
      SubCountry_L2_FromSource,
      "</b></font>",
      "<br>Total Items recorded: ",
      format(Totalltems_EventRecord, big.mark = ","),
      "<br>Plastic and foam %: ",
      paste0(round(PCT_PlasticAndFoam, 1), "%")
    )
  ) %>%
  addCircles(
    data = 
      by_subcountry %>%
      filter(Year == 2017),
    group = "2017",
    lng = ~ X,
    lat = ~ Y,
    radius = ~Totalltems_EventRecord,
    color = ~ pal(Is_AboveMean),
    popup = ~ paste0(
      "<b><font size = 4>",
      SubCountry_L2_FromSource,
      "</b></font>",
      "<br>Total Items recorded: ",
      format(Totalltems_EventRecord, big.mark = ","),
      "<br>Plastic and foam %: ",
      paste0(round(PCT_PlasticAndFoam, 1), "%")
    )) %>%
  addLayersControl(
    overlayGroups = c("2017", "2018"),
    options = layersControlOptions(collapsed = FALSE)
  )




# smaller area --------

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


# WIP
  
  
  
  
  
  
