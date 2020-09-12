#REAMDE
#Folder for raw data from source

#CSV
dat <- read.csv("_Combined_MLW,_MDMAP,_TIDES_Data__All_Points,_2015-2018.csv", stringsAsFactors = F)
#parquet

library(arrow)
dat <- read_parquet("Combined_MLW_MDMAP_TIDES_Data__All_Points_2015-2018.parquet")
