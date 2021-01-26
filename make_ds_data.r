#make ds data
library(tidyverse)

rm(list=ls())

## import data - !! data must be sorted YEAR > MONTH > DAY > FILEID > EVENTNO !!
# alter this path for your computer
#dat <- read.csv("/home/dan/Documents/WorkDocuments/Projects/MassCEC2731/Aerial survey data/BK Data Processed 2011 - 2018 ORIGINAL/NEAQ 2011-2019_PHSTRIP-dp1.csv")
dat <- read.csv("C://Users//oobrien//Documents//R_Work_Directory//Distance//NEAQ 2011-2019_PHSTRIP-dp.csv")

## select your season beginning and ending as "YEAR.MO"
season.beg <- 2019.06
season.end <- 2019.06

## specify data directory and place where new files will be saved
target_dir <- paste(getwd(), "/output/", sep="")
print(target_dir)

## run ds_data_prep.r
#set condensed to 1 or 0 to include or exclude condensed surveys
condensed = 0
source('ds_data_prep.r')
dat <- ds_data_prep(dat, season.beg, season.end, condensed)

## run ds_data.r
# select species to include
#spp = c("RIWH","FIWH","HUWH","SEWH")
spp = c("RIWH")
#spp = c("RIWH","HUWH")
#spp = c("RIWH","HUWH","FIWH","SEWH")

#set phstrip to 0 or 1.
phstrip = 1
source('ds_data.r')
ds_data(dat, target_dir, spp, phstrip)
