library(tidyverse)
library(stringr)
library(sp)
library(rgdal)

# data key
# cataincidence = catastrophic spending 
# cataincidence_q = catastrophic spending by consumption quintile
# oop_che = out of pocket payments as a share of current spending on health
# unmetneed_hc = self-reported unmet need for healthcare
# read in data
dat <- read.csv('data-raw/example_dataset_whobcn_10_Nov.csv', stringsAsFactors = FALSE)

# make data long
dat <- dat %>% gather(key='year', value='value', -Indicator.code, -Country, -Country.code )

# remove percent from value column and X from year column
dat <- dat %>% mutate(year = gsub('X','',year),
                      value = as.numeric(gsub('%','',value)))

# recode columns 
names(dat) <- tolower(gsub('.', '_', names(dat), fixed = TRUE))

usethis::use_data(dat, overwrite = TRUE)

# read in shp files 
world <- readOGR('data-raw/world/', 'TM_WORLD_BORDERS-0.3')
usethis::use_data(world, overwrite = T)

