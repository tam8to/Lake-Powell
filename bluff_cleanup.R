# Cleaning and tidying
# Set working drive
# Load libraries
library(tidyverse)
library(lubridate)
library(rio)


bluff_raw <- read.csv("data/Bluff_Long_Term.csv", stringsAsFactors = F, strip.white = T)
bluff_raw$sample_dt <- mdy(bluff_raw$sample_dt)

# A total of 509 variables spanning the entire data set.
# Check to see how many variables were monitored prior to damn construction.
# Dam construction 1960-06-16 through 1963-09-13
bluff_predam <- bluff_raw %>%
  filter(sample_dt < 1960-06-16) %>%
  select_if(~sum(!is.na(.)) > 0)

# Too many coded variables.
# Create table variable code and associated variable group, other descriptions, and units.
# Table data taken from https://nwis.waterdata.usgs.gov/nwis/pmcodes
pcode <- import("data/usgs_pcodes", format = "psv")
