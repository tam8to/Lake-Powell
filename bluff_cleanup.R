# This file contains all the script for tidying up raw files.
# Set working drive
# Load libraries
library(tidyverse)
library(lubridate)
library(rio)

# Read in all csv files at one time with assigned source to create one dataframe for all locations.
# Working drive and "folder" need to be the same for this chunk of code to work.
folder <- "C:/Users/tamtam/Documents/Lake-Powell/data/raw"
filenames <- list.files(path = folder, pattern = "*.csv", all.files = FALSE, full.names = FALSE,
                        recursive = FALSE, ignore.case = FALSE)
read_csv_filename <- function(filenames){
  ret <- read.csv(filenames, stringsAsFactors = F, strip.white = T)
  ret$Source <- filenames
  ret
}
powell_raw <- plyr::ldply(filenames, read_csv_filename)

# Reset working drive to Lake-Powell.
# Combine sample date and time to account for multi-day/time sampling events as separate observations.
powell_raw <- powell_raw %>%
  mutate(sample.time = ifelse(sample_tm == "", "00:00", sample_tm))
powell_raw <- powell_raw %>%
  unite(date.time, sample_dt, sample.time, sep = " ", remove = F)
powell_raw$date.time <- mdy_hm(powell_raw$date.time)

saveRDS(powell_raw, "data/powell_raw.rds")

# Tidy data where column is "parameter" and the parameter becomes the value
powell <- powell_raw %>%
  gather(parameter, measurement, 14:967, -Source, -sample.time, -year, -Year, -date, -Day, -month, -Month)
powell$parameter <- as.factor(powell$parameter)

# Tidy up Source values
powell$site.name <- ifelse(grepl("Bluff", powell$Source), "Bluff",
                      ifelse(grepl("Colorado", powell$Source), "Colorado",
                        ifelse(grepl("Green", powell$Source), "Green",
                          ifelse(grepl("Lees", powell$Source), "Lees",
                            ifelse(grepl("Cisco", powell$Source), "Cisco", "Other")))))

# Eliminate unnecessary columns:
#   - agency_cd (all USGS)
#   - tm_datum_rlbty_cd (not useful for this analysis)
#   - tui_id (all NAs)
#   - body_part_id (all NAs)
#   - Source, and any other previously created columns
powell <- powell %>%
  select(-agency_cd, -tm_datum_rlbty_cd, -tu_id, -body_part_id, -Source,
         -sample.time, -year, -Year, -date, -Day, -month, -Month)

saveRDS(powell, "data/powell.rds")

# Looking for parameters that have been monitored since at least 1950 (ten years before dam construction).
# Dam construction 1960-06-16 through 1963-09-13
powell_predam <- powell_raw %>%
  filter(sample_dt < 1950-01-01) %>%
  select_if(~sum(!is.na(.)) > 0)
predam_powell <- powell_predam %>%
  gather(parameter, measurement, 12:887, -Source, -sample.time, -year, -Year, -date, -Day, -month, -Month)
predam_powell$parameter <- as.factor(predam_powell$parameter)
predam_parameters <- names(table(predam_powell$parameter)) %>% as.data.frame()
colnames(predam_parameters) <- c("parameter")
predam_parameters$parameter <- as.character(predam_parameters$parameter)

# 868 parameter values down from a total of 967 parameters.
# Use predam_parameters to subset powell dataframe.

powell$parameter <- as.character(powell$parameter)
predam_raw <- semi_join(powell, predam_parameters, by = "parameter")
predam$parameter <- as.factor(predam$parameter)

# Too many coded parameters!
# Create table parameter code and associated parameter group, other descriptions, and units.
# Table data taken from https://nwis.waterdata.usgs.gov/nwis/pmcodes
pcode <- import("data/usgs_pcodes", format = "psv")
pcode_c <- pcode %>%
  mutate(pcode = paste("p", parameter_cd, sep = "")) %>%
  select(group = parameter_group_nm, description = parameter_nm, units = parameter_units, parameter = pcode)
saveRDS(pcode_c, "data/pcode.rds")

# Add parameter descriptions to predam dataframe
predam_raw$parameter <- as.character(predam_raw$parameter)
predam_add <- left_join(predam_raw, pcode_c, by = "parameter")

# De-duplicate multiple entries for each site and type of sampling, eliminate any NA measurements.
predam <- predam_add %>%
  filter(measurement != "NA") %>%
  group_by(site.name, medium_cd, parameter) %>%
  distinct(date.time, .keep_all = T) %>%
  as.data.frame()

saveRDS(predam, "data/predam.rds")

# Look for parameters consistent across all sites.
# Exclude groups of parameters if one site has less than 100 observations
table(predam$group, predam$site.name)

exclude.groups <- c("Stable Isotopes", "Organics, pesticide", "Organics, PCBs", 
                    "Radiochemical", "Biological", "Organics, other")
predam_c <- filter(predam, !group %in% exclude.groups)

saveRDS(predam_c, "data/predam_c.rds")

predam_d <- table(predam$parameter, predam$site.name) %>%
  as.data.frame()
predam_d <- predam_d %>%
  mutate(Freq = replace(Freq, Freq == 0, NA)) %>%
  spread(Var2, Freq)
predam_e <- predam_d %>%
  filter(complete.cases(.)) %>%
  as.data.frame()

# Exclude parameters if there are less than 500 observations combined across all sites
predam_e$total.obs <- rowSums(predam_e[2:6])
predam_f <- filter(predam_e, total.obs > 500) %>%
  as.data.frame()

# Filter predam data for only parameters that were measured arcross all site
# and have at least over 500 observations total.
predam_f <- rename(predam_f, parameter = Var1)
predam_tidy <- semi_join(predam_c, predam_f, by = "parameter")


# Export list and description of remaining parameter codes to look if there are
# any parameters we're not currently interested in examining.
pcode_table <- distinct(predam_tidy, parameter, .keep_all = T) %>%
  select(parameter, group, description)
write.csv(pcode_table, "data/LakePowell_pcode.csv", row.names = F)

# Eliminate any non-essential parameters - edited by B.Deemer.
pcode_table_DB <- read.csv("data/LakePowell_pcode_BD.csv", stringsAsFactors = F)
pcode_table_ex <- filter(pcode_table_DB, Exclude == "x")

predam_tidier <- anti_join(predam_tidy, pcode_table_ex, by = "parameter")
table(predam_tidier$parameter, predam_tidier$site.name)
predam_tidier$parameter <- as.factor(predam_tidier$parameter) # 78 parameters

saveRDS(predam_tidier, "data/predam_tidier.rds")

# Some methods for measuring certain parameters have changed over the years,
# each change in method results in a new parameter code (e.g. bicarbonate).
# R markdown file, "Appendix1.Rmd" shows figures of all remaining parameters to 
# see if there are any obvious changes in methods.

# Second pass of eliminating parameters.

pcode_table_DB <- read.csv("data/LakePowell_pcode_BD.csv", stringsAsFactors = F)
pcode_table_ex <- filter(pcode_table_DB, Exclude == "x")

predam_tidier$parameter <- as.character(predam_tidier$parameter)
predam_tidying <- anti_join(predam_tidier, pcode_table_ex, by = "parameter")

pcode_table_in <- filter(pcode_table_DB, Exclude != "x") %>%
  select(parameter, new.parameter.code) %>%
  filter(new.parameter.code != "") %>%
  as.data.frame()
predam_tidying2 <- left_join(predam_tidying, pcode_table_in, by = "parameter")
predam_tidying2 <- predam_tidying2 %>%
  mutate(rev.parameter = ifelse(is.na(new.parameter.code), parameter, new.parameter.code))
table(predam_tidying2$rev.parameter, predam_tidying2$site.name)

# Further exclude revised parameter p71999 due to low sample number for Colorado site.
predam_tidiest <- filter(predam_tidying2, rev.parameter != "p71999") %>%
  as.data.frame()

# Save predam_tidiest and use for further analysis.
saveRDS(predam_tidiest, "data/predam_tidiest.rds")
