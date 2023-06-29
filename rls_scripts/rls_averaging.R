###### RLS DATA EAVERAGING

## source: https://portal.aodn.org.au/search
## accessed: 26 June 2023 at 10:50-UTC
## range: Lat/Lon -45.26, 107.53 - Lat/Lon -8.09, 169.23
## time span: 2019-Jan-01-00:00-UTC - 2023-Jun-26-23:59-UTC

# load libraries
library(corrplot) # v.0.92
library(ggplot2) # v.3.4.0
library(dplyr) # v.1.0.10
library(readr) # v.2.1.3
library(stringr) # v.1.4.1
library(tidyr) # v.1.2.1
library(vegan) # v.2.6-4

# clean memory
rm(list=ls())

# set working directory
setwd("~/Downloads")

# read RLS dataset
rls_data = readr::read_delim("rls_26062023.csv", skip = 71, delim = ",")

# save copy
rls_copy = rls_data

# size_class contains zeros, which represent missing values in this dataset
summary(rls_copy$size_class)

# replace zeros by NAs
rls_copy["size_class"] = lapply(rls_copy["size_class"], function(x) 
  replace(x, rls_copy$size_class == 0.0, NA))

# check NAs in size_class
summary(rls_copy$size_class) # 95 NAs

## remove brackets from species name, e.g., change Pomacentrus sp. [rhodonotus] to Pomacentrus rhodonotus
## write function that explaces the different signs
rmBrackets = function(spname){
  return(stringr::str_replace_all(spname, "sp\\. \\[([^\\\\]*)\\]", "\\1"))
}
## apply function on species column in original file
rls_copy["species_name"] <- lapply(rls_copy["species_name"], rmBrackets)

## select unique species
rls_copy %>%
  dplyr::distinct(species_name)
### number of species: 1293

# # assign surveys into depth bins
# range(rls_data$depth)
# # rls_subset$hour = as.POSIXct(rls_subset$hour, format="%H:%M:%S", tz="UTC")
# rls_binned = rls_data %>% 
#   dplyr::mutate(depth_bin = cut(depth, breaks = c(0,5,10,15,20,25,30)))

# average data pers survey over both blocks
rls_avg = rls_binned %>% 
  group_by(latitude, longitude, survey_date, depth_bin, species_name) %>% 
  summarise(
    size_class_mean = mean(size_class, na.rm = TRUE),
    total_mean = mean(total, na.rm = TRUE),
    biomass_mean = mean(biomass, na.rm = TRUE)
  ) %>% 
  #mutate(ID = cur_group_id()) %>% # assign unique id number to each group
  ungroup()

# calculate community stuff
rls_cwm = rls_copy %>%
  dplyr::group_by(survey_id) %>%  
  dplyr::summarise(
    number_individuals = sum(na.omit(total)),
    total_biomass = sum(na.omit(biomass)),
    shannon = vegan::diversity(total,index = "shannon"),
    sp_richness = vegan::specnumber(species_name),
    sp_evenness = shannon/log(number_individuals),
    size_cwm_total = stats::weighted.mean(size_class, total, na.rm = TRUE),
    size_cwm_biomass = weighted.mean(size_class, biomass, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup()

# add selected columns of original data
rls_joined = left_join(rls_cwm,
                       rls_data %>% select(survey_id, site_code, country, ecoregion, latitude, longitude, survey_date, area, depth, program),
                       by = 'survey_id',
                       keep = FALSE) %>% 
  distinct(survey_id, .keep_all = TRUE) %>%
  select(survey_id, site_code, country, ecoregion, area, program, 
         latitude, longitude, survey_date, depth,
         number_individuals, total_biomass, sp_richness, sp_evenness, shannon, size_cwm_total, size_cwm_biomass)

# save cwm data
write.csv(rls_joined,"~/Documents/thesis_publication/survey_cwm_data.csv", row.names = FALSE)
