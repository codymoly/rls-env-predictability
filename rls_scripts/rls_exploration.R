###### RLS DATA EXPLORATION
 
## source: https://portal.aodn.org.au/search
## accessed: 26 June 2023 at 10:50-UTC
## range: Lat/Lon -45.26, 107.53 - Lat/Lon -8.09, 169.23
## time span: 2019-Jan-01-00:00-UTC - 2023-Jun-26-23:59-UTC

# load libraries
library(ggplot2) # v.3.4.0
library(dplyr) # v.1.0.10
library(readr) # v.2.1.3
library(tidyr) # v.1.2.1

# clean memory
rm(list=ls())

# set working directory
setwd("~/Downloads")

# read RLS dataset
rls_data = readr::read_delim("rls_26062023.csv", skip = 71, delim = ",")

# check column names
names(rls_data)


##### geography
unique(rls_data$country) 
# "Australia" "New Caledonia"

# areas
unique(rls_data$area)
# [1] "Western Australia"  "New South Wales"    "Northern Territory" "Tasmania"           "Victoria"          
# [6] "South Australia"    "Queensland"         "Other Territories"  "Noumea"

unique(rls_data$ecoregion)
# [1] "Exmouth to Broome"                         "Manning-Hawkesbury"                       
# [3] "Bonaparte Coast"                           "Arnhem Coast to Gulf of Carpenteria"      
# [5] "Bassian"                                   "South Australian Gulfs"                   
# [7] "Cape Howe"                                 "Leeuwin"                                  
# [9] "Central and Southern Great Barrier Reef"   "Coral Sea"                                
# [11] "Tweed-Moreton"                             "Ningaloo"                                 
# [13] "Western Bassian"                           "Lord Howe and Norfolk Islands"            
# [15] "Houtman"                                   "Torres Strait Northern Great Barrier Reef"
# [17] "Gulf of Papua"                             "Shark Bay"                                
# [19] "New Caledonia"    

unique(rls_data$realm)
## output:
# [1] "Central Indo-Pacific"  "Temperate Australasia"

# locations
unique(rls_data$location)
# [1] "Ashmore/Cartier"            "Sydney"                     "Scott & Seringapatam Reefs"
# [4] "Kimberley"                  "Arafura"                    "Arnhem"                    
# [7] "Tinderbox"                  "Ninepin Point"              "Port Phillip Heads"        
# [10] "Port Phillip Bay"           "Port Stephens"              "Encounter"                 
# [13] "Port Davey"                 "Kangaroo Island"            "Gippsland Lakes"           
# [16] "Tasmania - South East"      "Swansea"                    "Adelaide"                  
# [19] "Maria Island"               "Bicheno"                    "Rocky Cape"                
# [22] "Jervis Bay"                 "Geographe Bay"              "QLD inshore - South"       
# [25] "Coral Sea - Central"        "D'Entrecasteaux & Derwent"  "GBR - South"               
# [28] "Rottnest Island"            "Solitary Islands"           "Ningaloo Reef"             
# [31] "Exmouth"                    "Batemans"                   "Coral Sea - South"         
# [34] "Lord Howe Island"           "Shoalwater"                 "Yorke Peninsula"           
# [37] "Marmion"                    "Wilsons Promontory"         "Beware Reef"               
# [40] "GBR - Central"              "QLD inshore - Central"      "Coral Sea - North"         
# [43] "Coral Sea - Far North"      "Torres Strait"              "West Cape York"            
# [46] "QLD inshore - North"        "GBR - North"                "Keppels"                   
# [49] "Capricorn-Bunker"           "Norfolk Island"             "Flinders Island"           
# [52] "Abrolhos (WA)"              "Cape Howe"                  "Shark Bay"                 
# [55] "Pilbara"                    "Byron"                      "Moreton"                   
# [58] "Great Sandy"                "Ningaloo Reef - North"      "NSW - Central North"       
# [61] "Wessel"                     "Darwin"                     "Oceanic Shoals"            
# [64] "Carpentaria"                "New Caledonia"              "Tweed Heads"               
# [67] "Gold Coast"                 "Tasmania - South"           "Port Stephens - North"     
# [70] "Perth Metro"                "Cod Grounds"    


##### identifier
length(unique(rls_data[["FID"]])) # 252506

length(unique(rls_data[["survey_id"]])) # 3886

length(unique(rls_data[["site_code"]])) # 1007

length(unique(rls_data[["site_name"]])) # 1003

rls_data %>%
  dplyr::select(latitude, longitude, survey_date) %>%
  dplyr::n_distinct()
# nrow(unique(rls_data[,c('latitude','longitude','survey_date')]))
# 1442

rls_data %>%
  dplyr::select(site_code, survey_date) %>%
  dplyr::n_distinct()
# nrow(unique(rls_data[,c('site_code','survey_date')]))
# 1620

##### geographical range
range(rls_data$latitude, na.rm = TRUE)
# -43.53  -9.88

range(rls_data$survey_latitude, na.rm = TRUE)
# -43.53  -9.88

range(rls_data$longitude, na.rm = TRUE)
# 105.67 167.99

range(rls_data$survey_longitude, na.rm = TRUE)
# 113.74 167.96

# let's look at the first rows of survey_latitude and survey_longitude
nrow(rls_data) # 252506
nrow(rls_data) - nrow(rls_data[complete.cases(rls_data$latitude),]) # 0
nrow(rls_data) - nrow(rls_data[complete.cases(rls_data$longitude),]) # 0
nrow(rls_data) - nrow(rls_data[complete.cases(rls_data$survey_latitude),]) # 138546
nrow(rls_data) - nrow(rls_data[complete.cases(rls_data$survey_longitude),]) # 138546

##### conclusion: use latitude and longitude columns


##### time period and survey time
range(rls_data$survey_date, na.rm = TRUE)
# "2019-01-01" "2023-06-02""
s_time = rls_data %>%
  dplyr::select(hour) %>%
  dplyr::distinct()
## surveys were done between 12am (midnight) and 17pm


##### program and method
unique(rls_data$program)
# "RLS"  "ATRC" "FRDC"

unique(rls_data$method)
# 1,  makes sense in view of the RLS manual (method 1 -> fish)

unique(rls_data$block)
# 2 1, check RLS manual, blocks can be merged


##### physicial parameters
range(rls_data$visibility, na.rm = TRUE)
# 1 60
range(rls_data$depth, na.rm = TRUE)
# 0.8 30


##### taxonomical data
unique(rls_data$phylum)
# "Chordata" 

unique(rls_data$class)
# "Actinopterygii"

unique(rls_data$order)
# [1] "Tetraodontiformes"         "Perciformes"               "Aulopiformes"              "Beryciformes"             
# [5] "Atheriniformes"            "Clupeiformes"              "Syngnathiformes"           "Anguilliformes"           
# [9] "Pectinoida"                "Gadiformes"                "Gobiesociformes"           "Scorpaeniformes"          
# [13] "Batrachoidiformes"         NA                          "Siluriformes"              "Beloniformes"             
# [17] "Eupercaria incertae sedis" "Zeiformes"                 "Pleuronectiformes"         "Acanthuriformes"          
# [21] "Callionymiformes"          "Blenniiformes"             "Kurtiformes"               "Gasterosteiformes"

nrow(unique(rls_data["family"])) # n = 99

nrow(unique(rls_data["species_name"])) # n = 1304

nrow(unique(rls_data["reporting_name"])) # n = 1291

##### ecological data
# size range
sum(is.na(rls_data$size_class)) # 0
range(rls_data$size_class, na.rm = TRUE) # 0 - 300 cm

# range of counts
sum(is.na(rls_data$total)) # 0
range(rls_data$total, na.rm = TRUE) # 0 - 11000

# range of biomass
sum(is.na(rls_data$biomass)) # 529
range(rls_data$biomass, na.rm = TRUE) # 0.0 - 754916.4 g

##### conclusion: species names need to be validated 

##### summarise stats
summary(rls_data)

##### visualisation
# randomly sample 20 surveys
sampled_surveys <- rls_data %>%
  dplyr::distinct(site_code, survey_date, depth) %>%
  dplyr::sample_n(20)

# subset data based on randomly selected surveys
subset_rls_data <- rls_data %>%
  dplyr::inner_join(sampled_surveys, by = c("site_code", "survey_date", "depth"))

# distribution plots for selected surveys
ggplot(subset_rls_data, aes(x = size_class)) +
  geom_density() +
  facet_grid(site_code ~ survey_date + depth, scales = "free") +
  labs(x = "Size Class", y = "Density") +
  theme_bw()

##### summary
# use coordinates from latitude and longitude columns
# average counts from both blocks (ref. RLS manual)
# calculate CWM per depth
