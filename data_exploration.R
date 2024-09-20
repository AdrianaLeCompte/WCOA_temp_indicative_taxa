library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(readr)
setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa")

# importing biodiversity data ---------------------------------------------
point_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "point_contact_summary_data")
quadrat_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "quadrat_summary_data")
swath_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "swath_summary_data")

# collecting how many years of biodiversity data each site has ------------
yr_pt_data <- point_data %>% select(marine_site_name, year, latitude, longitude) %>% distinct
yr_qdrt_data <- quadrat_data %>% select(marine_site_name, year, latitude, longitude) %>% distinct
yr_swth_data <- swath_data %>% select(marine_site_name, year, latitude, longitude) %>% distinct

temp_tbl <- bind_rows(yr_pt_data, yr_qdrt_data)
all_years <- bind_rows(temp_tbl, yr_swth_data) %>% distinct

summary_tbl <- all_years %>% 
  group_by(marine_site_name) %>% 
  mutate(min_yr = min(year)) %>% 
  mutate(max_yr = max(year)) %>% 
  mutate(total_yrs = length(year)) %>% 
  select(-year) %>% distinct

write.csv(summary_tbl, "R outputs/years_stations_summary_coords.csv", row.names = F)

# creating a species list by collection method (bio data) -----------------
species_pt <- point_data %>% 
  filter(number_of_hits > 0) %>% 
  select(species_lump) %>% 
  distinct %>% 
  arrange(species_lump)

species_qdrt <- quadrat_data %>%
  filter(total_count > 0) %>% 
  select(species_lump) %>% 
  distinct %>% 
  arrange(species_lump)
  
species_swth <- swath_data %>% 
  filter(total_count > 0) %>% 
  select(species_lump) %>% 
  distinct %>% 
  arrange(species_lump)

write.csv(species_pt, "R outputs/species_list_point.csv", row.names = F)
write.csv(species_qdrt, "R outputs/species_list_quadrat.csv", row.names = F)
write.csv(species_swth, "R outputs/species_list_swath.csv", row.names = F)

# importing long term data ------------------------------------------------
species_code_key <- read_csv("raw data (MARINe)/long term monitoring/marine_lumping_codes_definitions.csv")

long_term_data <- read_csv("raw data (MARINe)/long term monitoring/phototransummarysd_download.csv")

phot_tran_species <- long_term_data %>% 
  filter(average_percent_cover > 0) %>% # 49 species had abundance >0
  rename(lumping_code = species_code) %>% 
  select(lumping_code) %>% distinct %>% 
  left_join(species_code_key) %>% 
  filter(kingdom == "Animalia") # 16 species are animalia, 12 chromista, 16 plantae, 5 null

sp.list <- phot_tran_species$lumping_code

write.csv(phot_tran_species, "R outputs/species_list_longtermdata.csv", row.names = F)

phot_tran_years <- long_term_data %>% 
  filter(average_percent_cover > 0) %>% 
  select(marine_site_name, marine_common_year) %>% 
  distinct %>% 
  group_by(marine_site_name) %>% 
  mutate(min_yr = min(marine_common_year)) %>% 
  mutate(max_yr = max(marine_common_year)) %>% 
  mutate(total_yrs = length(marine_common_year)) %>%
  select(-marine_common_year) %>% 
  distinct
  
phot_tran_species_v2 <- long_term_data %>% 
  filter(average_percent_cover > 0) %>% 
  rename(lumping_code = species_code) %>% 
  select(marine_site_name, marine_common_year, lumping_code) %>% 
  filter(lumping_code %in% sp.list) %>% distinct()

