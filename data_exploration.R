library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa")

#################### importing biodiversity data
point_data <- read_excel("data/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "point_contact_summary_data")
quadrat_data <- read_excel("data/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "quadrat_summary_data")
swath_data <- read_excel("data/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "swath_summary_data")

#################### collecting how many years of biodiversity data each site has
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

#################### creating a species list by collection method (biodiversity data)
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