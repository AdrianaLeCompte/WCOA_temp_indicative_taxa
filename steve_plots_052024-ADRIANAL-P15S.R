library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(readr)
#setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa") #laptop wd
setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa") #desktop wd

#################### importing biodiversity data
point_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "point_contact_summary_data")
quadrat_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "quadrat_summary_data")
swath_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "swath_summary_data")

quad.dat.clean <- quadrat_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

swat.dat.clean <- swath_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

all_biodi <- bind_rows(quad.dat.clean, swat.dat.clean) %>% 
  select(species_lump, year) %>% distinct()

tbl_1 <- all_biodi %>%
  group_by(species_lump) %>% 
  summarise(num_yrs = n()) %>% 
  ungroup() %>% 
  group_by(num_yrs) %>% 
  summarise(num_sp = n())

ggplot(data = tbl_1, aes(x = num_yrs, y = num_sp)) + geom_col()

all_biodi_v2 <- bind_rows(quad.dat.clean, swat.dat.clean) %>% 
  select(species_lump, year, marine_site_name) %>% distinct()

tbl_2 <- 