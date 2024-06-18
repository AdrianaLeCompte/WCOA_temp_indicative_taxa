library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(readr)
devtools::install_github("an-bui/calecopal")
library(calecopal)
install.packages("paletter")
#setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa") #laptop wd
setwd("C:/Users/davidg/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa") #desktop wd

#################### importing biodiversity data
point_data <- read_excel("C:/Users/davidg/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa/raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "point_contact_summary_data")
quadrat_data <- read_excel("C:/Users/davidg/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa/raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "quadrat_summary_data")
swath_data <- read_excel("C:/Users/davidg/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa/raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "swath_summary_data")

# clean data --------------------------------------------------------------
quad.dat.clean <- quadrat_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

swat.dat.clean <- swath_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

all.bio <- bind_rows(quad.dat.clean, swat.dat.clean) #133 unique species in df

site.info<-distinct(all.bio, marine_site_name, latitude) %>% 
  arrange(desc(latitude)) %>% 
  rowid_to_column("id") %>%
  mutate(site_id=paste("m_site_",id, sep = "")) %>% 
  select(-id)

all.bio.pa<-all.bio %>% 
  left_join(., site.info, by="marine_site_name") %>%  
  mutate(p_a="yes", yr=paste("_", year, sep=""))  %>% 
  arrange(yr) %>% 
 # group_by(species_lump, site_id, year) %>% mutate(n=length(density_per_m2)) %>% filter(n>1) %>% 
 #  distinct(species_lump, marine_site_name)
 # 
 #  write.csv(all.bio.pa, "C:/Users/davidg/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa/multi occurance taxa.csv", row.names = FALSE)
  
  pivot_wider(id_cols=c(species_lump, site_id), names_from = yr, values_from = p_a, values_fn = max, values_fill = "no") %>% 
  arrange(species_lump, site_id) %>%
  pivot_longer(cols=c(-species_lump, -site_id), names_to = "yr", values_to = "p_a") %>% 
  group_by(species_lump, site_id) 

tax="Acanthinucella spp"
dat=all.bio.pa
sit="m_site_100"

run.fun<-function(tax, dat)
{
  df.1<-dat %>% 
    filter(species_lump==tax)
  
  sits<-unique(df.1$site_id)
  
  sit.itr<-function(sit)
  {
    df.2<-df.1 %>% 
      filter(site_id==sit)
    
    tax.sit.run<-rle(df.2$p_a)
   #tax.sit.run.p<-tax.sit.run$lengths[tax.sit.run$values=="yes"]
    
    tsr.df<-tibble("taxon"=tax, site_id=sit, "present"=tax.sit.run$values, "consecutive_years"=tax.sit.run$lengths)
    
  }
  
  
  
}

