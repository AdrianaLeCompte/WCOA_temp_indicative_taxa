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

# clean data --------------------------------------------------------------
quad.dat.clean <- quadrat_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

swat.dat.clean <- swath_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

all.bio <- bind_rows(quad.dat.clean, swat.dat.clean) #133 unique species in df

# plot 1 ------------------------------------------------------------------
all.bio.1 <- bind_rows(quad.dat.clean, swat.dat.clean) %>% 
  select(species_lump, year) %>% distinct()

tbl_1 <- all.bio.1 %>%
  group_by(species_lump) %>% 
  summarise(num_yrs = n()) %>% 
  ungroup() %>% 
  group_by(num_yrs) %>% 
  summarise(num_sp = n())

plot.1 <- ggplot(data = tbl_1, aes(x = num_yrs, y = num_sp))+
  geom_col()+ 
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank())+
  labs(x="number of years", y="number of species")
print(plot.1)

# ggsave(paste("R outputs/plots/plot_1.tiff"),
#         plot.1, dpi=150, width=16, height=10, units="cm")


# plot 2 ------------------------------------------------------------------
summ.df <- all.bio %>% #133 unique species in df
  mutate(species_lump = str_replace(species_lump, "Cucumaria/Pseudocnus spp", "Pseudocnus spp")) %>% 
  group_by(species_lump, year) %>%
  mutate(sum_dens=sum(density_per_m2)) %>%  
  ungroup() %>% 
  mutate(wt_lat = latitude*(density_per_m2/sum_dens)) %>% 
  group_by(species_lump, year) %>% 
  summarize(x_lat=mean(latitude), 
            x_abun=mean(density_per_m2), 
            max_lat=max(latitude), 
            min_lat=min(latitude), 
            x_wtlat=sum(wt_lat)) %>% 
  ungroup() %>% 
  filter(x_abun > 0) %>% 
  group_by(species_lump) %>% 
  mutate(frequency = length(year),
         time_span=(max(year)-min(year))) %>% 
  ungroup()

lm.df <- summ.df %>% #100 unique species in df
  filter(frequency >=5) %>% 
  nest(dat=c(-species_lump, -time_span)) %>% 
  mutate(modz=map(dat, ~lm(x_wtlat~year, data=.x)), 
         mod_sum=map(modz, broom::tidy), 
         peep=map(modz, broom::glance))

shift.df <- lm.df %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% 
  filter(term=="(Intercept)") %>% 
  select(species_lump,time_span,intercept=estimate, r.squared) %>% 
  right_join(lm.df,., by=c("species_lump")) %>% 
  select(species_lump, mod_sum, intercept, r.squared) %>% 
  unnest(mod_sum) %>% filter(term=="year") %>% 
  rename(Year_est=estimate) %>% select(-term) %>% 
  mutate(direction=if_else(Year_est>0, "Northward","Southward")) 
# write.csv(migration.df.qdrt, "R outputs/migration_data_quadrat.csv", row.names = F)


