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

#################### pt 1 set up data ################################################################################
########## quadrat data
quad.dat.clean <- quadrat_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

quad.dat.summ <- quad.dat.clean %>% #all taxa abundances by year and location
  mutate(species_lump = str_replace(species_lump, "Cucumaria/Pseudocnus spp", "Pseudocnus spp")) %>% 
  group_by(species_lump, year) %>%
  mutate(sum_dens=sum(density_per_m2)) %>%  #calculate total abundance in given year for each taxon across all sites
  ungroup() %>% 
  mutate(wt_lat = latitude*(density_per_m2/sum_dens)) %>% 
  group_by(species_lump, year) %>% 
  summarize(x_lat=mean(latitude), #average latitude per year of a given taxon across all sites
            x_abun=mean(density_per_m2), #average abundance 
            max_lat=max(latitude), #max and min latitude
            min_lat=min(latitude), 
            x_wtlat=sum(wt_lat)) %>% #sum of the site-specific abundance-weighted latitude
  ungroup() %>% 
  filter(x_abun > 0) %>% 
  group_by(species_lump) %>% 
  mutate(frequency = length(year),
         time_span=(max(year)-min(year))) %>% 
  ungroup()

########## swath data
swat.dat.clean <- swath_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

swat.dat.summ <- swat.dat.clean %>% #all taxa abundances by year and location
  group_by(species_lump, year) %>%
  mutate(sum_dens=sum(density_per_m2)) %>%  #calculate total abundance in given year for each taxon across all sites
  ungroup() %>% 
  mutate(wt_lat = latitude*(density_per_m2/sum_dens)) %>% 
  group_by(species_lump, year) %>% 
  summarize(x_lat=mean(latitude), #average latitude per year of a given taxon across all sites
            x_abun=mean(density_per_m2), #average abundance 
            max_lat=max(latitude), #max and min latitude
            min_lat=min(latitude), 
            x_wtlat=sum(wt_lat)) %>% #sum of the site-specific abundance-weighted latitude
  ungroup() %>% 
  filter(x_abun > 0) %>% 
  group_by(species_lump) %>% 
  mutate(frequency = length(year),
         time_span=(max(year)-min(year))) %>% 
  ungroup()

#################### pt 2 run linear model, clean up results ########################################################
########## quadrat data
max.lat.shifts.qdrt <- quad.dat.summ %>% 
  left_join(select(migration.df.qdrt, species_lump, direction),., by=c("species_lump"))%>% #filtering for only the significant taxa
  group_by(species_lump, direction) %>% 
  summarise(shift=(max(x_lat)-min(x_lat)),
            max_lat_all=max(x_lat),
            min_lat_all=min(x_lat)) %>% 
  arrange(desc(shift)) %>% #calculating the range of shift
  ungroup()
  # write.csv(max.lat.shifts.qdrt, "R outputs/max_latitude_shifts_quadrat.csv", row.names = F)

lm.infauna.qdrt <- quad.dat.summ %>% 
  filter(frequency >=5) %>% #removing taxa that were observed less than 10 times
  nest(dat=c(-species_lump, -time_span)) %>% #nesting data by unit of analysis - e.g., taxon and depth zone
  mutate(modz=map(dat, ~lm(x_wtlat~year, data=.x)), #"nested" simple linear models for each unit of analysis
         mod_sum=map(modz, broom::tidy), #nested model terms for each regression - intercept, slope, SE, pvalue, etc
         peep=map(modz, broom::glance)) #more nested model characteristics - AIC, sample size, df, r-square

migration.df.qdrt <- lm.infauna.qdrt %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% #unnesting model terms and tidying their names
  filter(term=="(Intercept)") %>% #isolating the intercept term from model summary
  select(species_lump,time_span,intercept=estimate, r.squared) %>% #retaining important info taxon names, the intercept value (and calling it intercept), and model r-squared
  right_join(lm.infauna.qdrt,., by=c("species_lump")) %>% #rejoining the same model outputs to extract and attach model slope information
  select(species_lump, mod_sum, intercept, r.squared) %>% #keeping variables of interest
  unnest(mod_sum) %>% filter(term=="year") %>% #extracting the information pertaining to the slope of the regression (i.e., term=year)
  rename(Year_est=estimate) %>% select(-term) %>% #renaming slope value to be "Year_est"
  filter(p.value<=0.1) %>% #selecting only "significant" taxa,i.e. those that showed statistically significant migration across the dataset
  mutate(direction=if_else(Year_est>0, "Northward","Southward")) #characterizing the nature of the migration
  # write.csv(migration.df.qdrt, "R outputs/migration_data_quadrat.csv", row.names = F)

########## swath data
max.lat.shifts.swth <- swat.dat.summ %>% 
  left_join(select(migration.df.swth, species_lump, direction),., by=c("species_lump"))%>% #filtering for only the significant taxa
  group_by(species_lump, direction) %>% 
  summarise(shift=(max(x_lat)-min(x_lat)),
            max_lat_all=max(x_lat),
            min_lat_all=min(x_lat)) %>% 
  arrange(desc(shift)) %>% #calculating the range of shift
  ungroup()
  # write.csv(max.lat.shifts.swth, "R outputs/max_latitude_shifts_swath.csv", row.names = F)

lm.infauna.swth <- swat.dat.summ %>% 
  filter(frequency >=5) %>% #removing taxa that were observed less than 10 times
  nest(dat=c(-species_lump, -time_span)) %>% #nesting data by unit of analysis - e.g., taxon and depth zone
  mutate(modz=map(dat, ~lm(x_wtlat~year, data=.x)), #"nested" simple linear models for each unit of analysis
         mod_sum=map(modz, broom::tidy), #nested model terms for each regression - intercept, slope, SE, pvalue, etc
         peep=map(modz, broom::glance)) #more nested model characteristics - AIC, sample size, df, r-square

migration.df.swth <- lm.infauna.swth %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% #unnesting model terms and tidying their names
  filter(term=="(Intercept)") %>% #isolating the intercept term from model summary
  select(species_lump,time_span,intercept=estimate, r.squared) %>% #retaining important info taxon names, the intercept value (and calling it intercept), and model r-squared
  right_join(lm.infauna.swth,., by=c("species_lump")) %>% #rejoining the same model outputs to extract and attach model slope information
  select(species_lump, mod_sum, intercept, r.squared) %>% #keeping variables of interest
  unnest(mod_sum) %>% filter(term=="year") %>% #extracting the information pertaining to the slope of the regression (i.e., term=year)
  rename(Year_est=estimate) %>% select(-term) %>% #renaming slope value to be "Year_est"
  filter(p.value<=0.1) %>% #selecting only "significant" taxa,i.e. those that showed statistically significant migration across the dataset
  mutate(direction=if_else(Year_est>0, "Northward","Southward")) #characterizing the nature of the migration
  # write.csv(migration.df.swth, "R outputs/migration_data_swath.csv", row.names = F)


#################### pt 3 plot data ################################################################################
########## quadrat data
species_list.qdrt<-unique(migration.df.qdrt$species_lump) #vector of species names to feed to the function

quad.dat.summ.v2 <- quad.dat.clean %>% #all taxa abundances by year and location
  group_by(species_lump, year) %>%
  mutate(sum_dens=sum(density_per_m2)) %>%  #calculate total abundance in given year for each taxon across all sites
  ungroup() %>% 
  mutate(wt_lat = latitude*(density_per_m2/sum_dens))

plot_species_qdrt<-function(xxx, dat) {
  
  dat.tax<-dat %>% inner_join(.,migration.df.qdrt, by=c("species_lump")) %>% #filtering for the north-south taxa
    filter(species_lump==xxx) #selecting an individual taxon for graphing
  
  plot.taxa<-ggplot(dat.tax, aes(x=year, y=x_wtlat))+ 
    theme_bw()+theme(panel.grid = element_blank(),
                     axis.text.y=element_text(face=c("bold", "plain","bold", "bold","plain", "bold","plain", "plain")),#bolding specific points on the y-axis that correspond to the scale y continuous
                     strip.background = element_blank(), strip.text = element_text(face="bold"))+
    geom_point(data = (filter(quad.dat.summ.v2, species_lump == xxx)), aes(x=year, y=latitude), size=1, shape=21, alpha=0.75, fill="grey")+ 
    geom_smooth(method = "lm", show.legend = FALSE, se=F)+
    geom_point(size=2, shape=21, alpha=0.75, fill="red")+
    ggtitle(xxx)+
    labs(x="year", y="Abundance Weighted Latitude")+ 
    facet_wrap("direction", nrow=2, ncol=5)
  print(plot.taxa)
  
  # ggsave(paste("R outputs/plots/", xxx, " quadrat_mean_weighted_lat.tiff",sep=""),
  #         plot.taxa, dpi=150, width=16, height=10, units="cm")
  # 
}

hummus.2<-map(species_list, ~plot_species_qdrt(.x,quad.dat.summ)) #iterating the function across the different north/south species

########## swath data
species_list.swth<-unique(migration.df.swth$species_lump) #vector of species names to feed to the function

swat.dat.summ.v2 <- swat.dat.clean %>% #all taxa abundances by year and location
  group_by(species_lump, year) %>%
  mutate(sum_dens=sum(density_per_m2)) %>%  #calculate total abundance in given year for each taxon across all sites
  ungroup() %>% 
  mutate(wt_lat = latitude*(density_per_m2/sum_dens))

plot_species_swth<-function(xxx, dat) {
  
  dat.tax<-dat %>% inner_join(.,migration.df.swth, by=c("species_lump")) %>% #filtering for the north-south taxa
    filter(species_lump==xxx) #selecting an individual taxon for graphing
  
  plot.taxa<-ggplot(dat.tax, aes(x=year, y=x_wtlat))+ 
    theme_bw()+theme(panel.grid = element_blank(),
                     axis.text.y=element_text(face=c("bold", "plain","bold", "bold","plain", "bold","plain", "plain")),#bolding specific points on the y-axis that correspond to the scale y continuous
                     strip.background = element_blank(), strip.text = element_text(face="bold"))+
    geom_point(data = swat.dat.summ.v2, aes(x=year, y=wt_lat), size=1, shape=21, alpha=0.75, fill="grey")+ 
    geom_smooth(method = "lm", show.legend = FALSE, se=F)+
    geom_point(size=2, shape=21, alpha=0.75, fill="red")+
    ggtitle(xxx)+
    labs(x="year", y="Abundance Weighted Latitude")+ 
    facet_wrap("direction", nrow=2, ncol=5)
  print(plot.taxa)
  
  # ggsave(paste("R outputs/plots/", xxx, " swath_mean_weighted_lat.tiff",sep=""),
  #        plot.taxa, dpi=150, width=16, height=10, units="cm")
  # 
}

hummus.2<-map(species_list.swth, ~plot_species_swth(.x,swat.dat.summ)) #iterating the function across the different north/south species

