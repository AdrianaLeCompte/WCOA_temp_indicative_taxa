
#################### libraries and wd --------------------------------------------------------
library(zoo)
library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(readr)
library(broom)
setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa") #laptop wd
setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa") #desktop wd

#################### importing biodiversity data ---------------------------------------------
point_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "point_contact_summary_data")
quadrat_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "quadrat_summary_data")
swath_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "swath_summary_data")

#################### pt 1 set up data --------------------------------------------------------
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

#################### pt 2 run linear model, clean up results --------------------------------------------------------
########## quadrat data
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

max.lat.shifts.qdrt <- quad.dat.summ %>% 
  left_join(select(migration.df.qdrt, species_lump, direction),., by=c("species_lump"))%>% #filtering for only the significant taxa
  group_by(species_lump, direction) %>% 
  summarise(shift=(max(x_lat)-min(x_lat)),
            max_lat_all=max(x_lat),
            min_lat_all=min(x_lat)) %>% 
  arrange(desc(shift)) %>% #calculating the range of shift
  ungroup()
# write.csv(max.lat.shifts.qdrt, "R outputs/max_latitude_shifts_quadrat.csv", row.names = F)

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


#################### pt 3 plot data 050725 --------------------------------------------------------
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


#################### pt 4 rolling averages (2 yr) --------------------------------------------------------

quad.dat.2 <- quadrat_data %>% 
  filter(species_lump == "Alia spp") %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>%
  group_by(species_lump, latitude) %>% 
  mutate(r2yravg=rollmean(density_per_m2, k=2, fill=NA)) %>% 
  ungroup() %>% 
  group_by(species_lump, year) %>%
  mutate(sum_avg=sum(r2yravg)) %>% 
  mutate(wt_lat = latitude*(r2yravg/sum_avg)) %>%
  summarize(x_lat=mean(latitude), 
            x_abun=mean(r2yravg), 
            max_lat=max(latitude), 
            min_lat=min(latitude), 
            x_wtlat=sum(wt_lat)) %>% 
  ungroup() %>% 
  drop_na(x_wtlat) %>% 
  group_by(species_lump) %>% 
  mutate(frequency = length(x_abun>0)) %>% 
  mutate(frequency = length(year),
         time_span=(max(year)-min(year))) %>% 
  filter(frequency>0)
view(quadrat_data)

v1 <- quadrat_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>%
  group_by(species_lump, latitude) %>%
  mutate(avg2yr= mean(density_per_m2))

# write.csv(quad.dat.2, file = "C:/Users/adrianal/Downloads/aliaspp.csv", row.names = F)
# saved file filtered with one species to test rollmean function

lm.qdrt.2 <- quad.dat.2 %>% 
  nest(dat=c(-species_lump, -time_span)) %>% 
  mutate(modz=map(dat, ~lm(x_wtlat~year, data=.x)), 
         mod_sum=map(modz, broom::tidy), 
         peep=map(modz, broom::glance)) 
view(lm.qdrt.2)

shift.qdrt.2 <- lm.qdrt.2 %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% 
  filter(term=="(Intercept)") %>% 
  select(species_lump,time_span,intercept=estimate, r.squared) %>% 
  right_join(lm.qdrt.2,., by=c("species_lump")) %>% # highlight to David that this was wrong last time we ran it, it was joining to the old df, not the rollmean one
  select(species_lump, mod_sum, intercept, r.squared) %>% 
  unnest(mod_sum) %>% filter(term=="year") %>% 
  rename(Year_est=estimate) %>% select(-term) %>% 
  filter(p.value<=0.1) %>% 
  mutate(direction=if_else(Year_est>0, "Northward","Southward"))
view(shift.qdrt.2)

write.csv(shift.qdrt.2, file="R outputs/quadrat_2yravg_species.csv", row.names = F)

max.shft.qdrt.2 <- quad.dat.2 %>% 
  left_join(select(shift.qdrt.2, species_lump, direction),., by=c("species_lump"))%>% 
  group_by(species_lump, direction) %>% 
  summarise(shift=(max(x_lat)-min(x_lat)),
            max_lat_all=max(x_lat),
            min_lat_all=min(x_lat)) %>% 
  arrange(desc(shift)) %>% 
  ungroup() %>% 
  filter(shift>0)
view(max.shft.qdrt.2)

########## swath data
swth.dat.2 <- swath_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  group_by(species_lump, latitude) %>%
  mutate(r2yravg=rollmean(density_per_m2, k=2, fill=NA)) %>%
  ungroup() %>% 
  drop_na(r2yravg) %>% 
  group_by(species_lump, year) %>%
  mutate(sum_avg=sum(r2yravg)) %>% 
  mutate(wt_lat = latitude*(r2yravg/sum_avg)) %>%
  summarize(x_lat=mean(latitude), 
            x_abun=mean(r2yravg), 
            max_lat=max(latitude), 
            min_lat=min(latitude), 
            x_wtlat=sum(wt_lat)) %>% 
  ungroup() %>% 
  drop_na(x_wtlat) %>% 
  group_by(species_lump) %>% 
  mutate(frequency = length(x_abun>0)) %>% 
  mutate(frequency = length(year),
         time_span=(max(year)-min(year))) %>% 
  filter(frequency>0)
view(swth.dat.2)

lm.swth.2 <- swth.dat.2 %>% 
  nest(dat=c(-species_lump, -time_span)) %>% 
  mutate(modz=map(dat, ~lm(x_wtlat~year, data=.x)), 
         mod_sum=map(modz, broom::tidy), 
         peep=map(modz, broom::glance)) 
view(lm.swth.2)

shift.swth.2 <- lm.swth.2 %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% 
  filter(term=="(Intercept)") %>% 
  select(species_lump,time_span,intercept=estimate, r.squared) %>% 
  right_join(lm.swth.2,., by=c("species_lump")) %>% 
  select(species_lump, mod_sum, intercept, r.squared) %>%
  unnest(mod_sum) %>% filter(term=="year") %>% 
  rename(Year_est=estimate) %>% select(-term) %>% 
  filter(p.value<=0.1) %>% 
  mutate(direction=if_else(Year_est>0, "Northward","Southward"))
view(shift.swth.2)

write.csv(shift.swth.2, file="R outputs/swath_2yravg_species.csv", row.names = F)

#################### pt 5 rolling averages (3 yr) --------------------------------------------------------

quad.dat.3 <- quadrat_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>%
  group_by(species_lump, latitude) %>% 
  mutate(r3yravg=rollmean(density_per_m2, k=3, fill=NA)) %>% 
  ungroup() %>% 
  group_by(species_lump, year) %>%
  mutate(sum_avg=sum(r3yravg)) %>% 
  mutate(wt_lat = latitude*(r3yravg/sum_avg)) %>%
  summarize(x_lat=mean(latitude), 
            x_abun=mean(r3yravg), 
            max_lat=max(latitude), 
            min_lat=min(latitude), 
            x_wtlat=sum(wt_lat)) %>% 
  ungroup() %>% 
  drop_na(x_wtlat) %>% 
  group_by(species_lump) %>% 
  mutate(frequency = length(x_abun>0)) %>% 
  mutate(frequency = length(year),
         time_span=(max(year)-min(year))) %>% 
  filter(frequency>0)
view(quad.dat.3)

lm.qdrt.3 <- quad.dat.3 %>% 
  nest(dat=c(-species_lump, -time_span)) %>% 
  mutate(modz=map(dat, ~lm(x_wtlat~year, data=.x)), 
         mod_sum=map(modz, broom::tidy), 
         peep=map(modz, broom::glance)) 
view(lm.qdrt.3)

shift.qdrt.3 <- lm.qdrt.3 %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% 
  filter(term=="(Intercept)") %>% 
  select(species_lump,time_span,intercept=estimate, r.squared) %>% 
  right_join(lm.qdrt.3,., by=c("species_lump")) %>% 
  select(species_lump, mod_sum, intercept, r.squared) %>% 
  unnest(mod_sum) %>% filter(term=="year") %>% 
  rename(Year_est=estimate) %>% select(-term) %>% 
  filter(p.value<=0.1) %>% 
  mutate(direction=if_else(Year_est>0, "Northward","Southward"))
view(shift.qdrt.3)

write.csv(shift.qdrt.3, file="R outputs/quadrat_3yravg_species.csv", row.names = F)

max.shft.qdrt.3 <- quad.dat.3 %>% 
  left_join(select(shift.qdrt.3, species_lump, direction),., by=c("species_lump"))%>% 
  group_by(species_lump, direction) %>% 
  summarise(shift=(max(x_lat)-min(x_lat)),
            max_lat_all=max(x_lat),
            min_lat_all=min(x_lat)) %>% 
  arrange(desc(shift)) %>% 
  ungroup() %>% 
  filter(shift>0)
view(max.shft.qdrt.3)

########## swath data
swth.dat.3 <- swath_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  group_by(species_lump, latitude) %>%
  mutate(r2yravg=rollmean(density_per_m2, k=3, fill=NA)) %>%
  ungroup() %>% 
  drop_na(r3yravg) %>% 
  group_by(species_lump, year) %>%
  mutate(sum_avg=sum(r3yravg)) %>% 
  mutate(wt_lat = latitude*(r3yravg/sum_avg)) %>%
  summarize(x_lat=mean(latitude), 
            x_abun=mean(r3yravg), 
            max_lat=max(latitude), 
            min_lat=min(latitude), 
            x_wtlat=sum(wt_lat)) %>% 
  ungroup() %>% 
  drop_na(x_wtlat) %>% 
  group_by(species_lump) %>% 
  mutate(frequency = length(x_abun>0)) %>% 
  mutate(frequency = length(year),
         time_span=(max(year)-min(year))) %>% 
  filter(frequency>0)
view(swth.dat.3)

lm.swth.3 <- swth.dat.3 %>% 
  nest(dat=c(-species_lump, -time_span)) %>% 
  mutate(modz=map(dat, ~lm(x_wtlat~year, data=.x)), 
         mod_sum=map(modz, broom::tidy), 
         peep=map(modz, broom::glance)) 
view(lm.swth.3)

shift.swth.3 <- lm.swth.3 %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% 
  filter(term=="(Intercept)") %>% 
  select(species_lump,time_span,intercept=estimate, r.squared) %>% 
  right_join(lm.swth.3,., by=c("species_lump")) %>% 
  select(species_lump, mod_sum, intercept, r.squared) %>%
  unnest(mod_sum) %>% filter(term=="year") %>% 
  rename(Year_est=estimate) %>% select(-term) %>% 
  filter(p.value<=0.1) %>% 
  mutate(direction=if_else(Year_est>0, "Northward","Southward"))
view(shift.swth.3)

write.csv(shift.swth.3, file="R outputs/swath_3yravg_species.csv", row.names = F)



#################### pt 6 abundance & temperature plotting ----------------------------------------

## df.clean is found under "temp_boxplot_code.R" and temperature data came from raster imaging
tmp.df <- df.clean %>% 
  group_by(site, yr) %>% 
  mutate(max.tmp = max(temp_value)) %>% 
  ungroup() %>% 
  select(site, y, yr, max.tmp) %>% 
  unique()

tmp.abn <- quadrat_data %>% 
  filter(island=="Mainland") %>% 
  filter(state_province==c("California", "Washington", "Oregon")) %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>%
  filter(density_per_m2 > 0) %>% 
  rename(site = marine_site_name) %>% 
  mutate(yr = str_sub(year, 3,4)) %>% 
  left_join(tmp.df, by = c("site", "yr")) %>% 
  select(species_lump, site, year, density_per_m2, max.tmp) %>% 
  drop_na(max.tmp)

tmp.abn.2 <- quadrat_data %>% 
  filter(island=="Mainland") %>% 
  filter(state_province==c("California", "Washington", "Oregon")) %>% 
  select(marine_site_name, year) %>% 
  rename(site = marine_site_name) %>% 
  distinct() %>% 
  mutate(yr = str_sub(year, 3,4)) %>% 
  mutate(yr = as.numeric(yr)) %>% 
  mutate(prev_yr = yr-1) %>%
  mutate(prev_yr = as.character(prev_yr)) %>% 
  mutate(prev_yr = case_when(
    prev_yr=="0"~"00",
    prev_yr=="1"~"01",
    prev_yr=="2"~"02",
    prev_yr=="3"~"03",
    prev_yr=="4"~"04",
    prev_yr=="5"~"05",
    prev_yr=="6"~"06",
    prev_yr=="7"~"07",
    prev_yr=="8"~"08",
    prev_yr=="9"~"09",
    .default = as.character(prev_yr))) %>% 
  left_join(tmp.df, by = c("site" = "site", "prev_yr" = "yr")) %>% 
  rename("prev_yr_tmp" = "max.tmp") %>% 
  mutate(prev_yr = as.numeric(prev_yr)) %>% 
  mutate(prev_yr = prev_yr + 2000) %>% 
  distinct() %>% 
  select(site, year, prev_yr, prev_yr_tmp)
view(tmp.abn.2)

tmp.abn.main <- tmp.abn %>% 
  left_join(tmp.abn.2, by = c("site", "year")) %>% 
  mutate(species_lump = str_replace(species_lump, "Cirolana spp/Gnorimosphaeroma oregonense", "Cirolana spp")) %>% 
  mutate(species_lump = str_replace(species_lump, "Cucumaria/Pseudocnus spp", "Pseudocnus spp")) %>% 
  mutate(species_lump = str_replace(species_lump, "Lirularia/Margarites spp", "Margarites spp")) %>%
  mutate(species_lump = str_replace(species_lump, "Lottia austrodigitalis/digitalis", "Lottia austrodigitalis")) %>%
  mutate(species_lump = str_replace(species_lump, "Lottia paradigitalis/strigatella", "Lottia paradigitalis")) %>%
  mutate(species_lump = str_replace(species_lump, "Lottia scabra/conus", "Lottia scabra")) %>%
  mutate(species_lump = str_replace(species_lump, "Nucella emarginata/ostrina", "Nucella emarginata")) %>%
  mutate(species_lump = str_replace(species_lump, "Pugettia gracilis/richii", "Pugettia gracilis")) %>%
  mutate(species_lump = str_replace(species_lump, "Tegula funebralis/gallina", "Tegula funebralis"))
view(tmp.abn.main)

tmp.abn.nest <- tmp.abn.main %>% 
  nest(dat=-species_lump) %>% 
  mutate(modz=map(dat, ~lm(density_per_m2~max.tmp, data=.x)),
         mod_sum=map(modz, broom::tidy),
         peep=map(modz, broom::glance))

tmp.abn.unnest <- tmp.abn.nest %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% 
  filter(term=="(Intercept)") %>% 
  select(species_lump,intercept=estimate, nobs, r.squared) %>% 
  right_join(tmp.abn.nest, by=c("species_lump")) %>% 
  select(species_lump, mod_sum, intercept, nobs, r.squared) %>% 
  unnest(mod_sum) %>% 
  filter(term=="max.tmp") %>% 
  rename(max.tmp=estimate) %>% 
  select(-term) %>% 
  filter(nobs>3)

write.csv(tmp.abn.unnest, "R outputs/model.results.abundance.temperature.csv", row.names = F)

tmp.abn.prevyr <- tmp.abn.main %>% 
  nest(dat=-species_lump) %>% 
  mutate(modz=map(dat, ~lm(density_per_m2~prev_yr_tmp, data=.x)),
         mod_sum=map(modz, broom::tidy),
         peep=map(modz, broom::glance))

tmp.abn.un.prevyr <- tmp.abn.prevyr %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% 
  filter(term=="(Intercept)") %>% 
  select(species_lump,intercept=estimate, nobs, r.squared) %>% 
  right_join(tmp.abn.prevyr, by=c("species_lump")) %>% 
  select(species_lump, mod_sum, intercept, nobs, r.squared) %>% 
  unnest(mod_sum) %>% 
  filter(term=="prev_yr_tmp") %>% 
  rename(prev_yr_tmp=estimate) %>% 
  select(-term) %>% 
  filter(nobs>3)
  
write.csv(tmp.abn.un.prevyr, "R outputs/model.results.abundance.temperature.prevyr.csv", row.names = F)

sp.list <- tmp.abn.unnest$species_lump



plot.fun.1<-function(xxx, dat) {
  
  dat.sp<-dat %>%
    filter(species_lump==xxx) 
  
  plot.abn<-ggplot(dat.sp, aes(x=max.tmp, y=density_per_m2))+ 
    theme_bw()+theme(panel.grid = element_blank(),
                     strip.background = element_blank(), strip.text = element_text(face="bold"))+
    geom_point(data = (filter(tmp.abn.main, species_lump == xxx)), aes(x=max.tmp, y=density_per_m2), size=1, shape=21, alpha=0.75, fill="grey")+ 
    geom_smooth(method = "lm", show.legend = FALSE, se=F)+
    geom_point(size=2, shape=21, alpha=0.75, fill="red")+
    ggtitle(xxx)+
    labs(x="Yearly Max Temperature", y="Abundance (density per m2)")
  print(plot.abn)
  
   ggsave(paste("R outputs/plots/temp-abundance/", xxx, " tmp.v.abun.tiff",sep=""),
           plot.abn, dpi=150, width=16, height=10, units="cm")
   
}

plot.loop.1<-map(sp.list, ~plot.fun.1(.x,tmp.abn.main))




plot.fun.2<-function(xxx, dat) {
  
  dat.sp<-dat %>%
    filter(species_lump==xxx) 
  
  plot.abn<-ggplot(dat.sp, aes(x=prev_yr_tmp, y=density_per_m2))+ 
    theme_bw()+theme(panel.grid = element_blank(),
                     strip.background = element_blank(), strip.text = element_text(face="bold"))+
    geom_point(data = (filter(tmp.abn.main, species_lump == xxx)), aes(x=prev_yr_tmp, y=density_per_m2), size=1, shape=21, alpha=0.75, fill="grey")+ 
    geom_smooth(method = "lm", show.legend = FALSE, se=F)+
    geom_point(size=2, shape=21, alpha=0.75, fill="red")+
    ggtitle(xxx)+
    labs(x="Max Temperature of Previous Year", y="Abundance (density per m2)")
  print(plot.abn)
  
   ggsave(paste("R outputs/plots/temp-abundance/", xxx, " prevtmp.v.abun.tiff",sep=""),
           plot.abn, dpi=150, width=16, height=10, units="cm")
   
}

plot.loop.2<-map(sp.list, ~plot.fun.2(.x,tmp.abn.main))


