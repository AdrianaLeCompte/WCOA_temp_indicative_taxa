library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(readr)
setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa")

#################### importing biodiversity data
point_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "point_contact_summary_data")
quadrat_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "quadrat_summary_data")
swath_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "swath_summary_data")

#################### pt 1 set up data 
tbl_1 <- quadrat_data %>% 
  select(species_lump, year, marine_site_name, latitude, total_count) %>% 
  filter(total_count > 0)

quadrat_means <- tbl_1 %>% #all taxa abundances by year and location
  group_by(species_lump, year) %>% mutate(sum_count=sum(total_count)) %>%  #calculate total abundance in given year for each taxon across all sites
  ungroup() %>% 
  mutate(wt_lat = latitude*(total_count/sum_count)) %>% 
  group_by(species_lump, year) %>% 
  summarize(x_lat=mean(latitude), #average latitude per year of a given taxon across all sites
            x_abun=mean(total_count), #average abundance 
            max_lat=max(latitude), #max and min latitude
            min_lat=min(latitude), 
            x_wtlat=sum(wt_lat)) %>% #sum of the site-specific abundance-weighted latitude
  ungroup() %>% filter(x_abun > 0) %>% 
  group_by(species_lump) %>% 
  mutate(frequency = length(year),
         time_span=(max(year)-min(year))) %>% 
  ungroup()
  
#################### pt 2 run linear model, clean up results
all.infauna.lat.plots <- quadrat_means %>% 
  filter(frequency >=5) %>% #removing taxa that were observed less than 10 times
  nest(dat=c(-species_lump, -time_span)) %>% #nesting data by unit of analysis - e.g., taxon and depth zone
  mutate(modz=map(dat, ~lm(x_wtlat~year, data=.x)), #"nested" simple linear models for each unit of analysis
         mod_sum=map(modz, broom::tidy), #nested model terms for each regression - intercept, slope, SE, pvalue, etc
         peep=map(modz, broom::glance)) #more nested model characteristics - AIC, sample size, df, r-square

pita <- all.infauna.lat.plots %>% 
  unnest(c(peep, mod_sum), names_repair="universal") %>% #unnesting model terms and tidying their names
  filter(term=="(Intercept)") %>% #isolating the intercept term from model summary
  select(species_lump,time_span,intercept=estimate, r.squared) %>% #retaining important info taxon names, depthzone, the intercept value (and calling it intercept), and model r-squared
  right_join(all.infauna.lat.plots,., by=c("species_lump")) %>% #rejoining the same model outputs to extract and attach model slope information
  select(species_lump, mod_sum, intercept, r.squared) %>% #keeping variables of interest
  unnest(mod_sum) %>% filter(term=="year") %>% #extracting the information pertaining to the slope of the regression (i.e., term=year)
  rename(Year_est=estimate) %>% select(-term) %>% #renaming slope value to be "Year_est"
  filter(p.value<=0.1) %>% #selecting only "significant" taxa,i.e. those that showed statistically significant migration across the dataset
  mutate(direction=if_else(Year_est>0, "Northward","Southward")) #characterizing the nature of the migration
         # flag=if_else(species_lump%in%c("Eyakia robusta", "Onuphis iridescens", "Edwardsiidae", "Onuphis sp A", "Urothoe elegans Cmplx"), 1,0))# Taxa that needed to be flagged for "bad names"

max.lat.shifts <- quadrat_means %>% 
  left_join(select(pita, species_lump, direction),., by=c("species_lump"))%>% #filtering for only the significant taxa
  group_by(species_lump, direction) %>% 
  summarise(shift=(max(x_lat)-min(x_lat))) %>% arrange(desc(shift)) %>% #calculating the range of shift
  ungroup()

#################### pt 3 plot data
take3.plot<-function(xxx, dat) {
  dat.tax<-dat %>% inner_join(.,pita, by=c("species_lump")) %>% #filtering for the north-south taxa
    filter(species_lump==xxx)#selecting an individual taxon for graphing
  
  plot.tax<-ggplot(dat.tax, aes(x=year, y=x_wtlat))+ #fill=DepthZone, color=DepthZone
    theme_bw()+theme(panel.grid = element_blank(),
                     axis.text.y=element_text(face=c("bold", "plain","bold", "bold","plain", "bold","plain", "plain")),#bolding specific points on the y-axis that correspond to the scale y continuous
                     strip.background = element_blank(), strip.text = element_text(face="bold"))+
    geom_point(data = "annual data", (aes(x=year, y="x_wtlat")), size=2, shape=21, alpha=0.75, fill="grey")+ #rename df latitude to match previous aes statemtn
    geom_smooth(method = "lm", show.legend = FALSE, se=F)+
    geom_point(size=2, shape=21, alpha=0.75, fill="red")+
    ggtitle(xxx)+
    labs(x="year", y="Abundance Weighted Latitude")+ 
    facet_wrap("direction", nrow=2, ncol=5)
  print(plot.tax)
  
  # ggsave(paste("R outputs/plots", xxx, " mean weighted lat-year-2.tiff",sep=""),
  #        plot.tax, dpi=150, width=16, height=10, units="cm")
}


species<-unique(pita$species_lump) #vector of species names to feed to the function

hummus.2<-map(species, ~take3.plot(.x,quadrat_means)) #iterating the function across the different north/south species












