########################################
#
# Purpose: Download, clean, and plot monthly PRISM temperature data 
#          to eventually evaluate alongside intertidal organism data 
#
# Monthly average minimum and maximum temperature data 
# 2000-2023 taken from PRISM (https://www.prism.oregonstate.edu/).
# 
# 4km pixel resolution 
#
# ALeCompte, August 2024
#######################################


# libraries ---------------------------------------------------------------

library(tidyr)
library(tidyverse)
library(prism)
library(ggplot2)
library(raster)
library(sp)
library(terra)
library(sf)
library(terra)
library(dplyr)

# download data -----------------------------------------------------------

prism_set_dl_dir("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/spatial data/prism_temp_data")

get_prism_monthlys(type = "tmean", year = 2000:2023, mon = 1:12, keepZip = F)
get_prism_monthlys(type = "tmax", year = 2000:2023, mon = 1:12, keepZip = F)
get_prism_monthlys(type = "tmin", year = 2000:2023, mon = 1:12, keepZip = F)

pd_get_name(prism_archive_ls())

# collecting temp values for sites of interest ----------------------------

site_coor <- read.csv("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/site_latitudes.csv") %>% 
  rename(x= longitude, y= latitude, site= marine_site_name) %>% select(x, y)

site_names <- read.csv("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/site_latitudes.csv") %>% 
  rename(x= longitude, y= latitude, site= marine_site_name) %>% mutate(obs = 1:n())

files <- list.files(path = "C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/spatial data/prism_temp_data", 
                    pattern = '*_bil.bil$', all.files = F, full.names = T, recursive = T)

file_stack <- stack(files)

temp_values_df <- extract(file_stack, site_coor, df = T) %>% mutate(obs = 1:n()) #takes a minute to run

temp_val_v2 <- left_join(temp_values_df, site_names, by = "obs")

temp_pvt <- pivot_longer(temp_val_v2,
                         cols = PRISM_tmax_stable_4kmM3_200001_bil:PRISM_tmin_stable_4kmM3_202312_bil,
                         names_to = "filename",
                         values_to = "temp_value") %>% 
  mutate(metric = str_sub(filename, start = 7, end = 10)) %>% 
  mutate(yr = str_sub(filename, start = -8, end = -7)) %>%
  mutate(mon = str_sub(filename, start = -6, end = -5)) %>% 
  mutate(season = case_when(
    mon=="01"~"winter",
    mon=="02"~"winter",
    mon=="03"~"winter",
    mon=="04"~"spring",
    mon=="05"~"spring",
    mon=="06"~"spring",
    mon=="07"~"summer",
    mon=="08"~"summer",
    mon=="09"~"summer",
    mon=="10"~"fall",
    mon=="11"~"fall",
    mon=="12"~"fall")) %>% 
  as_tibble() %>% 
  subset(select = -c(1,2,6))

nans <- temp_pvt %>% 
  filter(is.na(temp_value))
# [1] "Babencho Grande"       
# [2] "El Nido"               
# [3] "El Tivo"               
# [4] "La Bufadora"           
# [5] "La Chorera"            
# [6] "La Cueva"              
# [7] "La Plana"              
# [8] "Mussel Flat Farallones"
# [9] "Punta Abreojos"        
# [10] "Punta Baja"            
# [11] "Punta Rompiente"       
# [12] "Punta San Roque"       
# [13] "Punta Libertad"  

# create boxplots loop ------------------------------------------------
output_path <- "C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/R outputs/plots/temperature boxplots"

df.clean <- temp_pvt %>% 
  as_tibble() %>%
  drop_na(temp_value) %>% 
  mutate(metric.2 = case_when(
    metric == "tmax" ~ "max" ,
    metric == "tmea" ~ "mean",
    metric == "tmin" ~ "min"
  )) %>% 
  mutate(region = case_when(
    y >=46 ~ "Washington" ,
    y >=43 & y <46 ~ "Northern Oregon",
    y >=41 & y <43 ~ "Southern Oregon",
    y >=37 & y <41 ~ "Northern California",
    y >=34 & y <37 ~ "Central California",
    y >=32 & y <34 ~ "Southern California",
  ))

# nans.coords <- df.clean %>% 
#   filter(is.na(region))

marine_sites <- unique(df.clean$site)

### for testing loop
data = df.clean
sites = "Alcatraz"
###

run.1 <- function(sites, data){

  seasonal.boxplot <- data %>%
    filter(site == sites) %>% 
    group_by(metric, yr, season) %>%
    mutate(temp_mean = mean(temp_value)) %>% 
    ungroup() %>% 
    ggplot()+
    geom_boxplot(aes(y=temp_value, x=yr))+
    geom_point(aes(y=temp_mean, x=yr, color="#d73027"), show.legend = FALSE)+
    xlab("year") + ylab("temp deg C")+
    facet_grid(metric.2 ~ season)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(size = 7))+
    scale_x_discrete(breaks=c("00","03","06","09","12","15","18","21"))+
    labs(title = paste(sites),
         subtitle = data$region)
  seasonal.boxplot
  ggsave(file = paste(output_path, "/", sites, "_seasonal_boxplot.tiff", sep = ""), 
         dpi = 150, seasonal.boxplot)
  
  annual.boxplot <- data %>%
    filter(site == sites) %>%
    group_by(metric, yr) %>%
    mutate(temp_mean = mean(temp_value)) %>% 
    ungroup() %>% 
    ggplot()+
    geom_boxplot(aes(y=temp_value, x=yr))+
    geom_point(aes(y=temp_mean, x=yr, color="#d73027"), show.legend = FALSE)+
    xlab("year") + ylab("temp deg C")+
    facet_grid(~metric.2)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(size = 7))+
    scale_x_discrete(breaks=c("00","03","06","09","12","15","18","21"))+
  labs(title = paste(sites),
       subtitle = data$region)
  annual.boxplot
  ggsave(file = paste(output_path, "/", sites, "_annual_boxplot.tiff", sep = ""), 
         dpi = 150, annual.boxplot)
  
  monthly.boxplot <- data %>% 
    filter(site == sites) %>% 
    group_by(metric, mon) %>%
    mutate(temp_mean = mean(temp_value)) %>% 
    ungroup() %>%
    ggplot()+
    geom_boxplot(aes(y=temp_value, x=mon))+
    geom_point(aes(y=temp_mean, x=mon, color="#d73027"), show.legend = FALSE)+
    xlab("month") + ylab("temp deg C")+
    facet_grid(~metric.2)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          strip.background = element_blank())+
    labs(title = paste(sites),
         subtitle = data$region)
  monthly.boxplot
  ggsave(file = paste(output_path, "/", sites, "_monthly_boxplot.tiff", sep = ""), 
         dpi = 150, monthly.boxplot)

}

xx<-purrr::map(.x=marine_sites, ~run.1(.x, df.clean))

