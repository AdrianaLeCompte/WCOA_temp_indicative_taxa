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

prism_set_dl_dir("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/spatial data/prism_temp_data")

get_prism_monthlys(type = "tmean", year = 2000:2023, mon = 1:12, keepZip = F)
get_prism_monthlys(type = "tmax", year = 2000:2023, mon = 1:12, keepZip = F)
get_prism_monthlys(type = "tmin", year = 2000:2023, mon = 1:12, keepZip = F)

pd_get_name(prism_archive_ls())

# defining example site
lajolla <- c(-117.2812, 32.8432312)

# prism_archive_subset() will return prism data that matches the specified 
# variable, time step, years, months, days, etc.
to_slice <- prism_archive_subset("tmean", "monthly", mon = 1)
p <- pd_plot_slice(to_slice, lajolla) 


# plot linear average
plot_test <- p + 
  stat_smooth(method="lm", se = FALSE) + 
  theme_bw() + 
  ggtitle("Average January air temperature in La Jolla Caves 2000-2023")
plot_test

ggsave(paste("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/R outputs/plots/tmean_jan_lajolla.tiff"),
       plot_test, dpi=150, width=17, height=13, units="cm")


####
int_sites <- read.csv("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/site_latitudes.csv") %>% 
  rename(x= longitude, y= latitude, site= marine_site_name) %>% select(x, y)

temp_metr <- c("tmean", "tmin", "tmax")

loop.test <- function(metrics, sites){
  
   slice_month_i <-  for (i in 1:12){
    slice_month <- prism_archive_subset(metrics, "monthly", mon = i)
  }
  
  slice_site <- pd_plot_slice(slice_month_i, sites)
  
  plot_test <- slice_site + 
    stat_smooth(method="lm", se = FALSE) + 
    theme_bw() + 
    ggtitle(paste(metrics, "air temperature in", sites,"2000-2023"))
  
  print(paste(metrics, sites))
  
}

run <- loop.test(temp_metr, int_sites)

slice_month_i <-  for (i in 1:12){
  slice_month <- prism_archive_subset("tmean", "monthly", mon = i)
  slice_site <- pd_plot_slice(slice_month, lajolla)
  plot_site <- slice_site +
    stat_smooth(method="lm", se = FALSE) + 
    theme_bw() + 
    ggtitle(paste("Month", i ,"average air temperature in la jolla 2000-2023"))
  print(plot_site)
}

files <- list.files(path = "C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/spatial data/prism_temp_data", 
                    pattern = '*_bil.bil$', all.files = F, full.names = T, recursive = T)

tbl_test <- tibble(folders = list.files(path = "C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/spatial data/prism_temp_data", 
                                        # pattern = "PRISM_tmax_stable_4kmM3_200001_bil",
                                        all.files = F, full.names = F)) %>% mutate(dates = str_sub(folders, start = 25, end = 30))

s <- stack(files)

q <- pd_stack(prism_archive_subset("tmean", "monthly", mon = 1:12))

t <- stackApply(q, 1:12)

ca_boundaries <- sf::read_sf("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/spatial data/ca_state_border/CA_State.shp")

plot(st_geometry(ca_boundaries))

crs(ca_boundaries, parse = T)

ca_nad83 <- ca_boundaries %>% st_transform(crs=4269)

raster.1 <- rast("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/raw data (MARINe)/spatial data/prism_temp_data/PRISM_tmax_stable_4kmM3_200001_bil/PRISM_tmax_stable_4kmM3_200001_bil.bil")

raster_crop <- terra::crop(raster.1, extent(ca_nad83))

terra::plot(raster_crop)

file_stack <- stack(files)

temp_values_df <- extract(file_stack, int_sites)

