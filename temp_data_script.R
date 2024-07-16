install.packages("prism")
library(prism)
library(ggplot2)

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

