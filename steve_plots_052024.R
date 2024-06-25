library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(readr)
library(vegan)
library(ggrepel)
#setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - General/Temperature indicative taxa") #laptop wd
setwd("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa") #desktop wd

# importing all data ------------------------------------------------------

#### biodiversity data
point_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "point_contact_summary_data")
quadrat_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "quadrat_summary_data")
swath_data <- read_excel("raw data (MARINe)/biodiversity/adriana_lecompte_santiago_cbs_data_20240227.xlsx", sheet = "swath_summary_data")

#### long term data
photoplots_data <- read_csv("raw data (MARINe)/long term monitoring/phototransummarysd_download.csv")
species_code_key <- read_csv("raw data (MARINe)/long term monitoring/marine_lumping_codes_definitions.csv")

# species list exploration ------------------------------------------------

#### species list for each collection type
quad.sp.list <- quadrat_data %>% select(species_lump) %>% distinct() # 127 species
swat.sp.list <- swath_data %>% select(species_lump) %>% distinct() # 11 species
phto.sp.list <- photoplots_data %>% select(species_code) %>% distinct() %>% # 49 species
  left_join(species_code_key, by= c("species_code" = "lumping_code")) %>%
  filter(kingdom == "Animalia") %>% select("lumping_name") %>% 
  rename("species_lump" = "lumping_name")#16 species under animalia

### do species lists intersect?
int <- intersect(quad.sp.list, swat.sp.list) # no overlap, collected by size
int.2 <- intersect(quad.sp.list, phto.sp.list) # only Lottia gigantea in both
int.3 <- intersect(phto.sp.list, swat.sp.list) # only Pisaster ochraceus in both

### filtering to see if data has species duplicates
test <- swath_data %>% #swath data has "survey rep" which seems to have multiple occurrence of species per year/site
  filter(species_lump == "Pisaster ochraceus", year == "2015", marine_site_name == "Alegria")

# clean data --------------------------------------------------------------
quad.dat.clean <- quadrat_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

swat.dat.clean <- swath_data %>% 
  select(species_lump, year, marine_site_name, latitude, density_per_m2) %>% 
  filter(density_per_m2 > 0)

all.bio <- bind_rows(quad.dat.clean, swat.dat.clean) #133 unique species in df

all.bio.col <- bind_rows(quadrat_data, swath_data) %>% 
  filter(density_per_m2 > 0)

latitudes <- all.bio.col %>% select(marine_site_name, latitude, longitude) %>% distinct()
write.csv(latitudes, "raw data (MARINe)/site_latitudes.csv", row.names = F)

phto.data <- photoplots_data %>%
  left_join(species_code_key, by= c("species_code" = "lumping_code")) %>%
  filter(kingdom == "Animalia") %>% 
  select(lumping_name, marine_site_name, marine_common_year, average_percent_cover, site_lat)

# plot 1 - species and years ------------------------------------------------------------------
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

# ggsave(paste("R outputs/plots/plot_1.tiff"), plot.1, dpi=150, width=16, height=10, units="cm")

tbl_1.1 <- photoplots_transects_data %>% 
  left_join(species_code_key, by= c("species_code" = "lumping_code")) %>% 
  filter(kingdom == "Animalia") %>% 
  select(lumping_name, average_percent_cover, marine_common_year) %>% 
  filter(average_percent_cover > 0) %>% select(-average_percent_cover) %>% 
  distinct()

tbl_1.2 <- tbl_1.1 %>% 
  group_by(lumping_name) %>%  
  summarise(num_yrs = n()) %>% 
  ungroup() %>% 
  group_by(num_yrs) %>% 
  summarise(num_sp = n())
  
plot.1.1 <- ggplot(data = tbl_1.2, aes(x = num_yrs, y = num_sp))+
  geom_col()+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank())+
  labs(x="number of years", y="number of species")
print(plot.1.1)

# ggsave(paste("R outputs/plots/plot_1.1.tiff"), plot.1.1, dpi=150, width=16, height=10, units="cm")

# shift direction  ------------------------------------------------------------------
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



# thermal tolerance -------------------------------------------------------
GlobalTherm <- read_excel("raw data (MARINe)/globaltherm/GlobalTherm_upload_10_11_17.xlsx")

GlobalTherm.cln <-  GlobalTherm %>%
  mutate(species_lump = paste(Genus,Species)) %>% 
  select(species_lump, Tmax, tmin)

therm.check <- left_join(all.bio.1, GlobalTherm.cln, by = "species_lump") %>% 
  select(-year) %>% distinct() %>% na.omit() # purple shore crab is the only species listed in this db

# plot 2 - sites and years ------------------------------------------------
sites.yrs <- bind_rows(quad.dat.clean, swat.dat.clean) %>% #ran these line without filtering for abun > 0
  select(marine_site_name, year) %>% distinct() %>% 
  count(year, sort = T) 

plot.2 <- ggplot(data = sites.yrs, aes(x = year, y = n))+
  geom_col()+ 
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(labels = sites.yrs$year, breaks = sites.yrs$year)+
  labs(y="# of sites visited")
print(plot.2)

# ggsave(paste("R outputs/plots/plot_2.tiff"),
#         plot.3, dpi=150, width=17, height=13, units="cm")


# plot 3 - how many sites per year (each species) -------------------------

all.bio.cln <- all.bio %>% 
  mutate(species_lump = recode(species_lump, 
                               "Cirolana spp/Gnorimosphaeroma oregonense" = "Cirolana spp",
                               "Cucumaria/Pseudocnus spp" = "Pseudocnus spp",
                               "Lirularia/Margarites spp" = "Margarites spp",
                               "Lottia austrodigitalis/digitalis" = "Lottia austrodigitalis",
                               "Lottia scabra/conus" = "Lottia scabra",
                               "Nucella emarginata/ostrina" = "Nucella emarginata",
                               "Pugettia gracilis/richii" = "Pugettia gracilis",
                               "Tegula funebralis/gallina" = "Tegula funebralis",
                               "Lottia paradigitalis/strigatella" = "Lottia paradigitalis")) %>% 
  group_by(species_lump, year) %>% 
  summarise(sites=n()) %>%  
  filter(n()>4) %>% 
  ungroup()

sp.list.2 <- unique(all.bio.cln$species_lump)

phto.dat.cln <- phto.data %>% 
  filter(average_percent_cover > 0.0) %>% 
  mutate(lumping_name = recode(lumping_name,
                               "Anthopleura elegantissima/sola" = "Anthopleura elegantissima",
                               "Chthamalus spp/Balanus glandula" = "Chthamalus spp",
                               "Phragmatopoma/Sabellaria spp" = "Sabellaria spp",
                               "Septifer bifurcatus; Brachidontes adamsianus" = "Septifer bifurcatus")) %>% 
           group_by(lumping_name, marine_common_year) %>% 
           summarise(sites=n()) %>%  
           filter(n()>4) %>% 
           ungroup()

plot_3_fnctn <- function(xxx, dat){
  x1 <- dat %>%
    filter(species_lump==xxx)
  y1 <- ggplot(data = x1, aes(x = year, y = sites))+
    geom_col()+ 
    scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40))+
    scale_x_continuous(breaks = seq(2000, 2023, 1))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = xxx)
  print(y1)
  ggsave(file= paste("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/R outputs/plots/species plots/",xxx,"plot.tiff", sep="_"), dpi=150, y1)
}

candle <- map(sp.list.2, ~plot_3_fnctn(.x,all.bio.cln))

# plot 4 - range extensions -----------------------------------------------

all.bio.lat <- all.bio %>% select(-density_per_m2) %>% distinct()
phto.data.lat <- phto.data %>% select(-average_percent_cover) %>% distinct()

quad.dat.lat <- quad.dat.summ %>% select(species_lump, year, max_lat, min_lat, x_wtlat) %>% 
  pivot_longer(cols = max_lat:x_wtlat,
               names_to = "latitude_type",
               values_to = "latitude")

list.1 <- unique(quad.dat.lat$species_lump)

plot_4_fnctn <- function(list, data){
  w1 <- data %>% 
    filter(species_lump==list)
  w2 <- ggplot(data = w1, aes(x = year, y = latitude, color = latitude_type))+
    geom_point()+
    geom_smooth(method = "lm", show.legend = FALSE, se=F)+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(limits = c(25, 45))+
    scale_x_continuous(breaks = seq(2000, 2023, 1), limits = c(2000, 2023))+
    labs(title = list)
  print(w2)
  ggsave(file= paste("C:/Users/adrianal/SCCWRP/Ocean Health Report Cards - Temperature indicative taxa/R outputs/plots/latitude plots/",list,"wtlat_plot.tiff", sep="_"), dpi=150, w2)
}
               
pencil <- map(list.1, ~plot_4_fnctn(.x,quad.dat.lat))


# ordination -------------------------------------------------------------
codes <- read_excel("raw data (MARINe)/sitename_codes.xlsx")
regions <- read_excel("raw data (MARINe)/site_regions.xls")

codes_reg <- left_join(codes, regions, by = 'marine_site_name') %>% select(-OBJECTID)

benthic_df_pa <- benthic_df %>% 
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult_sum = sum(BAResult)) %>% 
  ungroup() %>% 
  mutate(p_a = 1) %>%
  select(-BAResult_sum) %>% 
  pivot_wider(names_from = FinalID, values_from = p_a, values_fill = 0) %>% 
  column_to_rownames("SampleID")

m1 <- all.bio %>% 
  left_join(codes, by = "marine_site_name") %>% 
  mutate(SampleID = paste(sitecode, year, sep = "_")) %>% 
  filter(density_per_m2 > 0) %>% 
  group_by(SampleID, species_lump) %>% 
  summarize(density_sum = sum(density_per_m2)) %>% 
  ungroup() %>% 
  mutate(p_a = 1) %>%
  select(-density_sum) %>% 
  pivot_wider(names_from = species_lump, values_from = p_a, values_fill = 0) %>% 
  column_to_rownames("SampleID")

m1.ord <- metaMDS(m1, k = 2, try = 1000, trymax = 10000, autotransform = F)
m1.stress = m1.ord$stress
## stress from ordination run with density per m2 values = 4.909649e-05
## stress from ordination run with presence absence values = 4.435782e-05

m1.scores <- data.frame(scores(m1.ord, display = "sites")) %>% 
  rownames_to_column("SampleID") %>% 
  mutate(stationcode = str_sub(SampleID,1,3), sam_year = str_sub(SampleID,5,8)) %>% 
  select(-SampleID) %>% 
  filter(stationcode != "PUL") %>% 
  left_join(codes_reg, by = c("stationcode" = "sitecode")) %>% 
  mutate(yr_cond = str_sub(sam_year,3,4))

m1.ord.plot <- m1.scores %>% 
  ggplot(., aes(x = NMDS1, y = NMDS2)) +
  theme_bw()+ theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                    axis.title = element_text(face = "bold"))+
  geom_text(aes(label = yr_cond, color = region, fontface = "bold"), pch = 21, size = 3.5)+
  scale_fill_discrete(name = "region")+
  scale_colour_manual(values= c("#999999", "#CC79A7", "#0072B2", "#009E73"))+
  labs(title = 'Biodiversity data, all sites all years',
        subtitle = 'presence absence')
m1.ord.plot




