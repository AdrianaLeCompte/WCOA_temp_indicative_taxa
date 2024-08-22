library(tidyverse)
library(sf)
library(prism)
library(rgdal)
library(elevatr)
library(raster)
library(exactextractr)

xwalk_df<-read_csv("Data/master_site_class_xwalk_030723_coordinates_REGIONS.csv") %>%
  mutate(Region_detail2 = case_when(Region_detail %in% c("GP_C","GP_N","GP_S","GP_U")~"GP",
                                    T~Region_detail) %>%
           factor(levels=c("AW","WM","GP","NE","SE", "CB")),
         Class=factor(Class, levels=c("P","I","E","U")))

gis_gp<-read_csv("Data/GISmetrics/gis_metrics_GP.csv") %>% dplyr::select(-`...1`,-ParentGlobalID, -ColdWetMonths, -ColdMonths, -WetMonths, -SnowDom_PptTemp)
gis_nese<-read_csv("Data/GISmetrics/gis_metrics_NESE_withcaribbean.csv") %>% dplyr::select(-ParentGlobalID, -BFI)
gis_wm<-read_csv("Data/GISmetrics/gis_metrics_WM.csv") %>% dplyr::select(-`...1`, -starts_with("Eco"), -ColdWetMonths, -ColdMonths, -WetMonths, -SnowDom_PptTemp, -SnowDom_Eco)

# identical(names(gis_gp), names(gis_nese))
# identical(names(gis_nese),names(gis_gp))
# identical(names(gis_nese),names(gis_wm))
# identical(names(gis_wm),names(gis_nese))

gis_already<-bind_rows(gis_wm, gis_gp, gis_nese) %>%
  filter(SiteCode %in% xwalk_df$sitecode) %>%
  group_by(SiteCode) %>%
  slice_head(n=1) 
  

setdiff(xwalk_df$sitecode, gis_already$SiteCode)
need_gis_df<-xwalk_df %>%
  filter(!sitecode %in% gis_already$SiteCode)
need_gis_df[duplicated(need_gis_df$sitecode),]
#####

mydf_sf<-st_as_sf(need_gis_df,
                  coords=c("long","lat"),
                  remove=F,
                  crs=4326)


prism_set_dl_dir("~/prism")


# download 30-year normals of annual statistics 
# First run will take longer because data is getting downloaded.
get_prism_normals(type="ppt", resolution="800m", keepZip = F, annual = T)
get_prism_normals(type="tmean", resolution="800m", keepZip = F, annual = T)
get_prism_normals(type="tmax", resolution="800m", keepZip = F, annual = T)
get_prism_normals(type="tmin", resolution="800m", keepZip = F, annual = T)
#download 30-year normals of monthly statistics, ppt only
get_prism_normals(type="ppt", resolution="800m", keepZip = F, mon=1:12)

#Get mean temps for identifying cold months
get_prism_normals(type="tmean", resolution="800m", keepZip = F, mon=1:12)


#Turn them into rasters with projection info
ppt_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_annual_bil")
proj4string(ppt_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

tmax_RS<-pd_stack("PRISM_tmax_30yr_normal_800mM2_annual_bil")
proj4string(tmax_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

tmean_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_annual_bil")
proj4string(tmean_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

tmin_RS<-pd_stack("PRISM_tmin_30yr_normal_800mM2_annual_bil")
proj4string(tmin_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


ppt.m01_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_01_bil")
proj4string(ppt.m01_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m02_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_02_bil")
proj4string(ppt.m02_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m03_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_03_bil")
proj4string(ppt.m03_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m04_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_04_bil")
proj4string(ppt.m04_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m05_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_05_bil")
proj4string(ppt.m05_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m06_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_06_bil")
proj4string(ppt.m06_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m07_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_07_bil")
proj4string(ppt.m07_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m08_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_08_bil")
proj4string(ppt.m08_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m09_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_09_bil")
proj4string(ppt.m09_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m10_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_10_bil")
proj4string(ppt.m10_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m11_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_11_bil")
proj4string(ppt.m11_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m12_RS<-pd_stack("PRISM_ppt_30yr_normal_800mM2_12_bil")
proj4string(ppt.m12_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


temp.m01_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_01_bil")
proj4string(temp.m01_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m02_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_02_bil")
proj4string(temp.m02_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m03_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_03_bil")
proj4string(temp.m03_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m04_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_04_bil")
proj4string(temp.m04_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m05_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_05_bil")
proj4string(temp.m05_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m06_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_06_bil")
proj4string(temp.m06_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m07_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_07_bil")
proj4string(temp.m07_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m08_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_08_bil")
proj4string(temp.m08_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m09_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_09_bil")
proj4string(temp.m09_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m10_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_10_bil")
proj4string(temp.m10_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m11_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_11_bil")
proj4string(temp.m11_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
temp.m12_RS<-pd_stack("PRISM_tmean_30yr_normal_800mM2_12_bil")
proj4string(temp.m12_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


mydf_prism<-data.frame(SiteCode=mydf_sf$sitecode) %>% 
  cbind(
    raster::extract(tmean_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(tmean = 1),
    raster::extract(tmax_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(tmax = 1),
    raster::extract(tmin_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(tmin = 1),
    raster::extract(ppt_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt = 1),
    
    raster::extract(ppt.m01_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m01 = 1),
    raster::extract(ppt.m02_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m02 = 1),
    raster::extract(ppt.m03_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m03 = 1),
    raster::extract(ppt.m04_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m04 = 1),
    raster::extract(ppt.m05_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m05 = 1),
    raster::extract(ppt.m06_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m06 = 1),
    raster::extract(ppt.m07_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m07 = 1),
    raster::extract(ppt.m08_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m08 = 1),
    raster::extract(ppt.m09_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m09 = 1),
    raster::extract(ppt.m10_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m10 = 1),
    raster::extract(ppt.m11_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m11 = 1),
    raster::extract(ppt.m12_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m12 = 1),
    
    raster::extract(temp.m01_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m01 = 1),
    raster::extract(temp.m02_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m02 = 1),
    raster::extract(temp.m03_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m03 = 1),
    raster::extract(temp.m04_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m04 = 1),
    raster::extract(temp.m05_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m05 = 1),
    raster::extract(temp.m06_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m06 = 1),
    raster::extract(temp.m07_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m07 = 1),
    raster::extract(temp.m08_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m08 = 1),
    raster::extract(temp.m09_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m09 = 1),
    raster::extract(temp.m10_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m10 = 1),
    raster::extract(temp.m11_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m11 = 1),
    raster::extract(temp.m12_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(temp.m12 = 1)
  )

#### ELEVATR to get site elevations ####


mydf_elev<-get_elev_point(mydf_sf, units="meters", src="aws") #Failed if I didn't specify src="aws" 

mydf_prism <- mydf_prism %>%
  inner_join(mydf_elev %>% 
               as.data.frame() %>%
               dplyr::select(SiteCode=sitecode, # REMOVE PARENTGLOBALID WHEN XWALK IS FIXED 
                             Elev_m=elevation) 
  )

#### Snow persistence, following Hammond et al. 2020 ####
#First, make a buffer of each site
mydf_sf_10km<-mydf_sf %>%     st_transform(crs=32611) %>%  st_buffer(dist=10000) #buffer radius in meters
mydf_sf_5km<-mydf_sf %>%    st_transform(crs=32611) %>% st_buffer(dist=5000) #buffer radius in meters
mydf_sf_1km<-mydf_sf %>%   st_transform(crs=32611) %>%  st_buffer(dist=1000) #buffer radius in meters

#This is a single-year raster, but will be replaced with a 20-y average raster
str_name<-'NotForGit/Shapefiles/AverageSnow_US/mod10a2_sci_AVG_v2.tif' # FULL US LOWER 48 TIFF 
snowp_raster<-raster::raster(str_name)

# library(exactextractr)
mydf_sf_10km$MeanSnowPersistence_10<-exactextractr::exact_extract(x=snowp_raster, y=mydf_sf_10km, 'mean')
mydf_sf_5km$MeanSnowPersistence_05<-exactextractr::exact_extract(x=snowp_raster, y=mydf_sf_5km, 'mean')
mydf_sf_1km$MeanSnowPersistence_01<-exactextractr::exact_extract(x=snowp_raster, y=mydf_sf_1km, 'mean')

mydf_sf_snow_combined<-mydf_sf_10km %>%
  inner_join(mydf_sf_5km %>% 
               as.data.frame() %>%
               dplyr::select(sitecode,  MeanSnowPersistence_05)) %>% # CHANGE PARENTGLOBALID TO SITECODE. FIX!
  inner_join(mydf_sf_1km %>% 
               as.data.frame() %>%
               dplyr::select(sitecode, MeanSnowPersistence_01)) # CHANGE PARENTGLOBALID TO SITECODE. FIX!



gis_newmetrics_df<-mydf_prism  %>% 
  # inner_join(ColdWetMonths) %>%
  inner_join(mydf_sf_snow_combined %>%
               as.data.frame() %>%
               dplyr::select(SiteCode=sitecode, MeanSnowPersistence_10, MeanSnowPersistence_05, MeanSnowPersistence_01) # CHANGE PARENTGLOBALID TO SITECODE. FIX!
  ) %>%
  mutate(SnowDom_SP10 = case_when(MeanSnowPersistence_10<25~"Not snow-dominated",
                                  MeanSnowPersistence_10>=25~"Snow-dominated",
                                  T~NA_character_),
         SnowDom_SP05 = case_when(MeanSnowPersistence_05<25~"Not snow-dominated", # FIX NA's
                                  T~"Snow-dominated"),
         SnowDom_SP01 = case_when(MeanSnowPersistence_01<25~"Not snow-dominated",
                                  T~"Snow-dominated"))
setdiff(names(gis_already), names(gis_newmetrics_df))
         
gis_metrics_df<-gis_already %>%
  dplyr::select(-CalcTime) %>%
  bind_rows( gis_newmetrics_df) %>%
  mutate(
         ppt.234 = ((ppt.m02 + ppt.m03 + ppt.m04)/3),
         ppt.567 = ((ppt.m05 + ppt.m06 + ppt.m07)/3),
         ppt.8910 = ((ppt.m08 + ppt.m09 + ppt.m10)/3),
         ppt.11121 = ((ppt.m11 + ppt.m12 + ppt.m01)/3),
         temp.234 = ((temp.m02 + temp.m03 + temp.m04)/3),
         temp.567 = ((temp.m05 + temp.m06 + temp.m07)/3),
         temp.8910 = ((temp.m08 + temp.m09 + temp.m10)/3),
         temp.11121 = ((temp.m11 + temp.m12 + temp.m01)/3)
         
         )

skim_without_charts(gis_metrics_df)

write_csv(gis_metrics_df, "Data/GISmetrics/COMPLETE_gis_metrics_df.csv")
detach(package:raster)
