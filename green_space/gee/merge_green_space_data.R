#-------------------------------------
# MERGE GREEN SPACE DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/4/21
# LAST UPDATED: 5/20/21
#-------------------------------------

# SET-UP------------------------------------------------------------------------

## LIBRARIES
packages <- c(
  "tidyverse",
  "panelr"
)
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Projects/public_space/green_space/gee")
wd <- getwd()

# import green space
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
  
  
# PREPARE FULL DATASET----------------------------------------------------------

## EVI SCORES LANDSAT
EVI90_Cbg10 <- EVI_1990_by_cbg10 %>% 
  select(GISJOIN, evi_1990 = "mean")
EVI00_Cbg10 <- EVI_2000_by_cbg10 %>% 
  select(GISJOIN, evi_2000 = "mean")
EVI10_Cbg10 <- EVI_2010_by_cbg10 %>% 
  select(GISJOIN, evi_2010 = "mean")
EVI20_Cbg10 <- EVI_2020_by_cbg10 %>% 
  select(GISJOIN, evi_2020 = "mean")

EVI_allyears_Cbg10 <- EVI90_Cbg10 %>%
  left_join(., EVI00_Cbg10, by = "GISJOIN") %>%
  left_join(., EVI10_Cbg10, by = "GISJOIN") %>%
  left_join(., EVI20_Cbg10, by = "GISJOIN") %>% 
  long_panel(.,
             prefix = "_",
             id = "GISJOIN",
             periods = c(1990, 2000, 2010, 2020),
             label_location = "end")

## EVI THRESHOLD AREA LANDSAT
EVI_area_90 <- EVI_1990_by_cbg10_AREA %>%
  select(GISJOIN, area_1990 = "sum")
EVI_area_00 <- EVI_2000_by_cbg10_AREA %>%
  select(GISJOIN, area_2000 = "sum")
EVI_area_10 <- EVI_2010_by_cbg10_AREA %>%
  select(GISJOIN, area_2010 = "sum")
EVI_area_20 <- EVI_2020_by_cbg10_AREA %>%
  select(GISJOIN, area_2020 = "sum")

EVI_area_all_years <- EVI_area_90 %>%
  left_join(., EVI_area_00, by = "GISJOIN") %>%
  left_join(., EVI_area_10, by = "GISJOIN") %>% 
  left_join(., EVI_area_20, by = "GISJOIN") %>%
  mutate_at(vars(starts_with("area")), ~./2589988.1103) %>% 
  long_panel(.,
             prefix = "_",
             id = "GISJOIN",
             periods = c(1990, 2000, 2010, 2020),
             label_location = "end")
  
## MODIS
select_modis_vars <- function(x) { 
  x %>%       
    select(GISJOIN, median)
}

pct_veg_2000 <- median_pct_veg_2000_by_cbg  %>%
  select_modis_vars(.) %>%
  rename(med_pct_veg_2000 = "median")
pct_veg_2010 <- median_pct_veg_2010_by_cbg  %>%
  select_modis_vars(.) %>%
  rename(med_pct_veg_2010 = "median")
pct_veg_2020 <- median_pct_veg_2018_by_cbg %>%
  select_modis_vars(.) %>%
  rename(med_pct_veg_2020 = "median")


wide_med_pct_veg <- pct_veg_2000 %>%
  left_join(., pct_veg_2010, by = "GISJOIN") %>% 
  left_join(., pct_veg_2020, by = "GISJOIN") %>% 
  mutate(veg_diff_00to10 = med_pct_veg_2010 - med_pct_veg_2000,
         veg_diff_10to20 = med_pct_veg_2020 - med_pct_veg_2010,
         veg_pctchg_00to10 = (veg_diff_00to10/med_pct_veg_2000) * 100,
         veg_pctchg_10to20 = (veg_diff_10to20/med_pct_veg_2010) * 100) %>%
  mutate(across(starts_with("veg_pctchg_"), ~na_if(., Inf)))
  

long_med_pct_veg <- wide_med_pct_veg %>% 
  select(GISJOIN:med_pct_veg_2020) %>%
  long_panel(.,
             prefix = "_",
             id = "GISJOIN",
             periods = c(2000, 2010, 2020),
             label_location = "end")


# EXPORTS-----------------------------------------------------------------------
saveRDS(EVI_allyears_Cbg10, "evi_all_years_on_2010cbg_LONG.rds")
saveRDS(EVI_area_all_years, "evi_area_all_years_on_2010cbg_LONG.rds")

saveRDS(wide_med_pct_veg, "vegetation_median_pct_WIDE.rds")
saveRDS(long_med_pct_veg, "vegetation_median_pct_LONG.rds")
