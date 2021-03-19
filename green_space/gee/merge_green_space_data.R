#-------------------------------------
# MERGE GREEN SPACE DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/4/21
# LAST UPDATED: 3/19/21
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
## JOIN FILES
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


EVI_area_90 <- EVI_1990_by_cbg10_AREA %>%
  select(GISJOIN, area_1990 = "sum")
EVI_area_00 <- EVI_2000_by_cbg10_AREA %>%
  select(GISJOIN, area_2000 = "sum")
EVI_area_10 <- EVI_2010_by_cbg10_AREA %>%
  select(GISJOIN, area_2010 = "sum")

EVI_area_all_years <- EVI_area_90 %>%
  left_join(., EVI_area_00, by = "GISJOIN") %>%
  left_join(., EVI_area_10, by = "GISJOIN") %>%
  mutate_at(vars(starts_with("area")), ~./2589988.1103) %>% 
  long_panel(.,
             prefix = "_",
             id = "GISJOIN",
             periods = c(1990, 2000, 2010),
             label_location = "end")
  
  
# EXPORTS-----------------------------------------------------------------------
saveRDS(EVI_allyears_Cbg10, "evi_all_years_on_2010cbg_LONG.rds")
saveRDS(EVI_area_all_years, "evi_area_all_years_on_2010cbg_LONG.rds")
