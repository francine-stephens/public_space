#-------------------------------------
# MERGE GREEN SPACE DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/4/21
# LAST UPDATED: 3/5/21
#-------------------------------------

#SET-UP-------------------------------------------------------------------------

## LIBRARIES
packages <- c(
  "tidyverse"
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
  
  
#PREPARE FULL DATASET-----------------------------------------------------------
## JOIN FILES
EVI90_Cbg10 <- EVI_1990_by_cbg10 %>% 
  select(GISJOIN, evi90 = "mean")
EVI00_Cbg10 <- EVI_2000_by_cbg10 %>% 
  select(GISJOIN, evi00 = "mean")
EVI10_Cbg10 <- EVI_2010_by_cbg10 %>% 
  select(GISJOIN, evi10 = "mean")
EVI20_Cbg10 <- EVI_2020_by_cbg10 %>% 
  select(GISJOIN, evi20 = "mean")

EVI_allyears_Cbg10 <- EVI90_Cbg10 %>%
  left_join(., EVI00_Cbg10, by = "GISJOIN") %>%
  left_join(., EVI10_Cbg10, by = "GISJOIN") %>%
  left_join(., EVI20_Cbg10, by = "GISJOIN")


EVI_area_90 <- EVI_1990_by_cbg10_AREA %>%
  select(GISJOIN, area90 = "sum")
EVI_area_00 <- EVI_2000_by_cbg10_AREA %>%
  select(GISJOIN, area00 = "sum")
EVI_area_10 <- EVI_2010_by_cbg10_AREA %>%
  select(GISJOIN, area10 = "sum")

EVI_area_all_years <- EVI_area_90 %>%
  left_join(., EVI_area_00, by = "GISJOIN") %>%
  left_join(., EVI_area_10, by = "GISJOIN") %>%
  mutate_at(vars(starts_with("area")), ~./2589988.1103)
  
  
#EXPORTS------------------------------------------------------------------------
saveRDS(EVI_allyears_Cbg10, "evi_all_years_on_2010cbg.rds")
saveRDS(EVI_area_all_years, "evi_area_all_years_on_2010cbg.rds")
