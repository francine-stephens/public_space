#-------------------------------------
# MERGE CENSUS DEMOGRAPHICS DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/4/21
# LAST UPDATED: 3/14/21
#-------------------------------------

#SET-UP-------------------------------------------------------------------------

## LIBRARIES
packages <- c(
  "tidyverse"
)
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Projects/public_space/census")
wd <- getwd()


## IMPORT DATASETS
race10 <- readRDS(paste0(wd,
                         "/race_10_on_10_cbg.rds"))
race00 <- readRDS(paste0(wd,
                         "/race_00_on_10_cbg.rds"))
race90 <- readRDS(paste0(wd,
                         "/race_90_on_10_cbg.rds"))

#PREPARE KEY VARIABLE-----------------------------------------------------------
## 1990 to 2010 GISJOIN in the NHGIS CW has an incorrect number of characters.
## To fix, remove the 3rd character (a padded zero on the county identifier) & 
## the 7th character (a padded zero on the tract identifier).
race90_edited <- race90 %>%
  mutate(CBG_10 = str_replace_all(CBG_10, "G", ""),
         state = str_sub(CBG_10, end=2),
         county = str_sub(CBG_10, start=4, end=6),
         tract = str_sub(CBG_10, start=8, end=13),
         cbg = str_sub(CBG_10, start=-1),
         GEOID = str_c(state, county, tract, cbg, sep="")) %>%
  select(-CBG_10, -state:-cbg)


#JOIN ALL DATASETS--------------------------------------------------------------
race_all_decades_cbg10 <- race10 %>%
  left_join(., race00, by = "CBG_10") %>%
  left_join(., race90_edited, by=c("CBG_10"="GEOID"))

# EXPORT
saveRDS(race_all_decades_cbg10, "all_decades_race_on_cbg10.rds")
