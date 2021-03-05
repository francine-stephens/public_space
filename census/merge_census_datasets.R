#-------------------------------------
# MERGE CENSUS DEMOGS DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/4/21
# LAST UPDATED: 3/4/21
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
                         census_data_path,
                         "/race_10_on_10_cbg.rds"))
race00 <- readRDS(paste0(wd,
                         census_data_path,
                         "/race_00_on_10_cbg.rds"))
race90 <- readRDS(paste0(wd,
                         census_data_path,
                         "/race_90_on_10_cbg.rds"))

#PREPARE KEY VARIABLE-----------------------------------------------------------
race90 <- race90 %>%
  mutate(CBG_10 = str_replace_all(CBG_10, "G", ""))


#JOIN ALL DATASETS--------------------------------------------------------------
race_all_decades_cbg10 <- race10 %>%
  left_join(., race00, by = "CBG_10") 

  ## FIGURE OUT HOW TO REDUCE THE CBG IDENTIFIER FOR THE 1990 DATA


# EXPORT
saveRDS(race_all_decades_cbg10, "all_decades_race_on_cbg10.rds")
