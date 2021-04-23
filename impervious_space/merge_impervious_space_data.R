#-------------------------------------
# MERGE IMPERVIOUS SPACE DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/23/21
# LAST UPDATED: 3/23/21
#-------------------------------------


# SET-UP------------------------------------------------------------------------

## LIBRARIES
packages <- c(
  "tidyverse",
  "panelr"
)
lapply(packages, library, character.only = T)

## Paths
setwd("~/Projects/public_space/impervious_space")
wd <- getwd()

## Import data
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

## Functions 
select_mutate <- function(x) { 
  x %>%       
    select(GISJOIN, area = "sum") %>%
    mutate(area = (area/2589988.1103)
           ) 
}

# PREPARE FULL DATASET----------------------------------------------------------
imp_area_90 <- Imperv_1990_by_cbg10_AREA %>%
  select_mutate(.) %>%
  mutate(year = 1990)
imp_area_00 <- Imperv_2000_by_cbg10_AREA %>%
  select_mutate(.) %>%
  mutate(year = 2000)
imp_area_10 <- Imperv_2010_by_cbg10_AREA %>%
  select_mutate(.) %>%
  mutate(year = 2010)
imp_area_19 <-  Imperv_2019_by_cbg10_AREA %>%
  select_mutate(.) %>%
  mutate(year = 2020)

imp_area_all_years <- bind_rows(imp_area_90, 
                                imp_area_00, 
                                imp_area_10, 
                                imp_area_19
                                )

# EXPORTS-----------------------------------------------------------------------
saveRDS(imp_area_all_years, "impervious_area_all_years_on_2010cbg_LONG.rds")
