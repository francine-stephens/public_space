#-------------------------------------
# MERGE IMPERVIOUS SPACE DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/23/21
# LAST UPDATED: 5/21/21
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
## WIDE DATASET
imp_area_90_w <- Imperv_1990_by_cbg10_AREA %>%
  select_mutate(.) %>% 
  rename(imp_area1990 = "area")
imp_area_00_w <- Imperv_2000_by_cbg10_AREA %>%
  select_mutate(.) %>%
  rename(imp_area2000 = "area")
imp_area_10_w <- Imperv_2010_by_cbg10_AREA %>%
  select_mutate(.) %>%
  rename(imp_area2010 = "area")
imp_area_19_w <-  Imperv_2019_by_cbg10_AREA %>%
  select_mutate(.) %>%
  rename(imp_area2020 = "area")

imp_area_all_years_wide <- imp_area_90_w %>%
  left_join(., imp_area_00_w, by = "GISJOIN") %>%
  left_join(., imp_area_10_w, by = "GISJOIN") %>%
  left_join(., imp_area_19_w, by = "GISJOIN") %>%
  mutate(areadiff_90to00 = imp_area2000 - imp_area1990,
         areadiff_00to10 = imp_area2010 - imp_area2000,
         areadiff_10to20 = imp_area2020 - imp_area2010)



## LONG DATASET
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
saveRDS(imp_area_all_years_wide, "impervious_area_all_years_on_2010cbg_WIDE.rds")
