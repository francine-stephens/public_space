#-------------------------------------
# MERGE CENSUS DEMOGRAPHICS DATA
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
setwd("~/Projects/public_space/census")
wd <- getwd()


## IMPORT DATASETS
race10 <- readRDS(paste0(wd,
                         "/race_10_on_10_cbg.rds"))
race00 <- readRDS(paste0(wd,
                         "/race_00_on_10_cbg.rds"))
race90 <- readRDS(paste0(wd,
                         "/race_90_on_10_cbg.rds"))
race19 <- readRDS(paste0(wd,
                         "/cbg_acs_2019_race.rds"))

# PROCESS VARIABLES & MERGE-----------------------------------------------------
## 1990 PREPARATION INCLUDES FIXING KEY VARIABLE.
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
  select(-CBG_10, -state:-cbg) %>%
  rename_with(., ~ gsub("_90", "", .x)) %>%
  mutate(wave = "1990") %>%
  relocate(GEOID, wave) %>%
  rename(OTHER_MULTIRACIAL_NH = "OTHER_RACE_NH",
         ASIAN_PACIFIC_NH = "ASIAN_NH")

race00_edited <- race00 %>%
  rename_with(., ~ gsub("_00", "", .x)) %>%
  rename(GEOID = "CBG_10",
         ASIAN_PACIFIC_NH = "ASIAN_NH") %>%
  mutate(wave = "2000",
         OTHER_MULTIRACIAL_NH = OTHER_RACE_NH + MULTIRACIAL_NH) %>%
  select(-OTHER_RACE_NH, -MULTIRACIAL_NH) %>%
  relocate(OTHER_MULTIRACIAL_NH, .before = HISPANIC) %>%
  relocate(GEOID, wave)

race10_edited <- race10 %>%
  rename(GEOID = "CBG_10") %>%
  rename_with(., ~ gsub("_10", "", .x)) %>%
  mutate(wave = "2010",
         OTHER_MULTIRACIAL_NH = OTHER_RACE_NH + MULTIRACIAL_NH, 
         ASIAN_PACIFIC_NH = ASIAN_NH + PACIFIC_NH) %>%
  select(-OTHER_RACE_NH, -MULTIRACIAL_NH, -ASIAN_NH, -PACIFIC_NH) %>%
  relocate(OTHER_MULTIRACIAL_NH, .before = HISPANIC) %>%
  relocate(ASIAN_PACIFIC_NH, .after = AM_IND_NH) %>%
  relocate(GEOID, wave)


## APPEND ALL DATASETS
race_all_decades_cbg10 <- rbind(race90_edited,
                                race00_edited,
                                race10_edited,
                                race19) %>%
  arrange(GEOID, wave)


# EXPORT
saveRDS(race_all_decades_cbg10, "all_years_race_on_cbg10_LONG.rds")
