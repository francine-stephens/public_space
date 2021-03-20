#-------------------------------------
# CREATE ACS DEMOGS DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/19/21
# LAST UPDATED: 3/19/21
#-------------------------------------

## LIBRARIES
packages <- c(
  "tidyverse"
)
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Projects/public_space/census")
wd <- getwd()
acs_path <- "/nhgisacs2019_csv/nhgis0022_ds244_20195_2019_blck_grp.csv"

## PARAMETERS
selected_vars <- c("GISJOIN",
                    "YEAR",
                    "GEOID",
                    "ALUKE001",
                    "ALUKE003",
                    "ALUKE004",
                    "ALUKE005",
                    "ALUKE006",
                    "ALUKE007",
                    "ALUKE008",
                    "ALUKE009",
                    "ALUKE012"
)

renamed_vars <- c("GISJOIN",
                  "wave",
                  "GEOID",
                  "TOT_POP",
                  "WHITE_NH",
                  "BLACK_NH",
                  "AM_IND_NH",
                  "ASIAN_NH",
                  "PACIFIC_NH",
                  "OTHER_RACE_NH",
                  "MULTIRACIAL_NH",
                  "HISPANIC"
)


# CLEAN & PROCESS DATA----------------------------------------------------------
acs <- read_csv(paste0(wd, 
                       acs_path))

acs_reduced <- acs %>%
  select(all_of(selected_vars)) %>%
  rename_at(vars(GISJOIN:ALUKE012), ~ renamed_vars) %>%
  mutate(wave = str_remove(wave, "2015-"),
         GEOID = str_sub(GEOID, start = 8),
         OTHER_MULTIRACIAL_NH = OTHER_RACE_NH + MULTIRACIAL_NH, 
         ASIAN_PACIFIC_NH = ASIAN_NH + PACIFIC_NH) %>%
  select(-OTHER_RACE_NH, -MULTIRACIAL_NH, -ASIAN_NH, -PACIFIC_NH, -GISJOIN) %>%
  relocate(OTHER_MULTIRACIAL_NH, .before = HISPANIC) %>%
  relocate(ASIAN_PACIFIC_NH, .after = AM_IND_NH) %>%
  relocate(GEOID, wave)


# EXPORTS-----------------------------------------------------------------------
saveRDS(acs_reduced, "cbg_acs_2019_race.rds")
