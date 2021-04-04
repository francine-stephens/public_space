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
acs_race_path <- "/nhgisacs2019_csv/nhgis0022_ds244_20195_2019_blck_grp.csv"
acs_hu_inc_path <- "/nhgisacs2019_csv/R12793886_SL150.csv"

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

renamed_hu_inc_vars <- c("GEOID",
                         "HH",
                         "INC_LT10K",
                         "INC_10KTO14999",
                         "INC_15KTO19999",
                         "INC_20KTO24999",
                         "INC_25KTO29999",
                         "INC_30KTO34999",
                         "INC_35KTO39999",
                         "INC_40KTO44999",
                         "INC_45KTO49999",
                         "INC_50KTO59999",
                         "INC_60KTO74999",
                         "INC_75KTO99999",
                         "INC_100KTO124999",
                         "INC_125KTO149999",
                         "INC_150KTO199999",
                         "INC_200KPLUS",
                         "H_UNITS",
                         "OCCUPIED_UNITS",
                         "OWNER_OCC",
                         "RENTER_OCC",
                         "VACANT_UNITS")

# CLEAN & PROCESS DATA----------------------------------------------------------
acs_race <- read_csv(paste0(wd, 
                       acs_race_path))

acs_race_reduced <- acs_race %>%
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

acs_hu_inc <- read_csv(paste0(wd, 
                              acs_hu_inc_path))

acs_hu_inc_reduced <- acs_hu_inc %>%
  select(Geo_GEOID,
         SE_A14001_001:SE_A10001_001,
         SE_A10060_001:SE_A10060_003,
         SE_A10044_003) %>%
  rename_at(vars(Geo_GEOID:SE_A10044_003), ~ renamed_hu_inc_vars) %>%
  mutate(wave = "2019",
         GEOID = str_sub(GEOID, start = 8)
         ) %>%
  relocate(GEOID, wave)


# EXPORTS-----------------------------------------------------------------------
saveRDS(acs_race_reduced, "cbg_acs_2019_race.rds")
saveRDS(acs_hu_inc_reduced, "cbg_acs_2019_hu_inc.rds")
