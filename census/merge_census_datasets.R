#-------------------------------------
# MERGE CENSUS DEMOGRAPHICS DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/4/21
# LAST UPDATED: 4/4/21
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

inc10 <- readRDS(paste0(wd,
                        "/income_10_on_10_cbg.rds"))
inc00 <- readRDS(paste0(wd,
                        "/income_00_on_10_cbg.rds"))
inc90 <- readRDS(paste0(wd,
                        "/income_90_on_10_cbg.rds"))
inc_housing19 <- readRDS(paste0(wd,
                                "/cbg_acs_2019_hu_inc.rds"))

housing10 <- readRDS(paste0(wd, 
                            "/housing_10_on_10_cbg.rds"))
housing00 <- readRDS(paste0(wd, 
                            "/housing_00_on_10_cbg.rds"))
housing90 <- readRDS(paste0(wd, 
                            "/housing_90_on_10_cbg.rds"))


# PROCESS VARIABLES & MERGE-----------------------------------------------------

#######################################################
## 1990 PREPARATION INCLUDES FIXING KEY VARIABLE
#######################################################
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

housing90_edited <- housing90 %>%
  mutate(CBG_10 = str_replace_all(CBG_10, "G", ""),
         state = str_sub(CBG_10, end=2),
         county = str_sub(CBG_10, start=4, end=6),
         tract = str_sub(CBG_10, start=8, end=13),
         cbg = str_sub(CBG_10, start=-1),
         GEOID = str_c(state, county, tract, cbg, sep="")) %>%
  select(-CBG_10, -state:-cbg) %>% 
  mutate(wave = "1990") %>%
  relocate(GEOID, 
           wave,
           H_UNITS,
           OCCUPIED_UNITS,
           OWNER_OCC,
           RENTER_OCC,
           VACANT_UNITS)

#############
# RACE VARS
#############
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


## APPEND ALL DATASETS & EXPORT
race_all_decades_cbg10 <- rbind(race90_edited,
                                race00_edited,
                                race10_edited,
                                race19) %>%
  arrange(GEOID, wave)

saveRDS(race_all_decades_cbg10, "all_years_race_on_cbg10_LONG.rds")


################ 
# HOUSING VARS
################
housing00_edited <- housing00 %>% 
  rename(GEOID = "CBG_10") %>% 
  mutate(wave = "2000") %>%
  relocate(GEOID, wave)
  
housing10_edited <- housing10 %>% 
  rename(GEOID = "CBG_10") %>% 
  mutate(wave = "2010",
         OWNER_OCC = OWNER_OCC_1 + OWNER_OCC_2) %>% 
  select(-OWNER_OCC_1, -OWNER_OCC_2) %>% 
  relocate(GEOID,
           wave,
           H_UNITS,
           OCCUPIED_UNITS,
           OWNER_OCC,
           RENTER_OCC,
           VACANT_UNITS)

housing19_edited <- inc_housing19 %>%
  select(GEOID, wave, H_UNITS:VACANT_UNITS)

## APPEND ALL DATASETS & EXPORT
housing_all_decades_cbg10 <- rbind(housing90_edited,
                                housing00_edited,
                                housing10_edited,
                                housing19_edited) %>%
  arrange(GEOID, wave)

saveRDS(housing_all_decades_cbg10, "all_years_housing_on_cbg10_LONG.rds")
rm(housing_all_decades_cbg10,
   housing00,
   housing00_edited, 
   housing10,
   housing10_edited,
   housing19_edited,
   housing90,
   housing90_edited)

################ 
# INCOME VARS
################
inc90_edited <- inc90 %>%
  mutate(HH = rowSums(across(INC_LT10K:INC_150KPLUS))) %>%
  relocate(GEOID, wave, HH)

inc00_edited <- inc00 %>%
  mutate(INC_150KPLUS = INC_150KTO199999 + INC_200KPLUS) %>%
  select(-INC_150KTO199999:-INC_200KPLUS) %>% 
  mutate(HH = rowSums(across(INC_LT10K:INC_150KPLUS))) %>%
  relocate(GEOID, wave, HH)

inc10_edited <- inc10 %>% 
  mutate(CBG_10 = str_replace_all(GISJOIN, "G", ""),
         state = str_sub(CBG_10, end=2),
         county = str_sub(CBG_10, start=4, end=6),
         tract = str_sub(CBG_10, start=8, end=13),
         cbg = str_sub(CBG_10, start=-1),
         GEOID = str_c(state, county, tract, cbg, sep="")) %>%
  select(-CBG_10, -state:-cbg) %>%
  mutate(INC_150KPLUS = INC_150KTO199999 + INC_200KPLUS) %>%
  select(-GISJOIN, -INC_150KTO199999:-INC_200KPLUS) %>%
  relocate(GEOID, wave)

inc19_edited <- inc_housing19 %>%
  select(GEOID, wave, HH:INC_200KPLUS) %>%
  mutate(INC_150KPLUS = INC_150KTO199999 + INC_200KPLUS) %>%
  select(-INC_150KTO199999:-INC_200KPLUS)

## APPEND ALL DATASETS & EXPORT
income_all_decades_cbg10 <- rbind(inc90_edited,
                                  inc00_edited, 
                                  inc10_edited, 
                                  inc19_edited) %>%
  arrange(GEOID, wave)
                                  
saveRDS(income_all_decades_cbg10, "all_years_income_on_cbg10_LONG.rds")
