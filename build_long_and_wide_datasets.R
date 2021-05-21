#-------------------------------------------------------------------------------
# BUILD GREEN & IMPERVIOUS SPACE DATASETS FOR ANALYSIS
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 5/21/2021
# LAST UPDATED: 5/21/2021
#
# This script builds long and wide versions of the dataframe used in scripts 
# that contain analysis.
#-------------------------------------------------------------------------------

## SETUP-----------------------------------------------------------------------_
# LIBRARIES
packages <- c(
  "readr",
  "tidyverse",
  "sf",
  "ggplot2", 
  "scales",
  "units",
  "ggcorrplot",
  "panelr",
  "tigris",
  "censusapi", 
  "tidycensus",
  "leaflet", 
  "tmap"
)
lapply(packages, library, character.only = T)

# PATHS
setwd("~/Projects/public_space")
wd <- getwd()
green_space_path <- "/green_space/gee/"
imperv_space_path <- "/impervious_space/"
census_data_path <- "/census"
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository"
shp2010_path <- "/2010USA_CensusGeog_Shp"
cbsa_path <- "/tl_2010_us_cbsa10/"
metdiv_path <- "/tl_2010_us_metdiv10/"
cbg10_path <- "/us_blck_grp_2010/"
places10_path <- "/tl2010_us_place_2010/"

## APIs
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c",  overwrite = T)
Sys.setenv(CENSUS_KEY="99ccb52a629609683f17f804ca875115e3f0804c")


## DATA IMPORTS-----------------------------------------------------------------
cbg10 <- st_read(paste0(shp_repo,
                        shp2010_path,
                        cbg10_path,
                        "US_blck_grp_2010.shp"), 
                 quiet = F)

# GREEN SPACE
evi_all_decades_cbg_l <- readRDS(paste0(wd, 
                                      green_space_path,
                                      "evi_all_years_on_2010cbg_LONG.rds"))

evi_area_decades_cbg_l <- readRDS(paste0(wd, 
                                       green_space_path,
                                       "evi_area_all_years_on_2010cbg_LONG.rds"))

evi_all_decades_cbg_w <- readRDS(paste0(wd, 
                                        green_space_path,
                                        "evi_all_years_on_2010cbg.rds"))
  
evi_area_decades_cbg_w <- readRDS(paste0(wd, 
                                         green_space_path,
                                         "evi_area_all_years_on_2010cbg.rds"))
  
  
vegetation_pct_all_decades_l <- readRDS(paste0(wd, 
                                               green_space_path,
                                               "vegetation_median_pct_LONG.rds"))
  
vegetation_pct_all_decades_w <- readRDS(paste0(wd, 
                                               green_space_path,
                                               "vegetation_median_pct_WIDE.rds"))


# IMPERVIOUS SPACE
imperv_area_decades_cbg_l <- readRDS(paste0(wd, 
                                          imperv_space_path,
                                          "impervious_area_all_years_on_2010cbg_LONG.rds"))

imperv_area_decades_cbg_w <- readRDS(paste0(wd, 
                                          imperv_space_path,
                                          "impervious_area_all_years_on_2010cbg_WIDE.rds"))


# CENSUS/ACS DATA
race_all_decades_cbg <- readRDS(paste0(wd,
                                       census_data_path, 
                                       "/all_years_race_on_cbg10_LONG.rds"))

housing_all_decades_cbg <- readRDS(paste0(wd, 
                                          census_data_path, 
                                          "/all_years_housing_on_cbg10_LONG.rds"))

income_all_decades_cbg <- readRDS(paste0(wd, 
                                         census_data_path, 
                                         "/all_years_income_on_cbg10_LONG.rds"))

counties <- get_decennial(geography = "county", 
                          variables = "P001001", 
                          year = 2010) 

places <- get_decennial(geography = "place", 
                        variables = "P001001", 
                        year = 2010) 

# GEOGRAPHIC IDENTIFIERS
cbg_higher_geog_ids <- readRDS(paste0(shp_repo,
                                      "/cbg_full_geog_identifers.rds"))
top100_cbsa <- read_csv(paste0(shp_repo, 
                               "/top100_cbsa_2010bounds.csv"))
top100_places <- read_csv(paste0(shp_repo, 
                                 "/top100_places_2010bounds.csv"))
state_id_regions <- read_csv(paste0(shp_repo, 
                                    "/state_identifiers_census_regions.csv"))
cbsa_county_cw <- read_csv(paste0(shp_repo,
                                  "/cbsa2fipsxw2010.csv"))


## DATA TRANSFORMATIONS---------------------------------------------------------
# MERGE ALL GEOIDs
top100_cbsa <- top100_cbsa %>%
  mutate(CBSAFP = as.character(CBSAFP)) %>%
  select(-CBSA_NM) %>%
  rename(top100_cbsa="top_100", cbsa_rank_pop_size="rank_pop_size")

top100_places <- top100_places %>%
  select(-PLACE_NM) %>%
  rename(top100_place="top_100", place_rank_pop_size="rank_pop_size")

counties <- counties %>%
  select(PLACEFP_DERIVED = "GEOID", PLACE_DERIVED_NM = "NAME")

cbsa_co_cw_reformat <- cbsa_county_cw %>%
  mutate(across(ends_with("code"), as.character)) %>%
  mutate(cbsacode = str_pad(cbsacode, 5, side = "left", "0"),
         metrodivisioncode = str_pad(metrodivisioncode, 5, side = "left", "0"),
         fipsstatecode = str_pad(fipsstatecode, 2, side = "left", "0"),
         fipscountycode = str_pad(fipscountycode, 3, side = "left", "0"),
         state_co = str_c(fipsstatecode, fipscountycode, sep = "")
  )

cbg_all_geoids <- cbg10 %>%
  st_set_geometry(NULL) %>%
  select(GEOID10, GISJOIN) %>% 
  mutate(state_co = str_sub(GEOID10, end = 5)) %>%
  left_join(., cbg_higher_geog_ids, by=c("GEOID10", "GISJOIN")) %>%
  select(-CBSAFP10:-METDIV_NM) %>%
  mutate(PLACEFP_DERIVED = if_else(is.na(PLACEID),
                                   str_sub(GEOID10, end=5),
                                   PLACEID)) %>% 
  left_join(., counties, by = "PLACEFP_DERIVED") %>%
  mutate(PLACE_DERIVED_NM=coalesce(PLACE_DERIVED_NM, PLACE_NM)) %>%
  relocate(PLACEFP_DERIVED, .after=PLACEID) %>%
  relocate(PLACE_DERIVED_NM, .after=PLACE_NM) %>%
  left_join(., top100_places, by=c("PLACEID"="PLACEFP")) %>%
  left_join(., cbsa_co_cw_reformat, by = "state_co") %>%
  left_join(., top100_cbsa, by=c("cbsacode"="CBSAFP")) %>%
  mutate(in_metro = if_else(str_detect(metropolitanmicropolitanstatus, "Metro"),
                            "Metropolitan Area",
                            "Micropolitan Area")
  )


# AREAL CALCULATIONS
cbg_area <- cbg10 %>%
  select(GEOID10) %>%
  mutate(area = st_area(.),
         area = as.numeric(set_units(area, mi^2))
  ) %>%
  st_set_geometry(NULL)


# MERGE ALL DEMOGRAPHIC DATA
race_pct_all_decades_cbg_l <- race_all_decades_cbg %>% 
  mutate_at(vars(WHITE_NH:HISPANIC), funs("PCT" = (./TOT_POP) * 100)) %>%
  select(GEOID, wave, TOT_POP, WHITE_NH_PCT:HISPANIC_PCT) %>%
  mutate(wave = as.numeric(wave),
         wave = if_else(wave == 2019,
                        2020,
                        wave)
  )

race_pct_all_decades_cbg_w <- race_pct_all_decades_cbg_l %>% 
  pivot_wider(., 
              id_cols = GEOID, 
              names_from = wave,
              values_from = c(TOT_POP:HISPANIC_PCT)
              ) %>%
  mutate(TPOP_PCTCHG_90TO00 = ((TOT_POP_2000 - TOT_POP_1990)/TOT_POP_1990) * 100,
         TPOP_PCTCHG_00TO10 = ((TOT_POP_2010 - TOT_POP_2000)/TOT_POP_2000) * 100,
         TPOP_PCTCHG_10TO20 = ((TOT_POP_2020 - TOT_POP_2010)/TOT_POP_2010) * 100,
         WHITE_NH_PCTCHG_90TO00 = ((WHITE_NH_PCT_2000 - WHITE_NH_PCT_1990)/WHITE_NH_PCT_1990) * 100,
         WHITE_NH_PCTCHG_00TO10 = ((WHITE_NH_PCT_2010 - WHITE_NH_PCT_2000)/WHITE_NH_PCT_2000) * 100,
         WHITE_NH_PCTCHG_10TO20 = ((WHITE_NH_PCT_2020 - WHITE_NH_PCT_2010)/WHITE_NH_PCT_2010) * 100,
         BLACK_NH_PCTCHG_90TO00 = ((BLACK_NH_PCT_2000 - BLACK_NH_PCT_1990)/BLACK_NH_PCT_1990) * 100,
         BLACK_NH_PCTCHG_00TO10 = ((BLACK_NH_PCT_2010 - BLACK_NH_PCT_2000)/BLACK_NH_PCT_2000) * 100,
         BLACK_NH_PCTCHG_10TO20 = ((BLACK_NH_PCT_2020 - BLACK_NH_PCT_2010)/BLACK_NH_PCT_2010) * 100,
         AAPI_PCTCHG_90TO00 = ((ASIAN_PACIFIC_NH_PCT_2000 - ASIAN_PACIFIC_NH_PCT_1990)/ASIAN_PACIFIC_NH_PCT_1990) * 100,
         AAPI_PCTCHG_00TO10 = ((ASIAN_PACIFIC_NH_PCT_2010 - ASIAN_PACIFIC_NH_PCT_2000)/ASIAN_PACIFIC_NH_PCT_2000) * 100,
         AAPI_PCTCHG_10TO20 = ((ASIAN_PACIFIC_NH_PCT_2020 - ASIAN_PACIFIC_NH_PCT_2010)/ASIAN_PACIFIC_NH_PCT_2010) * 100,
         HISPANIC_PCTCHG_90TO00 = ((HISPANIC_PCT_2000 - HISPANIC_PCT_1990)/HISPANIC_PCT_1990) * 100,
         HISPANIC_PCTCHG_00TO10 = ((HISPANIC_PCT_2010 - HISPANIC_PCT_2000)/HISPANIC_PCT_2000) * 100,
         HISPANIC_PCTCHG_10TO20 = ((HISPANIC_PCT_2020 - HISPANIC_PCT_2010)/HISPANIC_PCT_2010) * 100
           ) %>%
  select(GEOID, TOT_POP_1990:TOT_POP_2020, TPOP_PCTCHG_90TO00:HISPANIC_PCTCHG_10TO20) %>%
  mutate(across(where(is.numeric), ~na_if(., Inf))) %>%
  mutate(across(where(is.numeric), ~na_if(., NaN)))


housing_pct_all_decades_cbg_l <- housing_all_decades_cbg %>%
  mutate_at(vars(OCCUPIED_UNITS, VACANT_UNITS),  funs("PCT" = (./H_UNITS) * 100)) %>%
  mutate_at(vars(OWNER_OCC:RENTER_OCC), funs("PCT" = (./OCCUPIED_UNITS) * 100)) %>%
  select(GEOID:H_UNITS, OCCUPIED_UNITS_PCT:RENTER_OCC_PCT) %>%
  mutate(wave = as.numeric(wave),
         wave = if_else(wave == 2019,
                        2020,
                        wave)
  )

housing_pct_all_decades_cbg_w <- housing_pct_all_decades_cbg_l %>%
  pivot_wider(., 
              id_cols = GEOID, 
              names_from = wave,
              values_from = c(H_UNITS:RENTER_OCC_PCT)
  )


income_pct_all_decades_cbg_l <- income_all_decades_cbg %>% 
  mutate_at(vars(starts_with("INC_")), funs("PCT" = (./HH) * 100)) %>%
  mutate(LOW_INC_PCT = rowSums(across(INC_LT10K_PCT:INC_20KTO24999_PCT)),
         MID_INC_PCT = rowSums(across(INC_25KTO29999_PCT:INC_75KTO99999_PCT)),
         AFF_INC_PCT = rowSums(across(INC_100KTO124999_PCT:INC_150KPLUS_PCT))
  ) %>% 
  select(GEOID:wave, LOW_INC_PCT:AFF_INC_PCT) %>% 
  mutate(wave = as.numeric(wave),
         wave = if_else(wave == 2019,
                        2020,
                        wave)
  )

income_pct_all_decades_cbg_w <- income_pct_all_decades_cbg_l %>%
  pivot_wider(., 
              id_cols = GEOID, 
              names_from = wave,
              values_from = c(LOW_INC_PCT:AFF_INC_PCT)
  )


# CLEAN ENVIRONMENTAL DATA
evi_area_clean_l <- evi_area_decades_cbg_l %>% 
  rename(green_area = "area")

evi_area_clean_w <- evi_area_decades_cbg_w %>% 
  rename(green_area1990 = "area90",
         green_area2000 = "area00",
         green_area2010 = "area10")

evi_all_decades_chg_cbg_w <- evi_all_decades_cbg_w %>%
  mutate(evi_pctchg_90to00 = ((evi00 - evi90)/evi90) * 100,
         evi_pctchg_00to10 = ((evi10 - evi00)/evi00) * 100,
         evi_pctchg_10to20 = ((evi20 - evi10)/evi10) * 100) %>%
  select(GISJOIN, evi_pctchg_90to00:evi_pctchg_10to20)


imp_area_clean_l <- imperv_area_decades_cbg_l %>%
  rename(imperv_area = "area",
         wave = "year") %>%
  mutate(wave = as.numeric(wave))

imp_area_clean_w <- imperv_area_decades_cbg_w %>% 
  rename_at(vars(areadiff_90to00:areadiff_10to20), function(x) paste0("imp_", x))
  


# CREATE FULL LONG DATASET
full_cbg_data_l <- evi_all_decades_cbg_l %>% 
  left_join(., cbg_all_geoids, by = "GISJOIN") %>% 
  left_join(., state_id_regions, by = c("fipsstatecode" = "state_fips")) %>% 
  left_join(., race_pct_all_decades_cbg_l, by = c("GEOID10" = "GEOID",
                                                "wave" = "wave")) %>% 
  left_join(., housing_pct_all_decades_cbg_l, by = c("GEOID10" = "GEOID",
                                                   "wave" = "wave")) %>% 
  left_join(., income_pct_all_decades_cbg_l, by = c("GEOID10" = "GEOID",
                                                  "wave" = "wave")) %>%   
  relocate(evi, .before = TOT_POP) %>%
  left_join(., cbg_area, by = "GEOID10") %>%
  mutate(POP_DENS = TOT_POP/area,
         H_DENS = H_UNITS/area) %>%
  left_join(., evi_area_clean_l, by = c("GISJOIN", "wave")) %>%  
  mutate(perc_green = green_area/area, 
         perc_green = if_else(perc_green > 1, 
                              1,
                              perc_green)
  ) %>%
  left_join(., imp_area_clean_l, by = c("GISJOIN", "wave")) %>%  
  mutate(perc_imperv = imperv_area/area, 
         perc_imperv = if_else(perc_imperv > 1, 
                               1,
                               perc_imperv)
  ) %>%
  left_join(., vegetation_pct_all_decades_l, by = c("GISJOIN", "wave"))


# CREATE FULL WIDE DATASET
full_cbg_chg_data_w <- evi_all_decades_chg_cbg_w %>% 
  left_join(., cbg_all_geoids, by = "GISJOIN") %>% 
  left_join(., state_id_regions, by = c("fipsstatecode" = "state_fips")) %>% 
  select(-state_nm) %>%
  left_join(., race_pct_all_decades_cbg_w, by = c("GEOID10" = "GEOID")) %>% 
    left_join(., vegetation_pct_all_decades_w, by = "GISJOIN") %>%
  left_join(., cbg_area, by = "GEOID10") %>%
  left_join(., evi_area_clean_w, by = "GISJOIN") %>%  
  left_join(., imp_area_clean_w, by = "GISJOIN") %>%
  mutate(pct_imperv_1990 = (imp_area1990/area) * 100,
         pct_imperv_2000 = (imp_area2000/area) * 100,
         pct_imperv_2010 = (imp_area2010/area) * 100,
         pct_imperv_2020 = (imp_area2020/area) * 100,
         pct_imperv_1990 = if_else(pct_imperv_1990 > 100,
                               100,
                               pct_imperv_1990),
         pct_imperv_2000 = if_else(pct_imperv_2000 > 100,
                                   100,
                                   pct_imperv_2000),
         pct_imperv_2010 = if_else(pct_imperv_2010 > 100,
                                   100,
                                   pct_imperv_2010),
         pct_imperv_2020 = if_else(pct_imperv_2020 > 100,
                                   100,
                                   pct_imperv_2020),
         imperv_pctchg_90to00 = ((pct_imperv_2000 - pct_imperv_1990)/pct_imperv_1990) * 100,
         imperv_pctchg_00to10 = ((pct_imperv_2010 - pct_imperv_2000)/pct_imperv_2000) * 100,
         imperv_pctchg_10to20 = ((pct_imperv_2020 - pct_imperv_2010)/pct_imperv_2010) * 100
  ) %>% 
  mutate(across(where(is.numeric), ~na_if(., Inf)))
  
  
## EXPORT-----------------------------------------------------------------------
saveRDS(full_cbg_data_l, "all_vars_on_cbg10_LONG.rds")
saveRDS(full_cbg_chg_data_w, "chg_vars_on_cbg10_WIDE.rds")
