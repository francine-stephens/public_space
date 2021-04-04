#-------------------------------------
# CREATE CENSUS DEMOGS DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 2/26/21
# LAST UPDATED: 4/3/21
#-------------------------------------

## LIBRARIES
packages <- c(
  "tidyverse"
  )
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Projects/public_space/census")
wd <- getwd()
census90_path <- "/nhgis1990_csv/nhgis0017_ds120_1990_block.csv"
census00_path <- "/nhgis2000_csv/nhgis0018_ds147_2000_block.csv"
census10_path <- "/nhgis2010_csv/nhgis0019_ds172_2010_block.csv"
censusinc_path <- "/nhgisinc_csv/"
inc90_path <- "nhgis0023_ds123_1990_blck_grp_598.csv"
inc00_path <- "nhgis0023_ds152_2000_blck_grp_090.csv"
inc10_path <- "nhgis0023_ds176_20105_2010_blck_grp_E.csv"
cw_path <- "/crosswalks/"
cw_1990_path <- "nhgis_blk1990_blk2010_gj.csv"
cw_2000_path <- "nhgis_blk2000_blk2010_ge.csv"
cw_1990_bgp_path <- "nhgis_bgp1990_bg2010.csv"
cw_2000_bgp_path <- "nhgis_bgp2000_bg2010.csv"

#CLEAN & PROCESS 2010-----------------------------------------------------------
census10 <- read_csv(paste0(wd,
                            census10_path))

selected_10_vars <- c("GISJOIN",
                      "YEAR",
                      "REGIONA",
                      "DIVISIONA",
                      "STATE",
                      "STATEA",
                      "COUNTY",
                      "COUNTYA",
                      "PLACEA",
                      "TRACTA", 
                      "BLKGRPA",
                      "BLOCKA",
                      "CBSAA",
                      "METDIVA",
                      "UAA",
                      "URBRURALA",
                      "H7V001",
                      "H7X002",
                      "H7X003",
                      "H7X004",
                      "H7X005",
                      "H7X006",
                      "H7X007",
                      "H7X008",
                      "H7Z002",
                      "H7Z003",
                      "H7Z004",
                      "H7Z005",
                      "H7Z006",
                      "H7Z007",
                      "H7Z008",
                      "H7Z009",
                      "H7Z010",
                      "IFC001",
                      "IFE002",
                      "IFE003",
                      "IFF002",
                      "IFF003",
                      "IFF004"
)

renamed_10_vars <- c("GISJOIN",
                      "YEAR",
                      "REGION_C",
                      "DIVISION_C",
                      "STATE_NM",
                      "STATE_C",
                      "COUNTY_NM",
                      "COUNTY_C",
                      "PLACE_C",
                      "TRACT_C", 
                      "BLCK_GRP_C",
                      "BLOCK_C",
                      "CBSA_C",
                      "METDIV_C",
                      "URB_AREA_C",
                      "URB_RURAL_C",
                      "TOT_POP",
                      "WHITE",
                      "BLACK",
                      "AM_IND",
                      "ASIAN",
                      "PACIFIC",
                      "OTHER_RACE",
                      "MULTIRACIAL",
                      "NOT_HISPANIC",
                      "WHITE_NH",
                      "BLACK_NH",
                      "AM_IND_NH",
                      "ASIAN_NH",
                      "PACIFIC_NH",
                      "OTHER_RACE_NH",
                      "MULTIRACIAL_NH",
                      "HISPANIC",
                      "H_UNITS",
                      "OCCUPIED_UNITS",
                      "VACANT_UNITS",
                      "OWNER_OCC_1",
                      "OWNER_OCC_2",
                      "RENTER_OCC"
)

# REDUCE 2010 CENSUS BLOCK DATA
census10_red <- census10 %>% 
  slice(-1) %>%
  select(all_of(selected_10_vars)) %>%
  rename_at(vars(GISJOIN:IFF004), ~ renamed_10_vars)

saveRDS(census10_red, "census_blocks_2010.rds")

rm(census10, census10_red)

######################################
## AGGREGATE 2010 RACE DATA INTO CBGs
#######################################
census_blocks_2010 <- readRDS("census_blocks_2010.rds")

cbg_10_census <- census_blocks_2010 %>%
  mutate(CBG_10 = str_c(STATE_C, COUNTY_C, TRACT_C, BLCK_GRP_C, sep = "")) %>%
  mutate(TOT_POP = as.numeric(TOT_POP),
         HISPANIC = as.numeric(HISPANIC)) %>%
  mutate(across(ends_with("_NH"), as.numeric)) %>%
  group_by(CBG_10) %>%
  summarize(TOT_POP_10 = sum(TOT_POP),
            WHITE_NH_10 = sum(WHITE_NH),
            BLACK_NH_10 = sum(BLACK_NH),
            AM_IND_NH_10 = sum(AM_IND_NH),
            ASIAN_NH_10 = sum(ASIAN_NH),
            PACIFIC_NH_10 = sum(PACIFIC_NH),
            OTHER_RACE_NH_10 = sum(OTHER_RACE_NH),
            MULTIRACIAL_NH_10 = sum(MULTIRACIAL_NH),
            HISPANIC_10 = sum(HISPANIC)
  )
saveRDS(cbg_10_census, "race_10_on_10_cbg.rds")
rm(cbg_10_census)

####################################
# AGGREGATE 2010 HOUSING INTO CBGs
###################################
cbg_10_census <- census_blocks_2010 %>% 
  select(STATE_C, COUNTY_C, TRACT_C, BLCK_GRP_C, H_UNITS:RENTER_OCC) %>%
  mutate(CBG_10 = str_c(STATE_C, COUNTY_C, TRACT_C, BLCK_GRP_C, sep = "")) %>%
  mutate(across(H_UNITS:RENTER_OCC, as.numeric)) %>%
  group_by(CBG_10) %>%
  summarize(across(where(is.numeric), ~sum(.x)))

saveRDS(cbg_10_census, "housing_10_on_10_cbg.rds")
rm(cbg_10_census, census_blocks_2010)


####################################
# AGGREGATE 2010 INCOME INTO CBGs
###################################
census10inc <- read_csv(paste0(wd, 
                               censusinc_path,
                               inc10_path))

renamed_10_inc_vars <- c("GISJOIN", 
                         "YEAR",
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
                         "INC_200KPLUS"
                        )
                          

census10inc_red <- census10inc %>% 
  slice(-1) %>%
  select(GISJOIN:YEAR, JOHE001:JOHE017) %>%
  rename_at(vars(GISJOIN:JOHE017), ~ renamed_10_inc_vars) %>%
  mutate(across(HH:INC_200KPLUS, as.numeric)) %>%
  mutate(wave = str_remove(YEAR, "2006-")) %>%
  select(-YEAR) %>%
  relocate(wave, .after = GISJOIN)

saveRDS(census10inc_red, "income_10_on_10_cbg.rds")
rm(census10inc, census10inc_red)

#CLEAN & PROCESS 1990-----------------------------------------------------------
census90 <- read_csv(paste0(wd, 
                            census90_path))

selected_90_vars <- c("GISJOIN",
                      "YEAR",
                      "BLOCKA",
                      "BLCK_GRPA",
                      "TRACTA",
                      "COUNTY",
                      "COUNTYA",
                      "STATE",
                      "STATEA",
                      "URBRURALA",
                      "URB_AREAA",
                      "ET1001",
                      "EUY001",
                      "EUY002",
                      "EUY003",
                      "EUY004",
                      "EUY005",
                      "ET2001",
                      "ET2002",
                      "ET2003",
                      "ET2004",
                      "ET2005",
                      "ET2006",
                      "ET2007",
                      "ET2008",
                      "ET2009",
                      "ET2010",
                      "ESA001",
                      "ESN001",
                      "ESN002",
                      "ES1001",
                      "ES1002"
)

renamed_90_vars <- c("GISJOIN",
                     "YEAR",
                     "BLOCK_C",
                     "BLCK_GRP_C",
                     "TRACT_C",
                     "COUNTY_NM",
                     "COUNTY_C",
                     "STATE_NM",
                     "STATE_C",
                     "URB_RURAL_C",
                     "URB_AREA_C",
                     "TOT_POP",
                     "WHITE",
                     "BLACK",
                     "AM_IND",
                     "ASIAN",
                     "OTHER_RACE",
                     "WHITE_NH",
                     "BLACK_NH",
                     "AM_IND_NH",
                     "ASIAN_NH",
                     "OTHER_RACE_NH",
                     "WHITE_HISP",
                     "BLACK_HISP",
                     "AM_IND_HISP",
                     "ASIAN_HISP",
                     "OTHER_RACE_HISP",
                     "H_UNITS",
                     "OCCUPIED_UNITS",
                     "VACANT_UNITS",
                     "OWNER_OCC",
                     "RENTER_OCC"
)

census90_red <- census90 %>%
  select(all_of(selected_90_vars)) %>%
  slice(-1) %>%
  rename_at(vars(GISJOIN:ES1002), ~ renamed_90_vars)

rm(census90)

saveRDS(census90_red, "census_blocks_1990.rds")
census90_red <- readRDS("census_blocks_1990.rds")

##################
# RACE DATA
##################

## HISPANIC---------------------------------------------------------------------
census90_hisp <- census90_red %>%
  select(GISJOIN, WHITE_HISP:OTHER_RACE_HISP) %>%
  mutate(across(where(is.character) & !c(GISJOIN), as.numeric)) %>%
  mutate(HISPANIC = rowSums(across(where(is.numeric)))) %>%
  select(GISJOIN, HISPANIC)

cw_1990 <- read_csv(paste0(wd, 
                           cw_path, 
                           cw_1990_path))

weighted_estimates <- census90_hisp %>%
  left_join(., cw_1990, by = c("GISJOIN" = "GJOIN1990")) %>%
  select(-PAREA_VIA_BLK00) %>%
  mutate(across(where(is.numeric) & !c(WEIGHT), ~. * WEIGHT)) %>%
  select(-WEIGHT, -GISJOIN) 
rm(cw_1990)

census90_hisp_on_10_geog <- weighted_estimates %>% 
  group_by(GJOIN2010) %>%
  summarize(HISPANIC_90 = sum(HISPANIC))
rm(weighted_estimates)

## WHITE - OTHER RACE-----------------------------------------------------------
census90_red <- readRDS("census_blocks_1990.rds")

cw_1990 <- read_csv(paste0(wd, 
                           cw_path, 
                           cw_1990)) 

census90_race <- census90_red %>%
  select(GISJOIN, TOT_POP, WHITE_NH:OTHER_RACE_NH) %>%
  mutate(across(where(is.character) & !c(GISJOIN), as.numeric)) %>%
  left_join(., cw_1990, by = c("GISJOIN" = "GJOIN1990")) %>%
  select(-PAREA_VIA_BLK00)
rm(census90_red, cw_1990)


weighted_estimates <- census90_race %>%
  mutate(across(where(is.numeric) & !c(WEIGHT), ~. * WEIGHT)) %>%
  select(-WEIGHT, -GISJOIN) 
rm(census90_race)

census90_race_on_10_geog <- weighted_estimates %>% 
  group_by(GJOIN2010) %>%
  summarize(TOT_POP_90 = sum(TOT_POP),
            WHITE_NH_90 = sum(WHITE_NH),
            BLACK_NH_90 = sum(BLACK_NH),
            AM_IND_NH_90 = sum(AM_IND_NH),
            ASIAN_NH_90 = sum(ASIAN_NH),
            OTHER_RACE_NH_90 = sum(OTHER_RACE_NH)
            )
rm(weighted_estimates)


## FULL RACE VARS 90 ESTIMATES ON 2010 GEOG
full_census_90_race_on_10_geog <- census90_race_on_10_geog %>%
  full_join(., census90_hisp_on_10_geog, by = "GJOIN2010")
rm(census90_hisp_on_10_geog, census90_race_on_10_geog)

saveRDS(full_census_90_race_on_10_geog, "blocks_race_90_on_10_blocks.rds")

## FULL RACE VARS 90 ESTIMATES ON 2010 CBGS
full_census_90_race_on_10_cbg <- full_census_90_race_on_10_geog %>%
  mutate(CBG_10 = str_sub(GJOIN2010, end = -4)) %>%
  select(-GJOIN2010) %>%
  group_by(CBG_10) %>%
  summarize(TOT_POP_90 = sum(TOT_POP_90),
            WHITE_NH_90 = sum(WHITE_NH_90),
            BLACK_NH_90 = sum(BLACK_NH_90),
            AM_IND_NH_90 = sum(AM_IND_NH_90),
            ASIAN_NH_90 = sum(ASIAN_NH_90),
            OTHER_RACE_NH_90 = sum(OTHER_RACE_NH_90),
            HISPANIC_90 = sum(HISPANIC_90)
  )
saveRDS(full_census_90_race_on_10_cbg, "race_90_on_10_cbg.rds")

rm(full_census_90_race_on_10_cbg, full_census_90_race_on_10_geog)

#################
# HOUSING DATA
#################
census90_housing <- census90_red %>%
  select(GISJOIN, H_UNITS:RENTER_OCC) %>%
  mutate(across(where(is.character) & !c(GISJOIN), as.numeric)) %>%
  left_join(., cw_1990, by = c("GISJOIN" = "GJOIN1990")) %>%
  select(-PAREA_VIA_BLK00)
rm(census90_red, cw_1990)
  
weighted_estimates <- census90_housing %>%
  mutate(across(where(is.numeric) & !c(WEIGHT), ~. * WEIGHT)) %>%
  select(-WEIGHT, -GISJOIN) 
rm(census90_housing)
  
cbg_90_census <- weighted_estimates %>%
  select(GJOIN2010, H_UNITS:RENTER_OCC) %>%
  mutate(CBG_10 = str_sub(GJOIN2010, end = -4)) %>%
  select(-GJOIN2010) %>%
  group_by(CBG_10) %>%
  summarize(across(where(is.numeric), ~sum(.x)))

saveRDS(cbg_90_census, "housing_90_on_10_cbg.rds")
rm(weighted_estimates, cbg_90_census)


####################################
# AGGREGATE 1990 INCOME INTO CBGs
###################################
census90inc <- read_csv(paste0(wd, 
                               censusinc_path,
                               inc90_path))

renamed_90_inc_vars <- c("GISJOIN", 
                         "YEAR",
                         "INC_LT5K",
                         "INC_5KTO9999",
                         "INC_10KTO12499",
                         "INC_12500TO14999",
                         "INC_15KTO17499",
                         "INC_17500TO19999",
                         "INC_20KTO22499",
                         "INC_22500TO24999",
                         "INC_25KTO274999",
                         "INC_27500TO29999",
                         "INC_30KTO32499",
                         "INC_32500TO34999",
                         "INC_35KTO37499",
                         "INC_37500TO39999",
                         "INC_40KTO42499",
                         "INC_42500TO44999",
                         "INC_45KTO47499",
                         "INC_47500TO49999",
                         "INC_50KTO54999",
                         "INC_55KTO59999",
                         "INC_60KTO74999",
                         "INC_75KTO99999",
                         "INC_100KTO124999",
                         "INC_125KTO149999",
                         "INC_150KPLUS"
)

census90inc_red <- census90inc %>% 
  slice(-1) %>%
  select(GISJOIN:YEAR, E4T001:E4T025) %>%
  rename_at(vars(GISJOIN:E4T025), ~ renamed_90_inc_vars) %>%
  mutate(across(INC_LT5K:INC_150KPLUS, as.numeric)) %>%
  rename(wave = "YEAR") %>%
  mutate(INC_LT10K = INC_LT5K + INC_5KTO9999,
         INC_10KTO14999 = INC_10KTO12499 + INC_12500TO14999,
         INC_15KTO19999 = INC_15KTO17499 + INC_17500TO19999,
         INC_20KTO24999 = INC_20KTO22499 + INC_22500TO24999,
         INC_25KTO29999 = INC_25KTO274999 + INC_27500TO29999, 
         INC_30KTO34999 = INC_30KTO32499 + INC_32500TO34999,
         INC_35KTO39999 = INC_35KTO37499 + INC_37500TO39999, 
         INC_40KTO44999 = INC_40KTO42499 + INC_42500TO44999,
         INC_45KTO49999 = INC_45KTO47499 + INC_47500TO49999,
         INC_50KTO59999 = INC_50KTO54999 + INC_55KTO59999
         ) %>%
  select(GISJOIN:wave, INC_60KTO74999:INC_50KTO59999) %>%
  relocate(GISJOIN, 
           wave, 
           INC_LT10K,
           INC_10KTO14999,
           INC_15KTO19999,
           INC_20KTO24999,
           INC_25KTO29999, 
           INC_30KTO34999,
           INC_35KTO39999, 
           INC_40KTO44999,
           INC_45KTO49999,
           INC_50KTO59999)
rm(census90inc)

## APPORTIONMENT
census90_bgp_cw <- read_csv(paste0(wd, 
                               cw_path,
                               cw_1990_bgp_path))


census90_bgp_cw <- census90_bgp_cw %>%
  select(bgp1990gj:bg2010ge, wt_hh) #%>%
  mutate(state_co_fips = str_sub(bgp1990gj, end = 8),
         county_sub_fips = str_sub(bgp1990gj, start = 9, end = 13),
         place_fips = str_sub(bgp1990gj, start = 14, end = 18),
         tract_fips = str_sub(bgp1990gj, start = 19, end = 24),
         bg_fips = str_sub(bgp1990gj, start = -1),
         bgp1990_GEOID = str_c(state_co_fips, tract_fips, bg_fips, sep = "")
         )

weighted_estimates <- census90inc_red %>%
  left_join(., census90_bgp_cw, by = c("GISJOIN" = "bgp1990gj")) %>%
  mutate(across(where(is.numeric) & !c(wt_hh), ~. * wt_hh)) %>%
  select(-wt_hh, -GISJOIN) 
rm(census90inc_red, census90_bgp_cw)

income_90_on_10_cbg <- weighted_estimates %>% 
  group_by(bg2010ge) %>%
  summarize(across(where(is.numeric), ~sum(.x))) %>%
  mutate(wave = "1990") %>%
  rename(GEOID = "bg2010ge") %>%
  relocate(GEOID, wave)

saveRDS(income_90_on_10_cbg, "income_90_on_10_cbg.rds")
rm(weighted_estimates, income_90_on_10_cbg)


#CLEAN & PROCESS 2000-----------------------------------------------------------
census00 <- read_csv(paste0(wd,
                            census00_path))

selected_00_vars <- c("GISJOIN",
                      "YEAR",
                      "STATE",
                      "STATEA",
                      "COUNTY",
                      "COUNTYA",
                      "TRACTA", 
                      "BLCK_GRPA",
                      "BLOCKA",
                      "FXS001",
                      "FYE001",   
                      "FYE002", 
                      "FYE003",
                      "FYE004",
                      "FYE005",
                      "FYE006",   
                      "FYE007",
                      "FYF001",
                      "FYF002",
                      "FYF003",
                      "FYF004",   
                      "FYF005",
                      "FYF006",
                      "FYF007",
                      "FYF008",
                      "FYF009",  
                      "FYF010",
                      "FYF011",
                      "FYF012",
                      "FYF013",
                      "FYF014",  
                      "FV5001",
                      "FV9001",
                      "FWA001",    
                      "FWA002",
                      "FWB001"
) 

renamed_00_vars <- c("GISJOIN",
                     "YEAR",
                     "STATE_NM",
                     "STATE_C",
                     "COUNTY_NM",
                     "COUNTY_C",
                     "TRACT_C", 
                     "BLCK_GRP_C",
                     "BLOCK_C",
                     "TOT_POP",
                     "WHITE",   
                     "BLACK", 
                     "AM_IND",
                     "ASIAN",
                     "PACIFIC",
                     "OTHER_RACE",   
                     "MULTIRACIAL",
                     "WHITE_NH",
                     "BLACK_NH",
                     "AM_IND_NH",
                     "ASIAN_NH",   
                     "PACIFIC_NH",
                     "OTHER_RACE_NH",
                     "MULTIRACIAL_NH",
                     "WHITE_HISP",
                     "BLACK_HISP",  
                     "AM_IND_HISP",
                     "ASIAN_HISP",
                     "PACIFIC_HISP",
                     "OTHER_RACE_HISP",
                     "MULTIRACIAL_HISP",  
                     "H_UNITS",
                     "OCCUPIED_UNITS",
                     "OWNER_OCC",
                     "RENTER_OCC",
                     "VACANT_UNITS"
) 

census00_red <- census00 %>% 
  select(all_of(selected_00_vars)) %>% 
  slice(-1) %>%
  rename_at(vars(GISJOIN:FWB001), ~ renamed_00_vars)

rm(census00)

saveRDS(census00_red, "census_blocks_2000.rds")
census00_red <- readRDS("census_blocks_2000.rds")


## HISPANIC---------------------------------------------------------------------
census00_hisp <- census00_red %>%
  select(STATE_C, COUNTY_C, TRACT_C, BLOCK_C, WHITE_HISP:MULTIRACIAL_HISP) %>%
  mutate(GEOID00 = str_c(STATE_C, COUNTY_C, TRACT_C, BLOCK_C, sep = "")) %>%
  select(-STATE_C:-BLOCK_C) %>%
  mutate(across(where(is.character) & !c(GEOID00), as.numeric)) %>%
  mutate(HISPANIC = rowSums(across(where(is.numeric)))) %>%
  select(GEOID00, HISPANIC)

cw_2000 <- read_csv(paste0(wd, 
                           cw_path, 
                           cw_2000_path))

weighted_estimates <- census00_hisp %>%
  left_join(., cw_2000, by = "GEOID00") %>%
  select(-PAREA) %>%
  mutate(across(where(is.numeric) & !c(WEIGHT), ~. * WEIGHT)) 

hisp_00_on_10_geog <- weighted_estimates %>%
  group_by(GEOID10) %>%
  summarize(HISPANIC_00 = sum(HISPANIC)
            )
rm(census00_hisp, weighted_estimates)
saveRDS(hisp_00_on_10_geog, "hisp_00_on_10_geog.rds")

## RACE WHITE - MULTIRACIAL-----------------------------------------------------
census00_race <- census00_red %>%
  select(STATE_C, COUNTY_C, TRACT_C, BLOCK_C, TOT_POP, WHITE_NH:MULTIRACIAL_NH) %>% 
  mutate(GEOID00 = str_c(STATE_C, COUNTY_C, TRACT_C, BLOCK_C, sep = "")) %>%
  mutate(across(ends_with("_NH"), as.numeric)) %>%
  mutate(TOT_POP = as.numeric(TOT_POP)) %>%
  select(-STATE_C:-BLOCK_C)

weighted_estimates <- census00_race %>%
  left_join(., cw_2000, by = "GEOID00") %>%
  select(-PAREA) %>%
  mutate(across(where(is.numeric) & !c(WEIGHT), ~. * WEIGHT))

saveRDS(weighted_estimates, "race_00_weighted_estimates_for_10geog.rds")

full_census_00_race_on_10_geog <- readRDS("race_00_weighted_estimates_for_10geog.rds")


## FULL RACE VARS 00 ESTIMATES ON 2010 GEOG
full_census_90_race_on_10_geog <- readRDS("blocks_race_90_on_10_blocks.rds")
census00_white_on_10_geog <- full_census_00_race_on_10_geog %>% 
  group_by(GEOID10) %>%
  summarize(TOT_POP_00 = sum(TOT_POP),
            WHITE_NH_00 = sum(WHITE_NH)
  )

census00_black_am_ind_on_10_geog <- full_census_00_race_on_10_geog %>% 
  group_by(GEOID10) %>%
  summarize(BLACK_NH_00 = sum(BLACK_NH),
            AM_IND_NH_00 = sum(AM_IND_NH)
  )

census_asian_oth_multi_on_10_geog <- full_census_00_race_on_10_geog %>% 
  group_by(GEOID10) %>%
  summarize(ASIAN_NH_00 = sum(ASIAN_NH),
            OTHER_RACE_NH_00 = sum(OTHER_RACE_NH),
            MULTIRACIAL_NH_00 = sum(MULTIRACIAL_NH)
  )
rm(full_census_00_race_on_10_geog)

hisp_00_on_10_geog <- readRDS("hisp_00_on_10_geog.rds")
rm(DF_obj)

census00_on_10_geog <- census00_white_on_10_geog %>%
  full_join(., census00_black_am_ind_on_10_geog, by = "GEOID10")
saveRDS(census00_on_10_geog, "blocks_race_00_on_10_blocks.rds")

census00_on_10_geog <- census00_on_10_geog %>%
  full_join(., census_asian_oth_multi_on_10_geog, by = "GEOID10")
saveRDS(census00_on_10_geog, "blocks_race_00_on_10_blocks.rds")

census00_on_10_geog <- census00_on_10_geog %>%
  full_join(., hisp_00_on_10_geog, by = "GEOID10")
saveRDS(census00_on_10_geog, "blocks_race_00_on_10_blocks.rds")

rm(census_asian_oth_multi_on_10_geog, census00_black_am_ind_on_10_geog,
   census00_white_on_10_geog, hisp_00_on_10_geog)

## FULL RACE VARS 00 ESTIMATES ON 2010 CBGS
full_census00_race_on_10_cbg <- census00_on_10_geog %>%
  mutate(CBG_10 = str_sub(GEOID10, end = -4)) %>%
  select(-GEOID10) %>%
  group_by(CBG_10) %>%
  summarize(TOT_POP_00 = sum(TOT_POP_00),
            WHITE_NH_00 = sum(WHITE_NH_00),
            BLACK_NH_00 = sum(BLACK_NH_00),
            AM_IND_NH_00 = sum(AM_IND_NH_00),
            ASIAN_NH_00 = sum(ASIAN_NH_00),
            OTHER_RACE_NH_00 = sum(OTHER_RACE_NH_00),
            MULTIRACIAL_NH_00 = sum(MULTIRACIAL_NH_00),
            HISPANIC_00 = sum(HISPANIC_00)
  )
saveRDS(full_census00_race_on_10_cbg, "race_00_on_10_cbg.rds")

#################
# HOUSING DATA
#################
census00_housing <- census00_red %>%
  select(STATE_C, COUNTY_C, TRACT_C, BLOCK_C, H_UNITS:VACANT_UNITS) %>%
  mutate(GEOID00 = str_c(STATE_C, COUNTY_C, TRACT_C, BLOCK_C, sep = "")) %>%
  mutate(across(where(is.character) & !c(GEOID00), as.numeric)) %>%
  left_join(., cw_2000, by = "GEOID00") %>%
  select(-STATE_C:-BLOCK_C, -PAREA)
rm(census00_red, cw_2000)

weighted_estimates <- census00_housing %>%
  mutate(across(where(is.numeric) & !c(WEIGHT), ~. * WEIGHT)) %>%
  select(-WEIGHT, -GEOID00) 
rm(census00_housing)

cbg_00_census <- weighted_estimates %>%
  select(GEOID10, H_UNITS:VACANT_UNITS) %>%
  mutate(CBG_10 = str_sub(GEOID10, end = -4)) %>%
  select(-GEOID10) %>%
  group_by(CBG_10) %>%
  summarize(across(where(is.numeric), ~sum(.x)))

saveRDS(cbg_00_census, "housing_00_on_10_cbg.rds")
rm(weighted_estimates, cbg_00_census)


####################################
# AGGREGATE 2000 INCOME INTO CBGs
###################################
census00inc <- read_csv(paste0(wd, 
                               censusinc_path,
                               inc00_path))

renamed_90_inc_vars <- c("GISJOIN", 
                         "YEAR",
                         "INC_LT5K",
                         "INC_5KTO9999",
                         "INC_10KTO12499",
                         "INC_12500TO14999",
                         "INC_15KTO17499",
                         "INC_17500TO19999",
                         "INC_20KTO22499",
                         "INC_22500TO24999",
                         "INC_25KTO274999",
                         "INC_27500TO29999",
                         "INC_30KTO32499",
                         "INC_32500TO34999",
                         "INC_35KTO37499",
                         "INC_37500TO39999",
                         "INC_40KTO42499",
                         "INC_42500TO44999",
                         "INC_45KTO47499",
                         "INC_47500TO49999",
                         "INC_50KTO54999",
                         "INC_55KTO59999",
                         "INC_60KTO74999",
                         "INC_75KTO99999",
                         "INC_100KTO124999",
                         "INC_125KTO149999",
                         "INC_150KPLUS"
)

census90inc_red <- census90inc %>% 
  slice(-1) %>%
  select(GISJOIN:YEAR, E4T001:E4T025) %>%
  rename_at(vars(GISJOIN:E4T025), ~ renamed_90_inc_vars) %>%
  mutate(across(INC_LT5K:INC_150KPLUS, as.numeric)) %>%
  rename(wave = "YEAR") %>%
  mutate(INC_LT10K = INC_LT5K + INC_5KTO9999,
         INC_10KTO14999 = INC_10KTO12499 + INC_12500TO14999,
         INC_15KTO19999 = INC_15KTO17499 + INC_17500TO19999,
         INC_20KTO24999 = INC_20KTO22499 + INC_22500TO24999,
         INC_25KTO29999 = INC_25KTO274999 + INC_27500TO29999, 
         INC_30KTO34999 = INC_30KTO32499 + INC_32500TO34999,
         INC_35KTO39999 = INC_35KTO37499 + INC_37500TO39999, 
         INC_40KTO44999 = INC_40KTO42499 + INC_42500TO44999,
         INC_45KTO49999 = INC_45KTO47499 + INC_47500TO49999,
         INC_50KTO59999 = INC_50KTO54999 + INC_55KTO59999
  ) %>%
  select(GISJOIN:wave, INC_60KTO74999:INC_50KTO59999) %>%
  relocate(GISJOIN, 
           wave, 
           INC_LT10K,
           INC_10KTO14999,
           INC_15KTO19999,
           INC_20KTO24999,
           INC_25KTO29999, 
           INC_30KTO34999,
           INC_35KTO39999, 
           INC_40KTO44999,
           INC_45KTO49999,
           INC_50KTO59999)
rm(census90inc)

## APPORTIONMENT
census90_bgp_cw <- read_csv(paste0(wd, 
                                   cw_path,
                                   cw_1990_bgp_path))


census90_bgp_cw <- census90_bgp_cw %>%
  select(bgp1990gj:bg2010ge, wt_hh) #%>%
mutate(state_co_fips = str_sub(bgp1990gj, end = 8),
       county_sub_fips = str_sub(bgp1990gj, start = 9, end = 13),
       place_fips = str_sub(bgp1990gj, start = 14, end = 18),
       tract_fips = str_sub(bgp1990gj, start = 19, end = 24),
       bg_fips = str_sub(bgp1990gj, start = -1),
       bgp1990_GEOID = str_c(state_co_fips, tract_fips, bg_fips, sep = "")
)

weighted_estimates <- census90inc_red %>%
  left_join(., census90_bgp_cw, by = c("GISJOIN" = "bgp1990gj")) %>%
  mutate(across(where(is.numeric) & !c(wt_hh), ~. * wt_hh)) %>%
  select(-wt_hh, -GISJOIN) 
rm(census90inc_red, census90_bgp_cw)

income_90_on_10_cbg <- weighted_estimates %>% 
  group_by(bg2010ge) %>%
  summarize(across(where(is.numeric), ~sum(.x))) %>%
  mutate(wave = "1990") %>%
  rename(GEOID = "bg2010ge") %>%
  relocate(GEOID, wave)

saveRDS(income_90_on_10_cbg, "income_90_on_10_cbg.rds")
rm(weighted_estimates, income_90_on_10_cbg)
