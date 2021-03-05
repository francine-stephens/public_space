#-------------------------------------
# DESCRIPTIVE ANALYSIS
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/2/21
# LAST UPDATED: 3/5/21
#-------------------------------------

#SET-UP-------------------------------------------------------------------------
## LIBRARIES
packages <- c(
  "readr",
  "tidyverse",
  "sf",
  "ggplot2",
  "tmap",
  "tigris",
  "censusapi", 
  "tidycensus"
)
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Projects/public_space")
wd <- getwd()
green_space_path <- "/green_space"
census_data_path <- "/census"
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository"
shp2010_path <- "/2010USA_CensusGeog_Shp"
cbsa_path <- "/tl_2010_us_cbsa10/"
metdiv_path <- "/tl_2010_us_metdiv10/"
cbg10_path <- "/us_blck_grp_2010/"
places10_path <- "/tl2010_us_place_2010/"

## DATA IMPORTS
# import green space merged data
evi_all_decades_cbg

# import pop data
race_all_decades_cbg <- readRDS(paste0(wd, 
                         census_data_path, 
                         "/all_decades_race_on_cbg10.rds"))

# import cbg to higher geog links 
cbg_higher_geog_ids <- readRDS(paste0(shp_repo,
                                      "/cbg_full_geog_identifers.rds"))
# import special subset census geog
top100_cbsa <- read_csv(paste0(shp_repo, 
                               "/top100_cbsa_2010bounds.csv"))
top100_places <- read_csv(paste0(shp_repo, 
                                 "/top100_places_2010bounds.csv"))


#DATA PREPARATION---------------------------------------------------------------
## FIX THIS!
cbg10_censusdata <- race_all_decades_cbg %>%
  left_join(., cbg_10_census, by = c("GEOID10" = "CBG_10")) %>%
  mutate_at(vars(WHITE_NH_10:HISPANIC_10), funs("percent" = (./TOT_POP_10) * 100))
  
cbg10_pop_evi_joined <- EVI_allyears_Cbg10_sf %>%
  st_set_geometry(NULL) %>%
  left_join(., cbg10_censusdata, by = c("GEOID10")) %>%
  select(GEOID10, TOT_POP_10, WHITE_NH_10_percent:HISPANIC_10_percent, evi90:evi20)


corr_v1 <- cbg10_pop_evi_joined %>%
  select(-GEOID10) %>%
  cor(., use = "complete.obs")
round(corr_v1, 2)
corrplot(corr_v1, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, sig.level = 0.01, insig = "blank")


#DESCRIPTIVE STATS------------------------------------------------------------
## CBSA
cbsa10bounds_evi_sumstats <- EVI_allyears_Cbg10_cbsa_sf %>%
  mutate(evi_chg_10to20 = (((evi20 - evi10)/evi10) * 100),
         evi_chg_00to10 = (((evi10 - evi00)/evi00) * 100),
         evi_chg_90to00 = (((evi00 - evi90)/evi90) * 100),
         evi_chg_90to20 = (((evi20 - evi90)/evi90) * 100)
         ) %>%
  group_by(CBSAFP10, NAMELSAD10) %>%
  summarize(across(starts_with("evi"), ~mean(.x, na.rm = TRUE))
            ) %>%
  mutate(metro_area = if_else(str_detect(NAMELSAD10, "Metro Area"),
                              1,
                              0)
  )

  ## View rankings
cbsa10bounds_evichg_sumstats %>%
  filter(metro_area == 1) %>% 
  arrange(-avg_evi_chg_10to20) 

## PLACE
place10_evi_sumstats <- EVI_allyears_Cbg10_places %>% 
  mutate(evi_chg_10to20 = (((evi20 - evi10)/evi10) * 100),
         evi_chg_00to10 = (((evi10 - evi00)/evi00) * 100),
         evi_chg_90to00 = (((evi00 - evi90)/evi90) * 100),
         evi_chg_90to20 = (((evi20 - evi90)/evi90) * 100)
  ) %>%
  group_by(PLACEFP10, PLACE_NM) %>%
  summarize(across(starts_with("evi"), ~mean(.x, na.rm = TRUE))
  ) %>%
  arrange(-evi_chg_90to20) %>%
  mutate(top100_places = if_else(PLACEFP10 %in% top100_places_2010$PLACEFP10,
                                 1, 
                                 0))

place10_evi_sumstats %>%
  filter(top100_places == 1) %>%
  arrange(-evi20)

## VISUALIZATION
asheville_cbg <- EVI_allyears_Cbg10_cbsa_sf%>%
  filter(CBSAFP10 == "11700")
tucson_cbg <- EVI_allyears_Cbg10_cbsa_sf %>%
  filter(CBSAFP10 ==  "46060")

EVI90_Cbg10_sf %>%
  filter(GISJOIN %in% tucson_cbg$GISJOIN) %>%
tm_shape(.) +
    tm_polygons("mean", palette="Greens")
