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
  "scales",
  "ggcorrplot",
  "tmap",
  "tigris",
  "censusapi", 
  "tidycensus",
  "corrplot",
  "RColorBrewer"
)
lapply(packages, library, character.only = T)

## PATHS
setwd("~/Projects/public_space")
wd <- getwd()
green_space_path <- "/green_space/gee/"
census_data_path <- "/census"
shp_repo <- "C:/Users/Franc/Documents/Shapefile_Repository"
shp2010_path <- "/2010USA_CensusGeog_Shp"
cbsa_path <- "/tl_2010_us_cbsa10/"
metdiv_path <- "/tl_2010_us_metdiv10/"
cbg10_path <- "/us_blck_grp_2010/"
places10_path <- "/tl2010_us_place_2010/"

# APIs
census_api_key("99ccb52a629609683f17f804ca875115e3f0804c",  overwrite = T)
Sys.setenv(CENSUS_KEY="99ccb52a629609683f17f804ca875115e3f0804c")


## DATA IMPORTS
evi_all_decades_cbg <- readRDS(paste0(wd, 
                                      green_space_path,
                                      "evi_all_years_on_2010cbg.rds"))

evi_area_all_decades_cbg <- readRDS(paste0(wd,
                                           green_space_path,
                                           "evi_area_all_years_on_2010cbg.rds"))

race_all_decades_cbg <- readRDS(paste0(wd, 
                         census_data_path, 
                         "/all_decades_race_on_cbg10.rds"))

cbg10 <- st_read(paste0(shp_repo,
                        shp2010_path,
                        cbg10_path,
                        "US_blck_grp_2010.shp"), 
                 quiet = F)

counties <- get_decennial(geography = "county", 
                          variables = "P001001", 
                          year = 2010) 

places <- get_decennial(geography = "place", 
                        variables = "P001001", 
                        year = 2010) 

cbg_higher_geog_ids <- readRDS(paste0(shp_repo,
                                      "/cbg_full_geog_identifers.rds"))
top100_cbsa <- read_csv(paste0(shp_repo, 
                               "/top100_cbsa_2010bounds.csv"))
top100_places <- read_csv(paste0(shp_repo, 
                                 "/top100_places_2010bounds.csv"))


#DATA PREPARATION---------------------------------------------------------------
## MERGE ALL GEOIDs
top100_cbsa <- top100_cbsa %>%
  mutate(CBSAFP = as.character(CBSAFP)) %>%
  select(-CBSA_NM) %>%
  rename(top100_cbsa="top_100", cbsa_rank_pop_size="rank_pop_size")

top100_places <- top100_places %>%
  select(-PLACE_NM) %>%
  rename(top100_place="top_100", place_rank_pop_size="rank_pop_size")

counties <- counties %>%
  select(PLACEFP_DERIVED="GEOID", PLACE_DERIVED_NM="NAME")

cbg_all_geoids <- cbg10 %>%
  st_set_geometry(NULL) %>%
  select(GEOID10, GISJOIN) %>%
  left_join(., cbg_higher_geog_ids, by=c("GEOID10", "GISJOIN")) %>%
  mutate(PLACEFP_DERIVED = if_else(is.na(PLACEID),
                                   str_sub(GEOID10, end=5),
                                   PLACEID)) %>% 
  left_join(., counties, by = "PLACEFP_DERIVED") %>%
  mutate(PLACE_DERIVED_NM=coalesce(PLACE_DERIVED_NM, PLACE_NM)) %>%
  relocate(PLACEFP_DERIVED, .after=PLACEID) %>%
  relocate(PLACE_DERIVED_NM, .after=PLACE_NM) %>%
  left_join(., top100_places, by=c("PLACEID"="PLACEFP")) %>%
  left_join(., top100_cbsa, by=c("CBSAFP10"="CBSAFP")) 

## MERGE ALL DEMOGRAPHIC & GREEN SPACE DATA

# PERCENT
race_pct_all_decades_cbg <- race_all_decades_cbg %>%
  mutate_at(vars(WHITE_NH_10:HISPANIC_10), funs("percent" = (./TOT_POP_10) * 100)) %>%
  mutate_at(vars(WHITE_NH_00:HISPANIC_00), funs("percent" = (./TOT_POP_00) * 100)) %>%
    select(CBG_10, TOT_POP_10, TOT_POP_00, WHITE_NH_10_percent:HISPANIC_00_percent)

cbg_geog_evi_race_all_years <- cbg_all_geoids %>%
  left_join(., evi_all_decades_cbg, by="GISJOIN") %>%
  left_join(., race_pct_all_decades_cbg, by=c("GEOID10"="CBG_10")) %>%
  left_join(., evi_area_all_decades_cbg, by="GISJOIN")



## DECADE SUBSETS
cbg_2010_data_subset <- cbg_geog_evi_race_all_years %>%
  select(where(is_numeric) & contains("10") & !contains("top100"))

cbg_2000_data_subset <- cbg_geog_evi_race_all_years %>%
  select(where(is_numeric) & contains("00") & !contains("top100"))

  ## INSERT 90
  ## INSERT 20?

## CHANGE SUBSET
cbg_evi_race_chg <-  cbg_geog_evi_race_all_years %>% 
  mutate_each(funs(.*10000), starts_with("evi")) %>%
  mutate(evi_chg_10to20 = (((evi20 - evi10)/evi10) * 100),
         evi_chg_00to10 = (((evi10 - evi00)/evi00) * 100),
         evi_chg_90to00 = (((evi00 - evi90)/evi90) * 100),
         evi_chg_90to20 = (((evi20 - evi90)/evi90) * 100), 
         evi_area_chg_90to10 = (((area10 - area90)/area90) * 100),
         evi_area_chg_00to10 = (((area10 - area00)/area00) * 100),
         WHITE_NH_chg_00to10 = (
           ((WHITE_NH_10_percent - WHITE_NH_00_percent)/WHITE_NH_00_percent)*100),
         BLACK_NH_chg_00to10 = (
           ((BLACK_NH_10_percent - BLACK_NH_00_percent)/BLACK_NH_00_percent)*100),
         ASIAN_NH_chg_00to10 = (
           ((ASIAN_NH_10_percent - ASIAN_NH_00_percent)/ASIAN_NH_00_percent)*100),
         AM_IND_NH_chg_00to10 = (
           ((AM_IND_NH_10_percent - AM_IND_NH_00_percent)/AM_IND_NH_00_percent)*100),
         OTHER_RACE_NH_chg_00to10 = (
           ((OTHER_RACE_NH_10_percent - OTHER_RACE_NH_00_percent)/OTHER_RACE_NH_00_percent)*100),
         MULTIRACIAL_NH_chg_00to10 = (
           ((MULTIRACIAL_NH_10_percent - MULTIRACIAL_NH_00_percent)/MULTIRACIAL_NH_00_percent)*100),
         HISPANIC_chg_00to10 = (
           ((HISPANIC_10_percent - HISPANIC_00_percent)/HISPANIC_00_percent)*100),
         TOT_POP_chg_00to10 = (
           ((TOT_POP_10 - TOT_POP_00)/TOT_POP_00)*100)
  ) %>%
  select(where(is_numeric) & contains("_chg_00to10")) 
is.na(cbg_evi_race_chg) <- sapply(cbg_evi_race_chg, is.infinite)
is.na(cbg_evi_race_chg) <- sapply(cbg_evi_race_chg, is.nan)

#DESCRIPTIVE STATS--------------------------------------------------------------
## CORRELATIONS
##2010
corr10 <-  round(cor(cbg_2010_data_subset, use="complete.obs"), 2)
pmat10 <- cor_pmat(cbg_2010_data_subset, use="complete.obs")

ggcorrplot(corr10,
           hc.order=T,
           type="lower",
           lab=T,
           #p.mat = pmat10,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

##2000
corr00 <- round(cor(cbg_2000_data_subset, use="complete.obs"), 2)
pmat00 <- cor_pmat(cbg_2000_data_subset, use="complete.obs")

ggcorrplot(corr00,
           hc.order=T,
           type="lower",
           lab=T,
           #p.mat = pmat00,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


#REPLICATE SAPORITO & CASEY DESCRIPTIVES----------------------------------------
# COUNTS
cbg_for_replication <- cbg_all_geoids %>%
  left_join(., race_all_decades_cbg, by=c("GEOID10"="CBG_10"))  %>%
  left_join(., evi_all_decades_cbg, by="GISJOIN") %>%
  select(-TOT_POP_00:-evi00, -evi20)

cbg_for_rep_wtd_evi <- cbg_for_replication %>%
  mutate(evi10 = evi10 * 10000) %>%
  mutate_each(funs(.*evi10), ends_with("_10"))
  
sumstats_race_wtd_evi <- cbg_for_rep_wtd_evi %>%
  select(TOT_POP_10:HISPANIC_10) %>%
  summarise_all(list(mean=mean, sd=sd, median=median), na.rm=T)

sumstats_race_wtd_evi_mean <- cbg_for_rep_wtd_evi %>%
  select(TOT_POP_10:HISPANIC_10) %>%
  summarise_all(mean, na.rm=T) %>%
  mutate(statistic = "mean")

sumstats_race_wtd_evi_sd <- cbg_for_rep_wtd_evi %>%
  select(TOT_POP_10:HISPANIC_10) %>%
  summarise_all(sd, na.rm=T) %>%
  mutate(statistic = "sd")

sumstats_race_wtd_evi_med <- cbg_for_rep_wtd_evi %>%
  select(TOT_POP_10:HISPANIC_10) %>%
  summarise_all(median, na.rm=T) %>%
  mutate(statistic = "median")

sumstats_race_wtd_evi <- rbind(
  sumstats_race_wtd_evi_mean,
  sumstats_race_wtd_evi_sd, 
  sumstats_race_wtd_evi_med) %>%
  relocate(statistic) %>%
  select(-TOT_POP_10) %>%
  pivot_longer(
    ends_with("_10"),
    names_to = "race",
    values_to = "value")
  
# GRAPH OF CONTACT WITH GREEN SPACE BY RACE
sumstats_race_wtd_evi %>% 
  filter(statistic == "median") %>% 
  ggplot(., aes(x = value, y = reorder(race, value), label = comma(value, accuracy=1))) +
  geom_segment(aes(yend = race), xend = 0, colour = "grey80") +
  geom_point(size = 4, aes(color=factor(race)), alpha=0.6) + #color = "red"
  scale_colour_brewer(palette = "Set2") +
  geom_text(size = 2.5, vjust= -0.5, fontface = "bold") +  
  labs(title = "Median EVI by Race",
       x = "Median EVI",
       y = "",
       color = "Race",
       caption = "EVI calculated using Landsat 7 imagery") + 
  scale_x_continuous(labels = scales::comma, breaks=pretty_breaks(n=10)) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust=0),
    #panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed")
  ) 

# 2) SUM RACE-WEIGHTED EVI TO PLACES
race_wtd_evi_summed_to_place <- cbg_for_rep_wtd_evi %>% 
  select(PLACEFP_DERIVED, PLACE_DERIVED_NM, WHITE_NH_10:HISPANIC_10) %>%
  group_by(PLACEFP_DERIVED, PLACE_DERIVED_NM) %>%
  summarize_all(sum, na.rm=T) %>%
  rename_at(vars(-PLACEFP_DERIVED, -PLACE_DERIVED_NM),function(x) paste0(x,"_EVI"))


# 3) DIVIDE RACE-WEIGHTED EVI BY TOTAL # RACE IN PLACE
race_counts_per_place <- cbg_for_replication %>%
  select(PLACEFP_DERIVED, PLACE_DERIVED_NM, WHITE_NH_10:HISPANIC_10) %>%
  group_by(PLACEFP_DERIVED, PLACE_DERIVED_NM) %>%
  summarize_all(sum, na.rm=T)

green_exposure_race_diffs_by_place <- race_wtd_evi_summed_to_place %>%
  left_join(., race_counts_per_place, by = c("PLACEFP_DERIVED", "PLACE_DERIVED_NM")) %>%
  mutate(WHITE_NH_EVI = WHITE_NH_10_EVI/WHITE_NH_10,
         BLACK_NH_EVI = BLACK_NH_10_EVI/BLACK_NH_10,
         ASIAN_NH_EVI = ASIAN_NH_10_EVI/ASIAN_NH_10,
         HISPANIC_EVI = HISPANIC_10_EVI/HISPANIC_10) %>%
  mutate(BLACK_WHITE_EVI_DIFF = (BLACK_NH_EVI - WHITE_NH_EVI),
         ASIAN_WHITE_EVI_DIFF = (ASIAN_NH_EVI - WHITE_NH_EVI),
         HISPANIC_WHITE_EVI_DIFF = (HISPANIC_EVI - WHITE_NH_EVI),
         BLACK_ASIAN_EVI_DIFF = (BLACK_NH_EVI - ASIAN_NH_EVI),
         BLACK_HISPANIC_EVI_DIFF = (BLACK_NH_EVI - HISPANIC_EVI),
         HISPANIC_ASIAN_EVI_DIFF = (HISPANIC_EVI - ASIAN_NH_EVI)
         ) %>%
  select(PLACEFP_DERIVED, PLACE_DERIVED_NM, BLACK_WHITE_EVI_DIFF:HISPANIC_ASIAN_EVI_DIFF)

## EVI DIFFERENCES
green_exposure_race_diffs_by_place %>%
  ungroup() %>%
  select(-PLACEFP_DERIVED, -PLACE_DERIVED_NM) %>%
  summarize_all(mean, na.rm=T)

green_exposure_race_diffs_by_place %>%
  ungroup() %>%
  select(-PLACEFP_DERIVED, -PLACE_DERIVED_NM) %>%
  summarize_all(sd, na.rm=T)

## CREATE A CUT OF ONLY PLACES WITH MORE THAN 25,000 PEOPLE
places_pop_over25k <- places %>%
  filter(value > 25000)

green_exposure_race_diffs_by_place %>%
  filter(PLACEFP_DERIVED %in% places_pop_over25k$GEOID) %>%
  ungroup() %>%
  select(-PLACEFP_DERIVED, -PLACE_DERIVED_NM) %>%
  summarize_all(mean, na.rm=T)

green_exposure_race_diffs_by_place %>%
  filter(PLACEFP_DERIVED %in% places_pop_over25k$GEOID) %>%
  ungroup() %>%
  select(-PLACEFP_DERIVED, -PLACE_DERIVED_NM) %>%
  summarize_all(sd, na.rm=T)

# CREATE DISIM. MEASURES--------------------------------------------------------

dissim_by_place <- cbg_for_replication %>% 
  select(PLACEFP_DERIVED, PLACE_DERIVED_NM, WHITE_NH_10:HISPANIC_10) %>%
  rename_at(vars(-PLACEFP_DERIVED, -PLACE_DERIVED_NM),function(x) paste0(x,"_CBG")) %>%
  left_join(., race_counts_per_place, by = c("PLACEFP_DERIVED", "PLACE_DERIVED_NM")) %>%
  mutate(diss_bw=abs(BLACK_NH_10_CBG/BLACK_NH_10 - WHITE_NH_10_CBG/WHITE_NH_10),
         diss_hw=abs(HISPANIC_10_CBG/HISPANIC_10 - WHITE_NH_10_CBG/WHITE_NH_10)) %>%
  group_by(PLACEFP_DERIVED, PLACE_DERIVED_NM) %>%
  summarise(dissim_bw= .5*sum(diss_bw, na.rm=T),
            dissim_hw= .5*sum(diss_hw, na.rm=T)
            )

full_place_data <- dissim_by_place %>%
  left_join(.,
            green_exposure_race_diffs_by_place,
            by = c("PLACEFP_DERIVED", "PLACE_DERIVED_NM"))

reg_bw_diff_evi <- lm(BLACK_WHITE_EVI_DIFF ~ dissim_bw, data=full_place_data)
summary(reg_bw_diff_evi ) # show results



#DESCRIPTIVE STATS--------------------------------------------------------------


###########
# PLACE
###########
place10_evi_sumstats <- cbg_geog_evi_race_all_years %>% 
  group_by(PLACEFP_DERIVED, PLACE_DERIVED_NM) %>% 
  summarize(across(starts_with("evi"), ~mean(.x, na.rm = TRUE)),
            across(starts_with("area"), ~sum(.x, na.rm = TRUE)),
            across(ends_with("percent"), ~mean(.x, na.rm = TRUE))
  ) #%>%
  mutate(evi_chg_10to20 = (((evi20 - evi10)/evi10) * 100),
         evi_chg_00to10 = (((evi10 - evi00)/evi00) * 100),
         evi_chg_90to00 = (((evi00 - evi90)/evi90) * 100),
         evi_chg_90to20 = (((evi20 - evi90)/evi90) * 100)
  ) 
  #arrange(-evi_chg_90to20) %>%
 # mutate(top100_places = if_else(PLACEFP10 %in% top100_places_2010$PLACEFP10, 1, 0))
place_for_corrs <- place10_evi_sumstats %>%
  ungroup() %>%
  select(where(is.numeric))
corrplace <-  round(cor(place_for_corrs, use="complete.obs"), 2)
pmatplace <- cor_pmat(place_for_corrs, use="complete.obs")
  
  ggcorrplot(corrplace,
             hc.order=T,
             type="lower",
             lab=T,
             #p.mat = pmat10,
             ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"))
  
  
place10_evi_sumstats %>%
  filter(top100_places == 1) %>%
  arrange(-evi20)


#############
# CBSA
#############
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




## VISUALIZATION
asheville_cbg <- EVI_allyears_Cbg10_cbsa_sf%>%
  filter(CBSAFP10 == "11700")
tucson_cbg <- EVI_allyears_Cbg10_cbsa_sf %>%
  filter(CBSAFP10 ==  "46060")

EVI90_Cbg10_sf %>%
  filter(GISJOIN %in% tucson_cbg$GISJOIN) %>%
tm_shape(.) +
    tm_polygons("mean", palette="Greens")
