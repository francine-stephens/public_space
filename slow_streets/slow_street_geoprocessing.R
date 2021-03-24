#-------------------------------------
# GEOPROCESSING
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/21/21
# LAST UPDATED: 3/22/21
#-------------------------------------

# SET-UP------------------------------------------------------------------------
## LIBRARIES
packages <- c(
  "readr",
  "tidyverse",
  "sf",
  "RSocrata",
  "ggplot2",
  "leaflet",
  "mapboxapi",
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

## PARAMETERS
socrata_token <- "EZnaKCxOX6zj0uyTkD340TWLh"
email <- "fis@stanford.edu"
pword <- "SFdata2017"

## PATHS
setwd("~/Projects/public_space/slow_streets")
wd <- getwd()
#sf_shapes_path <- "C:/Users/Franc/Documents/Shapefile_Repository/san_francisco_shapes/"


## DATA IMPORT
ss_intersections <- read_csv(paste0(wd,
                                "/sf_slow_streets.csv")
                         )

sf_street_intersections <- read.socrata("https://data.sfgov.org/resource/jfxm-zeee.json", 
                                        app_token = socrata_token, 
                                        email = email,
                                        password  = pword
)

sf_streets_csv <- read.socrata("https://data.sfgov.org/resource/3psu-pn9h.csv",
                               app_token = socrata_token,
                               email     = email,
                               password  = pword)

# GEOPROCESSING-----------------------------------------------------------------
## Process & Intersections (points)
ss_intersections_sf <- ss_intersections %>% 
  separate(intersection, c("st1", "st2"), sep = " & ") %>%
  mutate(across(starts_with("st"), toupper)) %>% 
  left_join(., sf_street_intersections, by = c("st1" = "street_name_1", 
                                               "st2" = "street_name_2")
  ) %>% 
  select_at(vars(-contains("_coord"))) %>%
  filter(id.y != "244828",
         id.y != "250079") %>%
  st_as_sf(., coords=c("longitude", "latitude"), crs=4326)

leaflet() %>% 
  addMapboxTiles(
    style_id = "streets-v11",
    username = "mapbox"
  ) %>%
  addCircleMarkers(
    data = ss_intersections_sf,
    radius = 1,
    label = ~name, 
  ) %>%
  addPolylines(data = ss_intersections_sf %>% 
                 group_by(name) %>%
                 summarise(do_union = FALSE) %>%
                 st_cast("LINESTRING")
               )

street_names <- ss_intersections %>%
  select(name) %>%
  mutate(name = toupper(name)) %>%
  unique()

## Process street segments
streets_lines <- sf_streets_csv %>%
         st_as_sf(., wkt = "line")

  #T2 
segments_to_be_filtered <- ss_intersections %>%
  slice(1:8) %>%
  separate(intersection, c("st1", "st2"), sep = " & ") %>%
  group_by(name) %>%
  mutate(st_t = lead(st2)) %>%
  filter(!is.na(st_t)) %>%
  mutate(across(starts_with("st"), toupper))
  
streets_lines %>%
  filter(streetname %in% segments_to_be_filtered$st1) %>%
  which(.$f_st %in% segments_to_be_filtered$st2)

  #T1
ss_20th_ave <- streets_lines %>%
  filter(streetname == "20TH AVE" & cnn %in% 1007000:1011000) %>%
  arrange(cnn)

ss_20th_st <- streets_lines %>%
  filter(streetname == "20TH ST" & cnn %in% 1044000:1057000) %>%
  arrange(cnn)

ss_23rd_ave <- streets_lines %>%
  filter(streetname == "23RD AVE" & cnn %in% 1211000:1216000) %>%
  arrange(cnn)

ss_41st_ave <- streets_lines %>%
  filter(streetname == "41ST AVE" & cnn %in% 1910000:1923000) %>%
  arrange(cnn)

ss_arkansas_st <- streets_lines %>%
  filter(streetname == "ARKANSAS ST" & cnn %in% 2436000:2442000) %>%
  arrange(cnn)

ss_arlington_st <- streets_lines %>%
  filter(streetname == "ARLINGTON ST" & cnn %in% 2447000:2453000) %>%
  arrange(cnn)

ss_cabrillo_st <- streets_lines %>%
  filter(streetname == "CABRILLO ST" & cnn %in% 3492000:3511000) %>%
  arrange(cnn)

ss_chenery_st <- streets_lines %>%
  filter(streetname == "CHENERY ST" & cnn %in% 3930000:3931000) %>%
  arrange(cnn)
  
ss_clay_st <- streets_lines %>%
  filter(streetname == "CLAY ST" & cnn %in% 4113000:4126000) %>%
  arrange(cnn)



ss_mendell_st <- streets_lines %>%
  filter(streetname == "MENDELL ST" ) %>%
  arrange(cnn)
ss_excelsior_st <- streets_lines %>%
  filter(streetname == "EXCELSIOR AVE" & f_st != "MISSION ST") %>%
  arrange(cnn)
ss_cayuga_ave <- streets_lines %>%
  filter(streetname == "CAYUGA AVE" ) %>%
  arrange(cnn)

ss_armstrong_kalmanovitz <- streets_lines %>%
  filter(
    streetname == "NEWHALL ST" | streetname == "KALMANOVITZ ST" | streetname == "BITTING AVE" | streetname == "ARMSTRONG AVE") %>%
  arrange(cnn)
  filter(
    streetname == "MABINI ST" | 
      streetname == "FOLSOM ST" | 
      streetname == "BONIFACIO ST" | 
      streetname == "TANDANG SORA" | 
      streetname == "RIZAL ST" | 
      streetname == "LAPU-LAPU ST" | 
      streetname == "HARRISON ST"
    ) %>%
  arrange(cnn)

ss_hollister_ave <- streets_lines %>%
  filter(streetname == "HOLLISTER AVE") %>%
  arrange(cnn)

ss_lapulapu_et_al <- streets_lines %>%
  filter(
    streetname == "MABINI ST" | 
      streetname == "BONIFACIO ST" | 
      streetname == "TANDANG SORA" | 
      streetname == "RIZAL ST" | 
      streetname == "LAPU-LAPU ST" 
  ) %>%
  arrange(streetname, cnn)

ss_scotia_silver_ave <- streets_lines %>%
  filter(
    streetname == "SCOTIA AVE" | 
      streetname == "SILVER AVE" | 
      streetname == "THOMAS AVE"
  ) %>%
  arrange(streetname, cnn)


## VISUALIZE
ggplot() + 
  geom_sf(data = streets_lines) #+ 
  geom_polygon(data = )

