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
  ) 


## Process street segments
streets_lines <- sf_streets_csv %>%
         st_as_sf(., wkt = "line")


## VISUALIZE
ggplot() + 
  geom_sf(data = streets_lines) #+ 
  geom_polygon(data = )

