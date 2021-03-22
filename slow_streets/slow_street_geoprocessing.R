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
## SPATIAL OBJECT
streets_lines <- sf_streets_csv %>%
         st_as_sf(., wkt = "line")


## VISUALIZE
ggplot() + 
  geom_sf(data = streets_lines) #+ 
  geom_polygon(data = )

