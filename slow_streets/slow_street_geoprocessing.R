#------------------------------------------------------------
# GEOPROCESSING &  SLOW STREETS & NEIGHBORHOOD DATA
# 
# AUTHOR: FRANCINE STEPHENS
# DATE CREATED: 3/21/21
# LAST UPDATED: 3/27/21
#-------------------------------------------------------------

# SET-UP------------------------------------------------------------------------

#### LIBRARIES ####
packages <- c(
  "readr",
  "tidyverse",
  "sf",
  "RSocrata",
  "ggplot2",
  "leaflet",
  "mapboxapi",
  "scales",
  "units",
  "ggcorrplot",
  "tmap",
  "tigris",
  "censusapi", 
  "tidycensus",
  "corrplot",
  "RColorBrewer"
)
lapply(packages, library, character.only = T)


#### PARAMETERS ####
socrata_token <- "EZnaKCxOX6zj0uyTkD340TWLh"
email <- "fis@stanford.edu"
pword <- "SFdata2017"
wgs <- 4326
proj_meters <- 7131   # epsg for SF city/county in meters (NAD) 


#### PATHS ####
setwd("~/Projects/public_space/slow_streets")
wd <- getwd()
shapefiles_path <- "C:/Users/Franc/Documents/Shapefile_Repository/"
sf_shapes_path <- "san_francisco_shapes/"
sf_nhoods_path <- "analysis_nhoods_2010_census_tracts/"


#### INGEST DATA ####
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
                               password  = pword
                               )

sf_nhoods_tracts10 <- st_read(paste0(shapefiles_path, 
                                     sf_shapes_path, 
                                     sf_nhoods_path, 
                                     "geo_export_afde1190-a893-43d3-9611-1325253cf07f.shp")
                    )   


# GEOPROCESSING-----------------------------------------------------------------

#### Process street segments #####
all_ss_segments <- ss_intersections %>%
  filter(!is.na(street_segments)) %>%
  separate(intersection, c("st1", "st2"), sep = " & ") %>%
  group_by(name) %>%
  mutate(across(starts_with("st"), toupper)) %>%
  mutate(segments = strsplit(as.character(street_segments), ", ")) %>%
  unnest(segments) %>%
  distinct(st1, segments, .keep_all = TRUE) %>%
  ungroup()

streets_lines <- sf_streets_csv %>%
         st_as_sf(., wkt = "line")

ss_street_segments <- streets_lines %>%
  right_join(., all_ss_segments, by = c("streetname" = "st1","f_st" = "segments")) %>%
  filter(!is.na(cnn),
         t_st != "EGBERT AVE") %>%
  arrange(id, cnn) %>%
  st_set_crs(wgs) %>%
  st_transform(., crs = proj_meters) %>%  
  mutate(length_meters = st_length(.),
         length_miles = as.numeric(set_units(length_meters, mi))
         )

ss_streets_lines <- ss_street_segments %>%
  group_by(name) %>%
  summarize(nblocks = n()) %>% 
  st_set_crs(wgs) %>%
  st_transform(., crs = proj_meters) %>%  
  mutate(length_meters = st_length(.),
         length_miles = as.numeric(set_units(length_meters, mi))
  ) # Create multi-linestring object for mapping full slow street

ss_street_segment_points <- st_cast(ss_street_segments, "POINT") %>%
  distinct(line, .keep_all = TRUE)  # Get intersection points for isochrones

  
#### Construct Isochrones ####
  ss_10_min_walk_iso <- mb_isochrone(
    ss_street_segment_points,
    profile = "walking",
    time = 10
  )

  ss_10_min_walk_iso_info <- ss_street_segment_points %>% 
    st_set_geometry(NULL) %>% 
    cbind(ss_10_min_walk_iso$geometry) %>% 
    st_as_sf() %>%
    group_by(name) %>% 
    summarize()
  
  ## Visualize ##
  leaflet() %>% 
    addMapboxTiles(
      style_id = "streets-v11",
      username = "mapbox"
    ) %>%
    addPolygons(
      data = ss_10_min_walk_iso_info %>%
        st_transform(., crs = wgs),
      label = ~name
    ) %>% 
    addPolylines(
      data = ss_streets_lines %>% st_transform(., crs = wgs),
      color = "red",
      label = ~name, 
    ) 
  
  
#### Aggregate street segments ####
ss_length_table <- ss_street_segments %>%
  group_by(name) %>%
  summarize(across(starts_with("length"), ~sum(.x, na.rm = TRUE))) %>%
  arrange(-length_meters)

ss_length_by_nhood_ss <- ss_street_segments %>%
  mutate_if(is.character, list(~na_if(.,""))) %>%
  mutate(nhood = str_replace_na(nhood, "Outer Mission")) %>%
  group_by(nhood, name) %>% 
  summarize(across(starts_with("length"), ~sum(.x, na.rm = TRUE))) %>%
  arrange(nhood, -length_meters)

ss_length_by_nhood <- ss_length_by_nhood_ss %>%
  group_by(nhood) %>%
  summarize(across(starts_with("length"), ~sum(.x, na.rm = TRUE)),
            n_ss = n(),
            slow_streets = toString(name)) %>%
  arrange(-length_meters)


  #### Visualize ####
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
    addPolylines(
      data = ss_streets_lines %>% st_transform(., crs = wgs)
    )

  ggplot() + 
    geom_sf(data = streets_lines) #+ 
    geom_polygon(data = )


## NEIGHBORHOODS 
sf_nhoods <- sf_nhoods_tracts10 %>%
  st_set_crs(wgs) %>%
  group_by(nhood) %>%
  summarize(ntracts = n()) %>%
  st_transform(., crs = st_crs(proj_meters))


