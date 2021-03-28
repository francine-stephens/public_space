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
sf_cbg <- block_groups(state = "CA",
                       county = "San Francisco",
                       cb = TRUE,
                       progress_bar = FALSE,
                       year = 2019)



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
  ) # Multi-linestring object for mapping full slow street

ss_street_segment_points <- st_cast(ss_street_segments, "POINT") %>%
  distinct(line, .keep_all = TRUE)  # All intersection points for isochrones

  
#### Construct Isochrones ####
ss_10_min_walk_iso <- mb_isochrone(
  ss_street_segment_points,
  profile = "walking",
  time = 10)

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
      label = ~name 
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


#### Process neighborhoods ####
sf_nhoods <- sf_nhoods_tracts10 %>%
  st_set_crs(wgs) %>%
  group_by(nhood) %>%
  summarize(ntracts = n()) %>%
  st_transform(., crs = st_crs(proj_meters))

  
acs_vars <- readRDS(
  "C:/Users/Franc/Documents/Stanford/equity_analysis/acs_vars_2018_5yr.rds"
  )
    
acs19_sf_bg <- get_acs(geography = "block group", 
                          variables = c(
                          tpop = "B01003_001E",
                          white_nh = "B03002_003E",
                          black_nh = "B03002_004E",
                          asian_nh = "B03002_006E",
                          pacificisl_nh = "B03002_007E",
                          latinx = "B03002_012E",
                          households = "B17017_001E",
                          hh_bpov = "B17017_002E",
                          m_und5 = "B01001_003E",
                          m_5to9 = "B01001_004E",
                          m_10to14 = "B01001_005E",
                          m_15to17 = "B01001_006E",
                          f_und5 = "B01001_027E",
                          f_5to9 = "B01001_028E",
                          f_10to14 = "B01001_029E",
                          f_15to17 = "B01001_030E",
                          m_65to66 = "B01001_020E",
                          m_67to69 = "B01001_021E",
                          m_70to74 = "B01001_022E",
                          m_75to79 = "B01001_023E",
                          m_80to84 = "B01001_024E",
                          m_85plus = "B01001_025E",
                          f_65to66 = "B01001_044E",
                          f_67to69 = "B01001_045E",
                          f_70to74 = "B01001_046E",
                          f_75to79 = "B01001_047E",
                          f_80to84 = "B01001_048E",
                          f_85plus = "B01001_049E"
                          ), 
                       year = 2019,
                       output = "wide",
                       state = "CA",
                       county = "San Francisco"
                       ) %>%
  select(!ends_with("M")) 

acs19_sf_bg_counts <- acs19_sf_bg %>%
  mutate(aapi_nh = (asian_nh + pacificisl_nh)) %>%
  relocate(aapi_nh, .after = black_nh) %>%
  mutate(under18 = rowSums(across(m_und5:f_15to17)),
         over65 = rowSums(across(m_65to66:f_85plus))
         ) %>%
  select(-NAME, -m_und5:-f_85plus)

saveRDS(acs19_sf_bg_counts, "acs19_counts_sf_bg.rds")

#### Apportion block groups to isochrones ####
sf_cbgs_wdata <- sf_cbg %>%
  st_transform(., crs = proj_meters) %>%
  mutate(original_area = st_area(.)) %>%
  left_join(., acs19_sf_bg_counts, by = "GEOID")

sf_cbg_10_min_walk_iso_intersect <- sf_cbgs_wdata %>% 
  st_intersection(
    ss_10_min_walk_iso_info %>% 
      #st_union() %>% 
      st_transform(proj_meters)
  ) %>% 
  mutate(leftover_area = st_area(.), 
         perc_area = as.numeric(leftover_area / original_area)
  )

ss_10_min_serv_area_demogs <- sf_cbg_10_min_walk_iso_intersect %>%
  mutate_at(vars(tpop:over65), .funs = funs(. * perc_area)) %>%
  group_by(name) %>% 
  summarise(across(tpop:over65, ~ sum(.x))) %>% 
  relocate(households, .after = over65) %>%
  relocate(hh_bpov, .after = households) %>%
  mutate_at(vars(white_nh:over65), funs("percent" = (./tpop) * 100)) %>%
  mutate(hh_bpov_percent = (hh_bpov/households) * 100)

ss_lines_with_service_area_demogs <- ss_10_min_serv_area_demogs %>% 
  st_set_geometry(NULL) %>%
  right_join(., ss_streets_lines, by = "name") %>%
  st_set_geometry("line")


  #### Visualize ####
ss_demog_pal <- colorNumeric(
  palette = "Reds",
  domain = ss_lines_with_service_area_demogs$tpop
)
 
leaflet() %>% 
   addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(
    data = ss_lines_with_service_area_demogs %>% st_transform(., crs = wgs),
    color = ~ss_demog_pal(tpop),
    #color = "white",
    opacity = 1,
    weight = 5,
    label = ~paste0(name, " - ",
                    format(tpop, digits = 1, big.mark = ","), 
                    " residents"
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      opacity = 1)
  ) %>% 
  addLegend(
    position = "bottomleft",
    data = ss_lines_with_service_area_demogs, 
    pal = ss_demog_pal,
    values = ~tpop,
    title = "Residents Served by Slow Streets"
  )
