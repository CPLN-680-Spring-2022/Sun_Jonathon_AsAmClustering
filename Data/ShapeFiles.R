# Universities
Remove <- c("OBJECTID","CBSA","NMCBSA","CBSATYPE","CSA","SLDU","SCHOOLYEAR","NECTA","NMNECTA","CD","SLDL","NMCSA","LOCALE")

Universities <- st_read("https://opendata.arcgis.com/datasets/a15e8731a17a46aabc452ea607f172c0_0.geojson") %>%
  dplyr::select(!Remove) %>%
  filter(STATE == "PA") %>%
  filter(NMCNTY == "Philadelphia County") %>%
  st_as_sf()

ALL_Universities <- st_read("https://opendata.arcgis.com/datasets/a15e8731a17a46aabc452ea607f172c0_0.geojson") %>%
  dplyr::select(!Remove) %>%
  st_as_sf()

# TIGRIS Philadelphia Census Tracts -------------------------------
Philadelphia_tracts <- tracts(state = "PA", county = "Philadelphia County") %>%
  st_transform(st_crs(Universities))

#ggplot() +
#  geom_sf(data = Philadelphia_tracts)

# Census Tract centroids as points -------------------------------------
Philadelphia_Centroids <- Philadelphia_tracts %>%
  st_centroid() %>%
  st_transform(st_crs(Universities))

#ggplot() +
#  geom_sf(data = Philadelphia_Centroids)

#ggplot() +
#  geom_sf(data = Philadelphia_School_District)

Philadelphia_HOLC <- st_read("https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/PAPhiladelphia1937.geojson") %>%
                        st_intersection(Philadelphia_School_District)


