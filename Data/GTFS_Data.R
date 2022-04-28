#GTFS script

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


# Test to make sure everything works --------------------------
GTFS_path <- c("Data\\OTP\\graphs\\default\\google_bus.zip")

GTFS <- read_gtfs(GTFS_path)
GTFS_Bus <- GTFS

validation_result <- attr(GTFS, "validation_result")
#head(validation_result)

GTFS_path <- c("Data\\OTP\\graphs\\default\\google_rail.zip")

GTFS <- read_gtfs(GTFS_path)
GTFS_rail <- GTFS

validation_result <- attr(GTFS, "validation_result")
#head(validation_result)

#GTFS geometry -------------------------

## Bus routes http://www.septa.org/schedules/bus/index.html
Buses <- c(1:89,90:311,"G","H","J","K","L","R","XH","47M","BLVDDIR")
Trolley <- as.character(c(10,11,13,15,34,36,"36B"))
LUCY <- c("LUCYGO","LUCYGR")
Subway <- c("MFL","MFO","BSO","BSL")
HSLine <- c("NHSL")


## Bus
GTFS_Bus_shp <- gtfs_as_sf(GTFS_Bus)
GTFS_Bus_route <- get_route_geometry(GTFS_Bus_shp) %>%
                    st_transform(st_crs(Universities))%>%
                    mutate(Transit_form = case_when(
                      route_id %in% Trolley ~ "Trolley",
                      route_id %in% Buses ~ "Buses",
                      route_id %in% LUCY ~ "LUCY",
                      route_id %in% Subway ~ "Subway",
                      route_id %in% HSLine ~ "HSLine"
                    ))


GTFS_Bus_points <- st_as_sf(GTFS_Bus$stops,
                            coords = c("stop_lon","stop_lat"),
                            crs=4326) %>%
                   st_transform(st_crs(Universities))

GTFS_Bus_points <- st_intersection(GTFS_Bus_points, Philadelphia_School_District)
GTFS_Bus_route <- st_intersection(GTFS_Bus_route, Philadelphia_School_District)

even <- seq(0,100,2)

GTFS_Subway_stops_route <- GTFS_Bus_points %>%
                              filter(grepl("MFL|MFO|BSO|BSL",stop_name)) %>%
                              select(stop_name) %>%
                              mutate(stop_name = substr(stop_name,1,nchar(stop_name)-5)) %>%
                              slice(even)

ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = GTFS_Subway_stops_route)
#GTFS_Bus_points_Buffers <- st_union(st_buffer(GTFS_Bus_points, 2640)) %>%
#  st_sf() 

## Rail
GTFS_Rail_shp <- gtfs_as_sf(GTFS_rail)
GTFS_Rail_route <- get_route_geometry(GTFS_Rail_shp) %>%
                    st_transform(st_crs(Universities))
GTFS_Rail_points <- st_as_sf(GTFS_rail$stops,
                            coords = c("stop_lon","stop_lat"),
                            crs=4326) %>%
                    st_transform(st_crs(Universities))

GTFS_Rail_points <- st_intersection(GTFS_Rail_points, Philadelphia_School_District)
GTFS_Rail_route <- st_intersection(GTFS_Rail_route, Philadelphia_School_District)

#GTFS_Rail_points_Buffers <- st_union(st_buffer(GTFS_Rail_points, 2640)) %>%
#  st_sf() 

ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = GTFS_Rail_points) +
  geom_sf(data = GTFS_Rail_route) +
#  geom_sf(data = GTFS_Rail_points_Buffers,
#          fill = "transparent") +
  mapTheme()

ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = GTFS_Bus_points) +
  geom_sf(data = GTFS_Bus_route) +
#  geom_sf(data = GTFS_Bus_points_Buffers) +
  mapTheme()
