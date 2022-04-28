#GTFS Transit Analysis SEPTA

#Validate GTFS
library(tidytransit)
GTFS_data <- file.path("E:/Documents/OneDrive - PennO365/2021-2022/Capstone/Greater_Philly/OTP/graphs/default")
GTFS_files <- list.files(GTFS_data)

GTFS <- read_gtfs(paste0(GTFS_data,"/",GTFS_files[1]))
validation_result <- attr(GTFS, "validation_result")

GTFS <- read_gtfs(paste0(GTFS_data,"/",GTFS_files[2]))
validation_result <- attr(GTFS, "validation_result")


### Path to the OTP files
### Note the name OTP MUST BE IN CAPS. Despite what the website says, it must 
### be caps
path_data <- file.path("E:/Documents/OneDrive - PennO365/2021-2022/Capstone/Greater_Philly/OTP")

path_otp <- otp_dl_jar(path_data, cache = FALSE)


### downloding the OTP graph
log1 <- otp_build_graph(otp = path_otp, dir = path_data)

### Launching the graph
log2 <- otp_setup(otp = path_otp, dir = path_data)

### Connect to the OTP from R
otpcon <- otp_connect()

# Finding routes

Clusters <- ACS_Cluster %>%
              filter(`Cluster type` == "high frequency high clustering") %>%
              select(Clusters,`Cluster type`, GEOID) %>%
              unique() %>%
              filter(!GEOID %in% Remove$GEOID) %>%
              st_centroid()

Clusters <- cbind(Clusters %>%
              st_drop_geometry(), 
            Clusters %>%
              st_coordinates() %>%
              as.tibble()) %>%
            relocate(Y, .before = X)
            
ggplot() +
  geom_sf(data = tigris::tracts(state = "PA",
                                county = "Philadelphia")) +
  geom_sf(data = Coords)

fromPlace <- as.numeric(Clusters[1,c(5,4)])
toPlace <- as.numeric(Clusters[4,c(5,4)])

route <- otp_plan(otpcon = otpcon,
                  fromPlace = c(-75.17283, 39.93975),
                  toPlace = c(-75.17309, 39.94679),
                  mode = c("CAR"))

