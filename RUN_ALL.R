if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, survey, srvyr, caret, sp,
               ISLR,MASS,spatstat,spatial,maptools,ppp,fossil,adehabitHR,gdata,raster,rgdal,geostatst, spdep, caret, tidyverse, sf, spdep, caret, ckanr,
               grid, gridExtra, knitr, kableExtra, scales, rgdal, rgeos, spdep, spgwr, tmap, rstatix, data.table, tidyr)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

#ALL DATA
source("Data/ShapeFiles.R")
source("Data/GreaterPhilly_ACS.R")
source("Data/Philly_School_metrics.R")
source("Data/Carnegie_Classification.R")

#Data Engineering


#Model 1: Philadelphia
source("Analysis/Moran_I.R")
source("Analysis/Kruskal_test.R")

#Transit: Philadelphia
