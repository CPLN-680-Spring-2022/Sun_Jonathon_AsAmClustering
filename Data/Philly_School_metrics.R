# Datasets
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate)

options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


FE <- list.files("Data/Philadelphia_school_metrics")
files <- paste0("Data/Philadelphia_school_metrics/",FE)

df <- read_excel(files[3]) %>%
          select(1:3,starts_with("overall")) %>%
          mutate(Year = 2014)

import_school_metric <- function(x,year){
                              df <- read_excel(files[x], sheet = 4) %>%
                                select(1:4,`Street Address`,starts_with("overall"),-`Rpt Type Long`)
                            for(i in 4:6){
                              bind <- read_excel(files[x], sheet = i) %>%
                                select(1:4,`Street Address`, starts_with("overall"),-`Rpt Type Long`)
                              df <- rbind(df,bind)
                            }
                              df <- df %>%
                                      mutate(year = year)
                              }
# I have to manually check to see what kind of codes were made because
# I have no idea what is stored in these sheets
#2015 and 2016 ----------------
df <- import_school_metric(4,2015)
colnames(df)
bind <- import_school_metric(5,2016)

df <- rbind(df,bind)

#2017 -----------------
bind <- import_school_metric(6, 2017) %>%
          select(-`ULCS Code`,-`Overall Pts Earn`,-`Overall Pts Poss`)

df <- rbind(df,bind)

#2018 ---------------
bind <- import_school_metric(7, 2018) %>%
  select(-`ULCS Code`,-`Overall Pts Earn`,-`Overall Pts Poss`)

df <- rbind(df,bind) %>% 
        mutate(School = toupper(School))

#Merging Safegraph lat_lon to K-12 Data
Philly_Schools <- st_read("Data/Philadelphia_schools_shp")

Philly_Schools <- left_join(df, Philly_Schools %>%
                  rename(School = SCHOOL_N_1) %>%
                  select(School),
          by = "School")  

Philly_Schools_Valid <- st_intersection(Philly_Schools%>%
                                          st_as_sf(), Philadelphia_School_District %>%
                                                        select())

Philly_Schools_NotValid <- Philly_Schools %>%
          filter(!School %in% Philly_Schools_Valid$School) %>%
          unique()

#GeoCode Philly_Schools_NotValid ------------------------------------
#Locations <- unique(Philly_Schools_NotValid$School)[-c(1,10,25)]

#locations_LatLon <- geocode(location = Locations)
#locations_Shp <- locations_LatLon %>% 
#  rename(Longitude = lon,
#         Latitude = lat) %>%
#  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
#  st_transform(st_crs(Universities)) %>%
#  cbind(Locations)
#st_write(locations_Shp, 
#         "Data/Philadelphia_Schools_Charter_Geocoded/Charter_geocoded.shp")


# Merge geocoded data with Notvalid ------------------------------

Philly_Schools_NotValid <- Philly_Schools_NotValid %>%
          select(-geometry) %>%
        left_join(st_read("Data/Philadelphia_Schools_Charter_Geocoded/Charter_geocoded.shp") %>%
                    rename(School = Locations),
                  by = "School") %>%
          st_as_sf()

colnames(Philly_Schools_NotValid) <- colnames(Philly_Schools_Valid)

Philly_Schools <- rbind(Philly_Schools_Valid %>%
                          st_transform(st_crs(Universities)), 
                        Philly_Schools_NotValid %>%
                          st_transform(st_crs(Universities)))  

# Import Asbestos ------------------------
FE <- list.files("Data/Philadelphia_Asbestos")
files <- paste0("Data/Philadelphia_Asbestos/",FE)


Asbestos <- read.csv(files[1]) %>%
              mutate(Application.Date = ymd(Application.Date),
                     Planned.Completion.Date = ymd(Planned.Completion.Date),
                     Year_Complete = year(Planned.Completion.Date))
Asbestos_sum <- Asbestos %>%
                  group_by(Year_Complete,School.Name) %>%
                  summarize(Asbestos_Repair_Complete = n()) %>%
                  rename(year = Year_Complete,
                         School = School.Name) %>%
                  mutate(School = toupper(School))
Philly_Schools <- left_join(Philly_Schools, Asbestos_sum, by = c("year","School")) %>%
                    mutate(Asbestos_Repair_Complete = replace_na(Asbestos_Repair_Complete,0))


#Import Race ---------
Directory <- "Data/Enrollment_Demographic_Philly_Schools/Original"
FE <- list.files(Directory)
files <- paste0(Directory,"/",FE)

# Reading in the first file to use as a test ----------------------
df_Edit1 <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_Edit1) <- c("SRCSchoolCode","School_Name","Grade","Total_enrolment","Count","Percent")

colseq <- as.tibble(5:18) %>%
            mutate(even_odd = value%%2 == 0) %>%
            filter(even_odd == FALSE)

race_headings <- c("American_Indian/Alaskan_Natve(Not_Hisp)",
                   "Asian(Not_Hisp)",
                   "Black_African_American(Not_Hisp",
                   "Hispanic",
                   "Multi-racial",
                   "Native_Hawaiian_Pacific_Islander",
                   "White")

df_Race <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_Race) <- c("SRCSchoolCode","School_Name","Grade","Total_enrolment","Count","Percent")

df_full <- data.frame(matrix(ncol = 7, nrow = 0 ))
colnames(df_full) <- c("SRCSchoolCode","School_Name","Grade","Total_enrolment","Count","Percent","Race")


years <- c(2010:2022)

for(f in 1:7){
  df <- read_excel(files[f], sheet = 5) %>%
    slice(4:nrow(read_excel(files[f], sheet = 5)))
  
  
for(i in 1:length(race_headings)){
  count = colseq$value[i]
  percent = colseq$value[i]+1  
  
  df_Race_Merge <- df %>% 
    select(1:4,count:percent) %>%
    slice(-(1:2)) %>%
    mutate(Race = race_headings[i]) %>%
    rename(SRCSchoolCode = 1,
           School_Name = 2,
           Grade = 3,
           Total_enrollment = 4,
           Count = 5,
           Percent = 6)
  
  df_Race <- rbind(df_Race,df_Race_Merge)
}
  
  df_full <- rbind(df_Race%>%
                     mutate(year = years[f]),
                   df_full)
  
}

# Reading in 2016 beyond ------------------------------------

colseq <- as.tibble(6:18) %>%
  mutate(even_odd = value%%2 == 0) %>%
  filter(even_odd == TRUE)

f = 8
  df <- read_excel(files[f], sheet = 6) %>%
    slice(4:nrow(read_excel(files[f], sheet = 5)))
  
  for(i in 1:length(race_headings)){
    count = colseq$value[i]
    percent = colseq$value[i]+1  
    
    df_Race_Merge <- df %>% 
      select(1:4,count:percent) %>%
      slice(-(1:2)) %>%
      mutate(Race = race_headings[i]) %>%
      rename(SRCSchoolCode = 1,
             School_Name = 2,
             Grade = 3,
             Total_enrollment = 4,
             Count = 5,
             Percent = 6)
    
    df_Race <- rbind(df_Race,df_Race_Merge)
  }
  
  df_full <- rbind(df_Race%>%
                     mutate(year = 2017),
                   df_full)
  
# f9 --------------------------------

f = 9
  df <- read_excel(files[f], sheet = 6) %>%
    slice(4:nrow(read_excel(files[f], sheet = 5)))
  
for(i in 1:length(race_headings)){
    count = colseq$value[i]
    percent = colseq$value[i]+1  
    
    df_Race_Merge <- df %>% 
      select(1,2,4,5,count:percent) %>%
      slice(-(1:2)) %>%
      mutate(Race = race_headings[i]) %>%
      rename(SRCSchoolCode = 1,
             School_Name = 2,
             Grade = 3,
             Total_enrollment = 4,
             Count = 5,
             Percent = 6)
    
    df_Race <- rbind(df_Race,df_Race_Merge)
  }
  
  df_full <- rbind(df_Race%>%
                     mutate(year = 2018),
                   df_full)  
  
# f10 ----------------------------------------------
  
f = 10  
    
colseq <- as.tibble(19:32) %>%
    mutate(even_odd = value%%2 == 0) %>%
    filter(even_odd == FALSE)  

  df <- read_excel(files[f]) %>%
    slice(4:nrow(read_excel(files[f])))
  
  for(i in 1:length(race_headings)){
    count = colseq$value[i]
    percent = colseq$value[i]+1  
    
    df_Race_Merge <- df %>% 
      select(2,3,5,6,count:percent) %>%
      slice(-(1:2)) %>%
      mutate(Race = race_headings[i]) %>%
      rename(SRCSchoolCode = 1,
             School_Name = 2,
             Grade = 3,
             Total_enrollment = 4,
             Count = 5,
             Percent = 6)
    
    df_Race <- rbind(df_Race,df_Race_Merge)
  }
  
  df_full <- rbind(df_Race%>%
                     mutate(year = 2018),
                   df_full) 
# f11 2019 beyond -------------------

f = 11
df <- read.csv(files[f])
  
  colseq <- as.tibble(21:length(colnames(df))-1) %>%
    mutate(even_odd = value%%2 == 0) %>%
    filter(even_odd == FALSE)  
  
  for(i in 1:length(race_headings)){
    count = colseq$value[i]
    percent = colseq$value[i]+1  
    
    df_Race_Merge <- df %>% 
      select(4,5,7,8,count:percent) %>%
      slice(-(1:2)) %>%
      mutate(Race = race_headings[i]) %>%
      rename(SRCSchoolCode = 1,
             School_Name = 2,
             Grade = 3,
             Total_enrollment = 4,
             Count = 5,
             Percent = 6)
    
    df_Race <- rbind(df_Race,df_Race_Merge)
  }
  
  df_full <- rbind(df_Race%>%
                     mutate(year = 2018),
                   df_full) 

# f13 2019 beyond -------------------
  
for(f in 12:length(files)){
  df <- read.csv(files[f])
  
  colseq <- as.tibble(21:length(colnames(df))-1) %>%
    mutate(even_odd = value%%2 == 0) %>%
    filter(even_odd == FALSE)  
  
  for(i in 1:length(race_headings)){
    count = colseq$value[i]
    percent = colseq$value[i]+1  
    
    df_Race_Merge <- df %>% 
      select(4,5,7,8,count:percent) %>%
      slice(-(1:2)) %>%
      mutate(Race = race_headings[i]) %>%
      rename(SRCSchoolCode = 1,
             School_Name = 2,
             Grade = 3,
             Total_enrollment = 4,
             Count = 5,
             Percent = 6)
    
    df_Race <- rbind(df_Race,df_Race_Merge)
  }
  
  df_full <- rbind(df_Race%>%
                     mutate(year = years[f]),
                   df_full) 
}
  
Philly_School_All <-  left_join(Philly_Schools %>%
              rename(SRCSchoolCode = SRC.School.ID,
                     School_Name = School),
           df_full %>%
              select(!School_Name),
           by = c("SRCSchoolCode","year")) %>%
                    mutate(Count = as.numeric(str_replace(Count,"s","-99")))
