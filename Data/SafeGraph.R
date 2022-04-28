
#Cleaning Data

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# SafeGraph Point data -------------------
Philly_Education <- read.csv("Data\\geometry.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform(crs = "EPSG:4326")%>%
  select(!c(1,2,4,6,7,9,10,11,13,14)) %>%
  mutate(location_name = toupper(location_name),
         Higher_Education = location_name %in% Universities$NAME | 
                                     street_address %in% Universities$STREET |
                                     grepl(toupper("University of Penn|University|DREXEL|COLlege|university|UNIV OF PENN"), location_name),
         Free_Library = grepl(toupper("Free Library"), location_name),
         Kumon = grepl(toupper("Kumon"), location_name),
         High_School = grepl(toupper("Hs"), location_name),
         Middle_School = grepl(toupper("\\<Ms\\>"), location_name),
         Elementary_School = grepl(toupper("\\<Sch\\>"), location_name),
         Charter_School = grepl(toupper("\\<Cs\\>|Charter School"), location_name),
         Martial_Arts_CoC = grepl(toupper("Jiu Jitsu|JiuJitsu|AIKIDO|FIght|kendo| 
                                      TAE KWON DO|Taekwondo|RAV MAGA ELITE|
                                      MUAY THAI|CERBERUS COMBAT SYSTEM|FIGHT|
                                      MARTIAL ARTS|KENJUTSU|KUNG FU|Kungfu|
                                      KARATE|MMA|CAPOEIRA|sword|SEMPER FI|
                                      URBAN DEFENSE|RENZO GRACIE PHILLY|MARTIAL|Boxing| 
                                      JIU JITSU|SINMOO|MARTIAL ARTS|AIKIDO|
                                      TAEKWON|Tae Kwon"),
                              location_name),
         Performing_Arts_CoC = grepl(toupper("Dance|Tango|Theatre|Theater|
                                         Performing|SOUND SPACE|Music|vocal|
                                         GAMP|ballroom"), location_name),
         Sports_CoC = grepl(toupper("Basketball|First tee|JMGOLFFIT|CRICKET"), location_name),
         Culinary_CoC = grepl(toupper("CULINARY"), location_name),
         Driving_Sup = grepl(toupper("driving|truck|driver"), location_name),
         Cultural_Edu_CoC = grepl(toupper("SCUOLA MARCO POLO|italian|French| 
                                      DISABILITY PRIDE|KOL TZEDEK|LINGUAL INST|
                                      German|Spanish|Korean|CHinese"), location_name),
         CPR = grepl(toupper("cpr"), location_name),
         Tutoring_Sup = grepl(toupper("kaplan|tutor|sylvan|princeton review|College success"), location_name),
         Religious = grepl(toupper("Jesus|Christ|Christian|Catholic|Hillel|Gospel|Saint|Archbishop|
                                   islamic|Lutheran|jewish"), location_name))

#Education ---------------------------

Philly_Education_Long <- pivot_longer(Philly_Education, 7:length(colnames(Philly_Education)), names_to = "Type_of_Ed", values_to = "Type_of_Ed_Yes") %>%
  filter(Type_of_Ed_Yes == TRUE) %>%
  select(!Type_of_Ed_Yes)

Philly_Education <- Philly_Education %>%
                        mutate(Valid = Philly_Education$location_name %in% Philly_Education_Long$location_name)

Philly_Education_Long_FALSE <- pivot_longer(Philly_Education %>%
                                              filter(Valid == FALSE), 7:length(colnames(Philly_Education)), names_to = "Type_of_Ed", values_to = "Type_of_Ed_Yes") %>%
                               mutate(Type_of_Ed = "Not_Categorized") %>%
                               select(!Type_of_Ed_Yes) %>%
                               unique()

Philly_Education_Long <- rbind(Philly_Education_Long,Philly_Education_Long_FALSE)


