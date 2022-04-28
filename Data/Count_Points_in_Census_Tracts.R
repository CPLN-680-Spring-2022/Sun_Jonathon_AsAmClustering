# Checking schools and census tracts

ggplot() +
  geom_sf(data = Philadelphia_tracts) +
  geom_sf(data = Philly_Schools_Valid) +
  mapTheme()


#Finding where Philadelphia Universities intersect with Census Tracts -----
University_Intersection <- st_intersection(Philadelphia_tracts[1,4], 
                                       Universities %>%
                                         select(NAME,Carnegie_Classifcation2021,Short_Carnegie))

for(i in 2:length(unique(Philadelphia_tracts$GEOID))) {
  
  bind <- st_intersection(Philadelphia_tracts[i,4], 
                          Universities %>%
                            select(NAME,Carnegie_Classifcation2021,Short_Carnegie))
  
  University_Intersection <- rbind(University_Intersection,bind)
  
}

University_Count <- University_Intersection %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(Total = n())


#Finding where Philadelphia K-12 schools intersect with Census Tracts ------
## In the future need to make a count of Asbestos schools. Rather than count

School_Intersection <- st_intersection(Philadelphia_tracts[1,4], 
                                       Philly_Schools_Valid %>%
                                          select(School) %>%
                                          unique())

for(i in 2:length(unique(Philadelphia_tracts$GEOID))) {
  
  bind <- st_intersection(Philadelphia_tracts[i,4], 
                          Philly_Schools_Valid %>%
                            select(School) %>%
                            unique())
  
  School_Intersection <- rbind(School_Intersection,bind)
  
}

School_Count <- School_Intersection %>%
                  st_drop_geometry() %>%
                  group_by(GEOID) %>%
                  summarize(Total = n())
