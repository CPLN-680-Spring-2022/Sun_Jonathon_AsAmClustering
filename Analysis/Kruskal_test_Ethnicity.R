# ANOVA Test


# T-tests

# ANOVA on Handmade groups

T_Test <- ACS_Cluster %>%
  st_drop_geometry() %>%
  filter(Category == "Medians") %>%
  na.omit() %>%
  select(-c("Cluster type","Clusters")) %>%
  rename(Type_of_Cluster = `Cluster type Ethnicity`,
         Clusters = Clusters_Ethnicity) %>%
  mutate(Clusters = str_replace_all(Clusters,"Native_Hawaiian_and_Other_Pacific_Islander_alone_Cluster","Hawaiian_PI_Cluster")) %>%
  unique()


### Adding column to Kruskal to specify which group -----------------------
Kruskal <- Kruskal %>%
  mutate(Cluster = "PanEthnic")

### Median Income ----------------------------------

name <- unique(T_Test$variable)[1]

df <- T_Test %>%
  filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[2]) %>%
  filter(variable == name) %>%
  na.omit()

Remove <- df %>%
  group_by(GEOID) %>%
  summarize(Frequency = n()) %>%
  filter(Frequency > 1)

Kruskal <- df %>% 
  filter(!GEOID %in% Remove$GEOID) %>%
  kruskal_test(estimate ~ Clusters) %>%
  mutate(Variable = name,
         Cluster = "Ethnic")

#### Looping through other variables ----------------------

for(i in 1:length(unique(T_Test$variable))){
  
  name <- unique(T_Test$variable)[i]
  
  df <- T_Test %>%
    ungroup() %>%
    filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[2]) %>%
    filter(variable == name) %>%
    rename(Values = estimate) %>%
    na.omit()
  
  Remove <- df %>%
    group_by(GEOID) %>%
    summarize(Frequency = n()) %>%
    filter(Frequency > 1)
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    kruskal_test(Values ~ Clusters) %>%
    mutate(Variable = name,
           Cluster = "Ethnic")
  
  Kruskal <- rbind(Kruskal, merge)
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats() %>%
    mutate(variable = name)
  
  Summary <- rbind(Summary, merge)
  
  df %>%
    ggplot( aes(x = Clusters, y = Values, fill = Clusters)) +
    geom_boxplot() +
    geom_jitter(color = "black", 
                size = 0.4,
                alpha = 0.9) +
    labs(title = paste0(name),
         subtitle = "By Broad Clustering Group") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45,
                                     hjust = 1)) +
    coord_flip() +
    plotTheme()
  
  ggsave(paste0("Analysis/Figs/Kruskal_Box/",name,"Ethnic.jpg"), width = 2000, units = "px")
}


#### Census tract demographics --------------------------------

T_Test <- ACS_Cluster %>%
  st_drop_geometry() %>%
  filter(!Category == "Medians") %>%
  na.omit() %>%
  select(-c("Cluster type","Clusters")) %>%
  rename(Type_of_Cluster = `Cluster type Ethnicity`,
         Clusters = Clusters_Ethnicity) %>%
  mutate(Clusters = str_replace_all(Clusters,"Native_Hawaiian_and_Other_Pacific_Islander_alone_Cluster","Hawaiian_PI_Cluster")) %>%
  unique()


#### Looping through other variables ----------------------

for(i in 1:length(unique(T_Test$variable))){
  
  name <- unique(T_Test$variable)[i]

  df <- T_Test %>%
    ungroup() %>%
    filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[2]) %>%
    filter(variable == name) %>%
    rename(Values = estimate) %>%
    na.omit()
  
  Remove <- df %>%
    group_by(GEOID) %>%
    summarize(Frequency = n()) %>%
    filter(Frequency > 1)
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    kruskal_test(Values ~ Clusters) %>%
    mutate(Variable = name,
           Cluster = "Ethnic")
  
  Kruskal <- rbind(Kruskal, merge)
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats() %>%
    mutate(variable = name)
  
  Summary <- rbind(Summary, merge)
  
  df %>%
    ggplot( aes(x = Clusters, y = Values, fill = Clusters)) +
    geom_boxplot() +
    geom_jitter(color = "black", 
                size = 0.4,
                alpha = 0.9) +
    labs(title = paste0(name),
         subtitle = "By Broad Clustering Group") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45,
                                     hjust = 1)) +
    coord_flip() +
    plotTheme()
  
  ggsave(paste0("Analysis/Figs/Kruskal_Box/",name,"Ethnic.jpg"), width = 2000, units = "px")
  
}

write.csv(Kruskal %>%
            arrange(p),"Analysis/Kruskal_4-12-2022.csv")

