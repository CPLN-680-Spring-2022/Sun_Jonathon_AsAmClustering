# Kruskal-Wallis & Pairwise Wilcox
## The Kruskal-Wallis test, is the non-parametric counterpart to the one-way 
## independent ANOVA test. All of these variables are non-parametric (not on a
## standard curve) THe Kruskal-wallis test first determines if the groups
## have differences that are statistically significant. After that I use the 
## Pairwise Wilcox test to determine which variables are different. The Pairwise
## Wilcox test is a nonparametric tests which tests the significance between each
## of the pairs.

# T-tests
## it says T_Test but it's really not the T_Test there's a lot that I will have
## to change if I change the word T_Test. It's really just prepping the data

T_Test <- ACS_Cluster %>%
            st_drop_geometry() %>%
            filter(Category == "Medians") %>%
            na.omit() %>%
            rename(Type_of_Cluster = `Cluster type`)

### Median Income ----------------------------------

name <- unique(T_Test$variable)[1]

df <- T_Test %>%
  ungroup() %>%
  dplyr::select(-c("NAME")) %>%
  filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[2]) %>%
  filter(variable == unique(T_Test$variable)[1]) %>%
  rename(Values = estimate) %>%
  na.omit()

Remove <- df %>%
  group_by(GEOID) %>%
  summarize(Frequency = n()) %>%
  filter(Frequency > 1)

df %>% 
  filter(!GEOID %in% Remove$GEOID) %>%
  ggplot( aes(x = Values, fill = Clusters)) +
  geom_histogram(bins = 50) +
  facet_wrap(~Clusters) +
  labs(title = paste0("Distribution: ",name),
       subtitle = "By Broad Clustering Group") +
  plotTheme()

ggsave(paste0("Analysis/Figs/Histograms/",name,".jpg"), width = 4000, units = "px")

Kruskal <- df %>% 
  filter(!GEOID %in% Remove$GEOID) %>%
  kruskal_test(Values ~ Clusters) %>%
  mutate(Variable = unique(T_Test$variable)[1])

Pairwise_Wilcox <- df %>% 
  filter(!GEOID %in% Remove$GEOID) %>%
  pairwise_wilcox_test(Values ~ Clusters,
                       p.adjust.method = "BH") %>%
  mutate(variable = name)

Summary <- df %>% 
            filter(!GEOID %in% Remove$GEOID) %>%
            group_by(Clusters) %>%
            get_summary_stats() %>%
            mutate(variable = name)

df %>%
  ggplot( aes(x = Clusters, y = Values, fill = Clusters)) +
  geom_boxplot() +
  geom_jitter(color = "black", 
              size = 0.4,
              alpha = 0.9) +
  labs(title = paste0(name),
       subtitle = "By Broad Clustering Group") +
  plotTheme()
  


#### Looping through other MEDIAN variables ----------------------

for(i in 2:length(unique(T_Test$variable))){

  if(i %in% c(3,6)) next
  
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
  
  df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    ggplot( aes(x = Values, fill = Clusters)) +
    geom_histogram(bins = 50) +
    facet_wrap(~Clusters) +
    labs(title = paste0("Distribution: ",name),
         subtitle = "By Broad Clustering Group") +
    plotTheme()
  
  ggsave(paste0("Analysis/Figs/Histograms/",name,".jpg"), width = 4000, units = "px")
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    kruskal_test(Values ~ Clusters) %>%
    mutate(Variable = name)
  
  Kruskal <- rbind(Kruskal, merge)
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats() %>%
    mutate(variable = name)
  
  Summary <- rbind(Summary, merge)
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    pairwise_wilcox_test(Values ~ Clusters,
                         p.adjust.method = "BH") %>%
    mutate(variable = name)
  
  Pairwise_Wilcox <- rbind(Pairwise_Wilcox, merge)
  
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
    plotTheme()
  
  ggsave(paste0("Analysis/Figs/Kruskal_Box/PanEthnic/",name,".jpg"), width = 2000, units = "px")
}

# Clean up Kruskal dataframe removing .y., and method
Kruskal <- Kruskal %>%
            select(statistic,df,p,Variable)
#### Looping through none MEDIAN variables ----------------------

T_Test <- ACS_Cluster %>%
  st_drop_geometry() %>%
  filter(!Category == "Medians") %>%
  filter(!Clusters == "AAPI_Race_Cluster") %>%
  na.omit() %>%
  rename(Type_of_Cluster = `Cluster type`,
         Values = estimate) %>%
  unique()

# Testing non median variables ---------------------------------

for(i in 1:length(unique(T_Test$Category))){
  
  name <- unique(T_Test$Category)[i]
  
  df <- T_Test %>%
    filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[2]) %>%
    filter(Category == name) %>%
    group_by(GEOID,Category,Clusters) %>%
    summarize(Values = sum(Values)) %>%
    unique()
  
  Remove <- df %>%
    group_by(GEOID) %>%
    summarize(Frequency = n()) %>%
    filter(Frequency > 1)
  
  df <- df %>%
    filter(!GEOID %in% Remove$GEOID) %>%
    mutate(Clusters = as.factor(Clusters))
  
  levels <- levels(df$Clusters)
  
  kruskal <- kruskal.test(Values ~ Clusters, data = df)
  
  merge <- t(c(kruskal$statistic,length(levels), kruskal$p.value, name))
  colnames(merge) <- colnames(Kruskal)
  
  Kruskal <- rbind(Kruskal,merge)
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats() %>%
    mutate(variable = name)
  
  JOIN <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats() %>%
    select(Clusters,n)
  
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
    plotTheme()
  
  ggsave(paste0("Analysis/Figs/Kruskal_Box/PanEthnic/",name,".jpg"), width = 2000, units = "px")
  
}


# clean up Pairwise_Wilcox 

Pairwise_Wilcox <- Pairwise_Wilcox %>%
  select(group1, group2, n1, n2, p.adj,p.adj.signif,variable)

#Pairwise Wilcox

merge <- pairwise.wilcox.test(df$Values,df$Clusters,
                              p.adjust.method = "BH")

merge <- merge[["p.value"]] %>%
          as.tibble(rownames = "rownames") %>%
          rename(group1 = rownames) %>%
          pivot_longer(2:4, names_to = "group2", values_to = "p.adj") %>%
          na.omit() %>%
          mutate(p.adj.signif = case_when(
                                  p.adj <= 0.001 ~ "**",
                                  p.adj <= 0.05 & p.adj > .001 ~ "*",
                                  p.adj > 0.05 ~ "ns")) %>%
         left_join(JOIN, 
                   by = c("group1" = "Clusters")) %>%
          rename(n1 = n) %>%
         left_join(JOIN, 
                  by = c("group2" = "Clusters")) %>%
          rename(n2 = n) %>%
          mutate(variable = name)

Pairwise_Wilcox <- rbind(Pairwise_Wilcox, merge)


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
  plotTheme()


### for loop for non median variables --------------------------------------------

for(i in 1:length(unique(T_Test$Category))){
  
  name <- unique(T_Test$Category)[i]
  
  df <- T_Test %>%
    filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[2]) %>%
    filter(Category == name) %>%
    group_by(GEOID,Category,Clusters) %>%
    summarize(Values = sum(Values)) %>%
    unique()
  
  Remove <- df %>%
    group_by(GEOID) %>%
    summarize(Frequency = n()) %>%
    filter(Frequency > 1)
  
  df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    ggplot( aes(x = Values, fill = Clusters)) +
    geom_histogram(bins = 50) +
    facet_wrap(~Clusters) +
    labs(title = paste0("Distribution: ",name),
         subtitle = "By Broad Clustering Group") +
    plotTheme()
  
  ggsave(paste0("Analysis/Figs/Histograms/",name,".jpg"), width = 4000, units = "px")
  
  df <- df %>%
    filter(!GEOID %in% Remove$GEOID) %>%
    mutate(Clusters = as.factor(Clusters))
  
  levels <- levels(df$Clusters)
  
  kruskal <- kruskal.test(Values ~ Clusters, data = df)
  
  merge <- t(c(kruskal$statistic,length(levels), kruskal$p.value, name))
  colnames(merge) <- colnames(Kruskal)
  
  Kruskal <- rbind(Kruskal,merge)
  
  merge <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats() %>%
    mutate(variable = name)
  
  JOIN <- df %>% 
    filter(!GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats() %>%
    select(Clusters,n)
  
  Summary <- rbind(Summary, merge)
  
  #Pairwise Wilcox
  
  merge <- pairwise.wilcox.test(df$Values,df$Clusters,
                                p.adjust.method = "BH")
  
  merge <- merge[["p.value"]] %>%
    as.tibble(rownames = "rownames") %>%
    rename(group1 = rownames) %>%
    pivot_longer(2:4, names_to = "group2", values_to = "p.adj") %>%
    na.omit() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "**",
      p.adj <= 0.05 & p.adj > .001 ~ "*",
      p.adj > 0.05 ~ "ns")) %>%
    left_join(JOIN, 
              by = c("group1" = "Clusters")) %>%
    rename(n1 = n) %>%
    left_join(JOIN, 
              by = c("group2" = "Clusters")) %>%
    rename(n2 = n) %>%
    mutate(variable = name)
  
  
  Pairwise_Wilcox <- rbind(Pairwise_Wilcox, merge)
  
  
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
    plotTheme()
  
  ggsave(paste0("Analysis/Figs/Kruskal_Box/",name,"PanEthnic.jpg"), width = 2000, units = "px")
}


Kruskal_Sig <- Kruskal %>%
                filter(!Variable == "okinwan_Asian") %>%
                mutate(p = as.numeric(p)) %>%
                filter(p <= 0.05) %>%
                filter(!Variable %in% c("Southeast_Asian_Asian",
                                        "East_Asian_Asian",
                                        "Median year structure built",
                                        "South_Asian_Asian")) %>%
                unique() %>%
                arrange(p)

Significant <- Pairwise_Wilcox %>%
                filter(!p.adj.signif == "ns") %>%
                filter(variable %in% Kruskal_Sig$Variable) %>%
                arrange(p.adj)

unique(Significant$variable)

options(scipen = 99999)

write.csv(Summary %>%
            filter(variable %in% unique(Significant$variable)),
          "Analysis/summary_panethnic.csv")

write.csv(Kruskal_Sig,"Analysis/Kruskal_panethnic.csv")

write.csv(Significant,"Analysis/PairwiseWilcox_panethnic.csv")
