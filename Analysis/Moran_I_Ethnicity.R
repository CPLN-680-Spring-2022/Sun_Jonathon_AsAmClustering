# Moran's I - model Ethnicity
# Log transform the population to even out the skew

ACS_Cluster %>%
  filter(is.na(estimate) == TRUE) 


# Moran's I using df as object ------------------------------

df <- ACS_Cluster %>%
  na.omit() %>%
  filter(Category %in% c("AAPI_Race","South_Asian_Asian","Southeast_Asian_Asian","East_Asian_Asian","Filipino_Asian")) %>%
  filter(grepl("Philadelphia",NAME)) 

for(i in 1:length(unique(df$variable))) {
  loop <- df %>%
    filter(variable == unique(df$variable)[i])
  
  Variable <- loop$estimate
  
  #queen neighbors
  queen <- poly2nb(loop, row.names = loop$GEOID)
  queen
  
  #global Moran's I
  queenlist <- nb2listw(queen, style = 'W') #Spatial weights for neighors. 
  
  #running Moran's I
  MoranI <- moran(Variable, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$'I'
  
  
  #Checking significance
  moranMC <- moran.mc(Variable, queenlist, nsim=999)
  moranMC
  moranMCres <- moranMC$res
  moran.plot(Variable, queenlist)
  
  hist(moranMCres, freq = 10000000, nclass = 100)
  abline(v = moran(loop$estimate, queenlist, n = length(queenlist$neighbours), S0=Szero(queenlist))$'I', col='red')
  
  bind <- t(c(MoranI, moranMC$p.value,unique(loop$variable)[1]))
  colnames(bind) <- x
  
  MoransITable <- rbind(MoransITable,bind)
}

df <- ACS_Cluster %>%
  filter(Category %in% c("AAPI_Race","South_Asian_Asian","Southeast_Asian_Asian","East_Asian_Asian","Filipino_Asian")) %>%
  select(-c("Category","Cluster type","Clusters")) %>%  
  unique() %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  st_as_sf()


#Moran's I statistical Significance -------------

for(i in 4:(length(colnames(df)))) {
  
  queen <- poly2nb(df, row.names = df$GEOID)
  queenlist <- nb2listw(queen, style = 'W')
  
  Variable <- as.vector(df[,i, drop = TRUE]) 
  
  moran.map <- cbind(df, localmoran(Variable, queenlist)) %>%
    mutate(Significant = `Pr.z....E.Ii..` < 0.05)
  ggplot() +
    geom_sf(data = moran.map, aes(fill = Significant)) +
    labs(title = paste0("P-Value Clustering:",colnames(df)[i])) +
    mapTheme()
  
  ggsave(filename = paste0("Analysis/Figs/Pvalue_Ethnicity/P-value_Clustering_Category_",colnames(df)[i],".png"), plot = last_plot())
}

# Local Moran's I Loop to print of maps ---------------------

colnames <- df %>%
  st_drop_geometry() %>%
  colnames()

for(i in 4:length(colnames(df))) {
  Variable <- as.vector(df[,i, drop = TRUE]) 
  
  local <- localmoran(Variable, listw = queenlist, zero.policy = FALSE)
  quadrant <- vector(mode = 'numeric', length=323)
  m.prop <- Variable - mean(Variable)
  m.local <- local[,1] - mean(local[,1])
  signif <- 0.05
  quadrant[m.prop >0 & m.local>0]<-4 #high Variable, high clustering
  quadrant[m.prop <0 & m.local<0]<-1 #low Variable, low clustering
  quadrant[m.prop <0 & m.local>0]<-2 #low variable, high clustering
  quadrant[m.prop >0 & m.local<0]<-3 #high Variable, low clustering
  quadrant[local[,5]>signif]<-0
  
  lmoran_plot <- cbind(df,as.tibble(quadrant)) %>%
    rename(Clustering_cat = value) %>%
    mutate(Clustering_cat = as.character(Clustering_cat),
           Clustering_cat = case_when(
             Clustering_cat == "4" ~ paste0("high ", colnames(df)[i], " high clustering"),
             Clustering_cat == "2" ~ paste0("low ", colnames(df)[i], " high clustering"),
             Clustering_cat == "1" ~ paste0("low ", colnames(df)[i]," low clustering"),
             Clustering_cat == "3" ~ paste0("high ", colnames(df)[i], " low clustering")),
           Clustering_cat = as.factor(Clustering_cat))
  
  ggplot() +
    geom_sf(data = lmoran_plot,
            aes(fill = Clustering_cat)) +
    labs(title = paste0("Clustering Category:",colnames(df)[i])) +
    theme(legend.position = "bottom") +
    mapTheme()
  
  ggsave(filename = paste0("Analysis/Figs/Clustering_Cat_Ethnicity/Clustering_Category_",colnames(df)[i],".png"), width = 2500, units = "px")
}


# Merge Cluster categories --------------
## AAPI 
i = 4
name <- colnames(as.vector(df[,i]))
Variable <- as.vector(df[,i, drop = TRUE]) 

local <- localmoran(Variable, listw = queenlist, zero.policy = FALSE)
quadrant <- vector(mode = 'numeric', length=323)
m.prop <- Variable - mean(Variable)
m.local <- local[,1] - mean(local[,1])
signif <- 0.05
quadrant[m.prop >0 & m.local>0]<-4 #high Variable, high clustering
quadrant[m.prop <0 & m.local<0]<-1 #low Variable, low clustering
quadrant[m.prop <0 & m.local>0]<-2 #low variable, high clustering
quadrant[m.prop >0 & m.local<0]<-3 #high Variable, low clustering
quadrant[local[,5]>signif]<-0

LISA_Cluster_Ethnicity <- cbind(df %>%
                                  dplyr::select(GEOID,NAME),
                                as.tibble(quadrant)) %>%
  rename(Clustering_cat = value) %>%
  mutate(Clustering_cat = as.character(Clustering_cat),
         Clustering_cat = case_when(
           Clustering_cat == "4" ~ paste0("high frequency high clustering"),
           Clustering_cat == "2" ~ paste0("low frequency high clustering"),
           Clustering_cat == "1" ~ paste0("low frequency low clustering"),
           Clustering_cat == "3" ~ paste0("high frequency low clustering")),
         Clustering_cat = as.factor(Clustering_cat))

colnames(LISA_Cluster_Ethnicity)[which(names(LISA_Cluster_Ethnicity) == "Clustering_cat")] <- paste0(name[1],"_Cluster")

for(i in 5:length(colnames(df))) {
  name <- colnames(as.vector(df[,i]))
  Variable <- as.vector(df[,i, drop = TRUE]) 
  
  local <- localmoran(Variable, listw = queenlist, zero.policy = FALSE)
  quadrant <- vector(mode = 'numeric', length=323)
  m.prop <- Variable - mean(Variable)
  m.local <- local[,1] - mean(local[,1])
  signif <- 0.05
  quadrant[m.prop >0 & m.local>0]<-4 #high Variable, high clustering
  quadrant[m.prop <0 & m.local<0]<-1 #low Variable, low clustering
  quadrant[m.prop <0 & m.local>0]<-2 #low variable, high clustering
  quadrant[m.prop >0 & m.local<0]<-3 #high Variable, low clustering
  quadrant[local[,5]>signif]<-0
  
  LISA_Cluster_Ethnicity <- cbind(LISA_Cluster_Ethnicity,
                                  as.tibble(quadrant)) %>%
                            rename(Clustering_cat = value) %>%
                            mutate(Clustering_cat = as.character(Clustering_cat),
                                   Clustering_cat = case_when(
                                     Clustering_cat == "4" ~ paste0("high frequency high clustering"),
                                     Clustering_cat == "2" ~ paste0("low frequency high clustering"),
                                     Clustering_cat == "1" ~ paste0("low frequency low clustering"),
                                     Clustering_cat == "3" ~ paste0("high frequency low clustering")),
                                   Clustering_cat = as.factor(Clustering_cat))
  
  colnames(LISA_Cluster_Ethnicity)[which(names(LISA_Cluster_Ethnicity) == "Clustering_cat")] <- paste0(name[1],"_Cluster")
  
}

## Clustering Separation
LISA_Cluster_Ethnicity <- LISA_Cluster_Ethnicity %>%
  st_drop_geometry() %>%
  dplyr::select(GEOID,ends_with("_Cluster")) %>%
  mutate_if(is.factor,as.character)

LISA_Cluster_Ethnicity[is.na(LISA_Cluster_Ethnicity)] <- c("No_Relationship")

LISA_Cluster_Ethnicity <- LISA_Cluster_Ethnicity %>%
  pivot_longer(2:length(colnames(LISA_Cluster_Ethnicity)), names_to = "Clusters_Ethnicity", values_to = "Cluster type Ethnicity")

ACS_Cluster <- left_join(ACS_Cluster, LISA_Cluster_Ethnicity, by = "GEOID")
