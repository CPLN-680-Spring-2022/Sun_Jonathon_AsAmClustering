# Moran's I
# Log transform the population to even out the skew

ACS_Cluster %>%
  filter(is.na(estimate) == TRUE) 


# Moran's I using df as object ------------------------------

## Make an empty data frame so I have all the moran's I, P-values, and variables ---------------------------------
MoransITable <- data.table::data.table(matrix(ncol = 3, nrow = 0))
x <- c("Morans_I","P-Value","Variable")
colnames(MoransITable) <- x

# Looping through Moran's I on other variables ------------------------------

df <- ACS_Cluster %>%
  na.omit() %>%
  filter(Category == unique(ACS_Cluster$Category)[1])

for(i in 1:length(unique(ACS_Cluster$Category))) {
  if(i == 11) next
  loop <- ACS_Cluster %>%
    na.omit() %>%
    filter(Category == unique(ACS_Cluster$Category)[i])
  
  Variable <- loop$estimate
  
  #queen neighbors
  queen <- poly2nb(loop, row.names = loop$GEOID)
  
  #global Moran's I
  queenlist <- nb2listw(queen, style = 'W') #Spatial weights for neighors. 
  
  #running Moran's I
  MoranI <- moran(Variable, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$'I'
  
  #Checking significance
  moranMC <- moran.mc(Variable, queenlist, nsim=999)
  moranMCres <- moranMC$res
  moran.plot(Variable, queenlist)
  
  hist(moranMCres, freq = 10000000, nclass = 100)
  abline(v = moran(loop$estimate, queenlist, n = length(queenlist$neighbours), S0=Szero(queenlist))$'I', col='red')
  
  bind <- t(c(MoranI, moranMC$p.value,unique(loop$Category)[1]))
  colnames(bind) <- x
  
  MoransITable <- rbind(MoransITable,bind)
}

write.csv(MoransITable,"Analysis/MoranI_Cluster.csv")

df <- ACS_Cluster %>%
        filter(Category %in% unique(ACS_Cluster$Category)[c(4,7,8,9,10)]) %>%
        group_by(GEOID,NAME,Category) %>%
        summarize(estimate = sum(estimate))%>%
        pivot_wider(names_from = Category, values_from = estimate) %>%
        st_as_sf()


#Moran's I statistical Significance -------------

for(i in 4:length(colnames(df))) {
  
  queen <- poly2nb(df, row.names = df$GEOID)
  queenlist <- nb2listw(queen, style = 'W')
  
  Variable <- as.vector(df[,i, drop = TRUE]) 
  
  moran.map <- cbind(df, localmoran(Variable, queenlist)) %>%
    mutate(Significant = `Pr.z....E.Ii..` < 0.05)
  ggplot() +
    geom_sf(data = moran.map, aes(fill = Significant)) +
    labs(title = paste0("P-Value Clustering:",colnames(df)[i])) +
    mapTheme()
  
  ggsave(filename = paste0("Analysis/Figs/Pvalue/P-value_Clustering_Category_",colnames(df)[i],".png"), width = 2000, units = "px", plot = last_plot())
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
    mapTheme()
  
  ggsave(filename = paste0("Analysis/Figs/Clustering_Cat/Clustering_Category_",colnames(df)[i],".png"), width = 2500, units = "px")
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

LISA_Clusters <- cbind(df,as.tibble(quadrant)) %>%
  rename(Clustering_cat = value) %>%
  mutate(Clustering_cat = as.character(Clustering_cat),
         Clustering_cat = case_when(
           Clustering_cat == "4" ~ paste0("high frequency high clustering"),
           Clustering_cat == "2" ~ paste0("low frequency high clustering"),
           Clustering_cat == "1" ~ paste0("low frequency low clustering"),
           Clustering_cat == "3" ~ paste0("high frequency low clustering")),
         Clustering_cat = as.factor(Clustering_cat)) 

colnames(LISA_Clusters)[which(names(LISA_Clusters) == "Clustering_cat")] <- paste0(name[1],"_Cluster")

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
  
  LISA_Clusters<- cbind(LISA_Clusters,
                                  as.tibble(quadrant)) %>%
    rename(Clustering_cat = value) %>%
    mutate(Clustering_cat = as.character(Clustering_cat),
           Clustering_cat = case_when(
             Clustering_cat == "4" ~ paste0("high frequency high clustering"),
             Clustering_cat == "2" ~ paste0("low frequency high clustering"),
             Clustering_cat == "1" ~ paste0("low frequency low clustering"),
             Clustering_cat == "3" ~ paste0("high frequency low clustering")),
           Clustering_cat = as.factor(Clustering_cat))
  
  colnames(LISA_Clusters)[which(names(LISA_Clusters) == "Clustering_cat")] <- paste0(name[1],"_Cluster")
  
}

## Clustering Separation
LISA_Clusters <- LISA_Clusters %>%
                  st_drop_geometry() %>%
                  dplyr::select(GEOID,ends_with("_Cluster")) %>%
                  mutate_if(is.factor,as.character)

LISA_Clusters[is.na(LISA_Clusters)] <- c("No_Relationship")

LISA_Clusters <- LISA_Clusters %>%
                  pivot_longer(2:length(colnames(LISA_Clusters)), names_to = "Clusters", values_to = "Cluster type")

ACS_Cluster <- left_join(ACS_Cluster, LISA_Clusters, by = "GEOID")
