# Sun_Jonathon_AsAmClustering

## Abstract
*Asian Americans comprise of a panoply of differing ethnic national origins, it is important to recognize the power in a shared political identity; however, a shared racial identity, can minimize the differences between ethnic groups by consolidating Asian American statistics into one consolidated group. To address this, I analyze census tracts in Philadelphia where South Asian, Southeast Asians, East Asians and Filipinos, are highly clustered and have high frequency utilizing Local Indices of Spatial Autocorrelation (LISA). I use college access indicators such as median income, frequency of college attainment, and racial indicators to argue that Asian Americans have different experiences based on ethnic background related to their geographic boundaries, which shape their college access opportunities.*

# Objective
Existing literature has shown that there are differences across ethnic group and socio-economic status, I expand upon this literature by including a geographic component utilizing geographic information systems (GIS) to map the socioeconomic and racial indicators in areas that are highly clustered and high frequency.

## Data 
* American Community Survey 2010 - 2019

## Methods

### Moran's I
To begin this analysis, I first determined if broadly Asian populations were spatially autocorrelated in Philadelphia using Moran’s I. Moran’s I has been widely used to test for spatial autocorrelation or spatial dependencies and its value determines the strength of autocorrelation indicating how clustered values are. Values that are closer to 1 indicate strong positive autocorrelation, while values closer to -1 indicate negative autocorrelation that being how repelled values are. Conversely, if values are positively autocorrelated they are spatially clustered. If the value is close to 0, then there is no spatial autocorrelation, indicating a random pattern. Unlike Pearson's correlation coefficient, Moran's I does not always lie within -1 and 1 and can be situated beyond each of these values.

### Local Indices of Spatial Autocorrelation
LISA is defined as having two properties. (1) the LISA for each observation gives an indication of the extent of significant spatial clustering of similar values around that observation and (2) the sum of LISAs for all observations is proportional to a global indicator of spatial association. In our case each census block is calculated individually and then summed to provide the I from Moran's I. The calculated LISA describes each blocks effects of the clustering within the data.

### Kruskil-Wallis and Pairwise Wilcox
I ran a Kruskil-Wallis test, because the data is non-parametric. The null hypothesis for the Kruskil-Wallis test is that the mean ranks of all the groups are the same. While the alternate hypothesis is that the mean ranks of the groups are different. Next, to determine which groups were significantly different I used a pairwise Wilcox test to identify which racial/ethnic groups were different from each other.

<center>
| 100K+ Income  |Bachelors or More  |Black Population   |White Population   |
|---|---|---|---|
| <img src = "https://github.com/CPLN-680-Spring-2022/Sun_Jonathon_AsAmClustering/blob/main/Analysis/Figs/Kruskal_Box/%24100%2BK_IncomePanEthnic.jpg?raw=true" width = "200" height = "200">   | <img src = "https://github.com/CPLN-680-Spring-2022/Sun_Jonathon_AsAmClustering/blob/main/Analysis/Figs/Kruskal_Box/Bachelor_more_Edu_AttainmentPanEthnic.jpg?raw=true" wight = "200" height = "200">   | <img src = "https://github.com/CPLN-680-Spring-2022/Sun_Jonathon_AsAmClustering/blob/main/Analysis/Figs/Kruskal_Box/Black_RacePanEthnic.jpg?raw=true" width = "200" height = "200">  | <img src = "https://github.com/CPLN-680-Spring-2022/Sun_Jonathon_AsAmClustering/blob/main/Analysis/Figs/Kruskal_Box/white_RacePanEthnic.jpg?raw=true" width = "200" height = "200">  |
</center>

## Findings
Asian Americans racial/ethnic groups are clustered across Philadelphia. There are significant differences in Education, Income, and proximity to White and Black populations

