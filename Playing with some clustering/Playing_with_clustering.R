df <- read.csv("C:/Users/lucia/Desktop/UniBG/I semester I year/R/relazione_laurea_triennale/Mall_Customers.csv")
library("dplyr")
library("GGally")
library("factoextra")
library("dbscan")
library("fpc")
library ("cluster")

glimpse(df)
df$CustomerID<-NULL
glimpse(df)

#options(repr.plot.width=12, repr.plot.height=8)
ggpairs(df,aes(color=Gender))+theme_bw(base_size = 15)

#scaling
df$Gender<-as.numeric(as.character(factor(df$Gender, c("Male", "Female"), labels=c(1,0))))
glimpse(df)

dfs<-scale(df)
dfs

# K-Means
withinrss<-0
for (i in 1:10){
  k_m<-kmeans(dfs, centers=i, nstart=10, iter.max=50) #starts with random centroids 10 times
  withinrss[i]<-k_m$tot.withinss
}

wss<-0
for (n in 1:10){
  km<-kmeans(dfs, centers=n, nstart=10, iter.max=50) #starts with random centroids 10 times
  wss[n]<-km$tot.withinss
}
#options(repr.plot.width=5, repr.plot.height=5)
plot(withinrss, type="b", xlab="Number of clusters", ylab="Sum of squares within groups")

set.seed(1)
km<-kmeans(dfs, centers=5, nstart=5, iter.max=500)
km

df$kmeans = km$cluster
df%>%
  group_by(kmeans)%>%
  summarise(Age_mean= mean(Age),
            AnIncome_mean= mean(Annual.Income..k..),
            SpenScore_mean= mean(Spending.Score..1.100.),
            Gender=(mean(Gender)),
            Count=(length(kmeans)))

fviz_cluster(eclust(dfs,FUNcluster="kmeans", k=5, hc_metric="euclidean"),dfs, 
             geom = "point") 
fviz_silhouette(eclust(dfs, FUNcluster="kmeans", k=5, hc_metric="euclidean"))

#To see how well the data fall in the clusters and decide if the number of clusters 
#should be changed, Silhouette Analysis can be used. 

# So, we have 5 groups here:
#   
# A group of women, mean age of 49 years with relative low income and low score.
# A group of men/women, mean age of 40 years with high income and low score.
# A group of men, mean age of 56 years with relative low income and low score.
# A group of women, mean age of 28 years with medium income and relative high score.
# A group of men, mean age of 28 years with medium income and relative high score.

# How to know if this is a good clustering? Let's try sillhouette analysis. 
# The silhouette plot shows, basically, how similar observations from a cluster 
# are to observations from a neighbor cluster. The coefficient goes from -1 to 1 
# (well clustered).

df$kmeans<-NULL

# Hierarquical
# With this method, we don't have to specify the number of clusters.
# Here I'm going to use the agglomerative aproach where closest single observations 
# will be combined until form a single one cluster. Divisive aproach is kind like the 
# oposit, a single cluster will be splitted. But how measure the similarity between 
# clusters? Some of the options are the complete linkage and single linkage. While 
# complete linkage considers the distance between the most dissimilar observations, 
# the single linkage considers the distance between the most similar observations.
# 
# How it works? Basically in case of agglomerative with complete linkage:
#   
# 1- a distance (similarity) matrix is calculated.
# 2- single samples (inicially considered as clusters) will be merged based on distance between the most dissimilar samples.
# 3- the matrix is updated
# 4- the steps are repeated until remains one cluster with all observations.

hm<-hclust(dist(dfs)) # Euclidian distance and complete linkage as default
options(repr.plot.width=15, repr.plot.height=6)
plot(hm)

df$hclust <- cutree(hm, k = 4)

df%>%
  group_by(hclust)%>%
  summarise(Age_mean= mean(Age),
            AnIncome_mean= mean(Annual.Income..k..),
            SpenScore_mean= mean(Spending.Score..1.100.),
            Gender=(mean(Gender)),
            Count=(length(hclust)))

# So, we have 4 groups here:
#   
# A group of men/women, mean age of 27 years with relative low income and medium score.
# A group of men/women, mean age of 55 years with relative low income and low score.
# A group of men/women, mean age of 33 years with relative high income and high score.
# A group of men/women, mean age of 40 years with relative high income and very low score.

fviz_cluster(eclust(dfs,FUNcluster="hclust", k=4, hc_metric="euclidean", 
                    hc_method="complete"), dfs, geom = "point")
fviz_silhouette(eclust(dfs,FUNcluster="hclust", k=4, hc_metric="euclidean", 
                       hc_method="complete"))

df$hclust <- NULL

# DBSCAN
# This method builds clusters based on density. MinPts and eps are parameters to 
# be estimated. MinPoints is the minimum neighbors within eps radius of neighborhood.
# 
# How it works? Basically:
#   
# after distance of each point to others being calculated, neighborhood is defined and points are classified in core (neighbors>=MinPoints), border (neighbors < MinPoints but is neighbor of some core point) or outlier.
# clusters based on core points continue to be building.
# definition of noise/outliers points.
# While Kmeans and Hierarquical are good for finding spherical/convex clusters in 
# datasets with low noise and outliers, DBSCAN has some advantages:
#   doesn't need the number of clusters to be pre-specified.
# it can find differents shapes of clusters
# it can find outliers (not all points have to be assign to a cluster)

# optimal eps - what is the average distance of points to its k nearest 
#neighbors? Pay attention to the elbow.
options(repr.plot.width=5, repr.plot.height=5)
kNNdistplot(dfs, k=3) # k= MinPoints

set.seed(1)
dm<-fpc::dbscan(dfs, eps=0.8, MinPts=3)
df$dbscan = dm$cluster

df%>%
  group_by(dbscan)%>%
  summarise(Age_mean= mean(Age),
            AnIncome_mean= mean(Annual.Income..k..),
            SpenScore_mean= mean(Spending.Score..1.100.),
            Gender=(mean(Gender)),
            Count=(length(dbscan)))

# So, we have (besides 11 outliers) 4 groups here:
#   
# A group of men, mean age of 39 years with medium income and medium score.
# A group of women, mean age of 38 years with medium income and medium score.
# A group of men, mean age of 61 years with low income and very low score.
# A group of women, mean age of 44 years with high income and low score.

fviz_cluster(dm, dfs, geom = "point")

fviz_silhouette(silhouette(df$dbscan , dist(dfs)))

# Until now, kmeans seems to be the best method. Remember he gave us two groups 
# mean age of 28 (one male and another female) with medium income and relative high 
# score, a group of women mean age of 48 with relative low income and low score, 
# group of men mean age of 56 years with relative low income and low score, but 
# also a group with males and females mean age of 38 with relative high income and 
# low score (which seems like an oportunity).













