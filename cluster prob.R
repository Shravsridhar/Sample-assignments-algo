library(tidyverse) 
library(cluster)    
library(factoextra) 
library(gridExtra)
library(dendextend)

# Loading the data
pharm.df<-read.csv("Pharmaceuticals.csv")

# Preprocessing the data
data.df <- pharm.df[,3:11]

# Normalzing
data.df <- scale(data.df, center = T)

row.names(data.df) <- row.names(pharm.df)

# visualization of data
heatmap(as.matrix(data.df))

# Calculating Dissimilarity matrix
distance <- get_dist(data.df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
d <- dist(data.df, method = "euclidean")

# trying k means with different number of clusters
k2<- kmeans(data.df, centers = 2,nstart = 20)
k3 <- kmeans(data.df, centers = 3, nstart = 20)
k4 <- kmeans(data.df, centers = 4, nstart = 20)
k5 <- kmeans(data.df, centers = 5, nstart = 20)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data.df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data.df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data.df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data.df) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# Choosing Optimal # of clusters
fviz_nbclust(data.df, kmeans, method = "wss")
fviz_nbclust(data.df, kmeans, method = "silhouette")

# Choosing k= 2 as the # of clusters
km <- kmeans(data.df, 2, nstart = 20)
print(km)

# Visualizing Cluster
fviz_cluster(km, data = data.df)

# Cluster Membership
km$cluster

# Cluster centroids
km$centers

km$totss

# Within cluster sum of squares
km$withinss

km$betweenss

# Distance between cluster centroids 
dist(km$centers) 

# Hierarchial Clustering
hc1 <- hclust(d, method = "single")
plot(hc1, hang = -1, ann = FALSE)

hc2 <- hclust(d, method = "complete")
plot(hc2, hang = -1, ann = FALSE)

hc3 <- hclust(d, method = "average")
plot(hc3, hang = -1, ann = FALSE)

# To find better method from the 3, agglomerative coefficient(measures the amount of clustering structure found) can be used
hc1 <- agnes(data.df, method = "single")
hc1_ac<-hc1$ac
hc2 <- agnes(data.df, method = "complete")
hc2_ac<-hc2$ac
hc3 <- agnes(data.df, method = "average")
hc3_ac<-hc3$ac

# complete linkage has the highest coefficient : Choosing complete linkage to perform clustering
hcomp <- hclust(d, method = "complete")
pltree(hc2, hang = -1, main = "Dendrogram") 

# finding optiamal k 
fviz_nbclust(data.df, FUN = hcut, method = "wss")
fviz_nbclust(data.df, FUN = hcut, method = "silhouette")

ct1 = cutree(hcomp, k = 2)
plot(hcomp, cex = 0.6)
rect.hclust(hcomp, k = 2, border = 2:5)
fviz_cluster(list(data = data.df, cluster = ct1))

# Clustering performed on the data 1:9 from the given dataset  
# Algorithms applied : K-means and the Hierarchical clustering

# Relation between categorical variable and clusters
table(pharm.df$Exchange, km$cluster)
table(pharm.df$Location, km$cluster)
table(pharm.df$Median_Recommendation, km$cluster)


# No other significant pattern is found with respect to the rest of the labels in data

# Cluster 1 : profitable firms
# Cluster 2 : Non- profitable firms

