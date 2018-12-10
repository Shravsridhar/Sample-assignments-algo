library(factoextra)
library(dendextend)
library(corrplot)
library(heatmap.plus)
library(gplots)

# Loading the data
cereal.df<-read.csv("Cereals.csv")

# Data Preprocessing 
# Removal of missing value data
cereal.df<-na.omit(cereal.df)
cereal.df<-cereal.df[,-2:-3]

# Check
any(is.na(cereal.df))
complete.cases(cereal.df)

# Converting col 1 names as row names
row.names(cereal.df)<-cereal.df[,1]
cereal.df<-cereal.df[,-1]

# Normalizing 
cereal.norm.df<-sapply(cereal.df, scale)
cereal.mat<- as.matrix(cereal.norm.df)

# Addition of row names
row.names(cereal.norm.df)<-row.names(cereal.df)

# Computing distance : euclidean 
d<-dist(cereal.norm.df,method="euclidean")

# Running hierarchial cluster with single linkage
hcl_single<-hclust(d,method="single")
# Plot and creation of Dendogram 
plot(hcl_single,hang=-1,ann= FALSE)
hcl1<-as.dendrogram (hcl_single)

# Running hierarchial cluster with complete linkage
hcl_comp<-hclust(d,method="complete")
# Plot and creation of Dendogram 
plot(hcl_comp,hang=-1,ann= FALSE)
hcl2<-as.dendrogram (hcl_comp)

# Tree cutting - to compute cluster membership
hcl_single
hcl_comp
# Print row labels in the order they appear in the tree
hcl_single$labels[hcl_single$order]
hcl_comp$labels[hcl_comp$order]

ct1 <- cutree(hcl_single, h=max(hcl_single$height)/2)
ct1[hcl_single$labels[hcl_single$order]] 


ct2 <- cutree(hcl_comp, h=max(hcl_comp$height)/2)
ct2[hcl_comp$labels[hcl_comp$order]] 

# Comparing with the original dataset
table(ct1,row.names(cereal.norm.df))
table(ct2,row.names(cereal.norm.df))


# plot for visualizing thier correlation

mycol <- colorpanel(40, "red", "lightblue", "black")
pheatmap(cereal.mat,color = mycol)

# Compute correlation matrix

dend_list <- dendlist("Complete" = hcl2, "Single" = hcl1)
cors <- cor.dendlist(dend_list)
round(cors,2)
# Plot
corrplot(cors, "pie", "lower")

# Number of members in each cluser
table(ct1)
table(ct2)

# Getting the names for the members of cluster 1
rownames(cereal.norm.df)[ct1 == 1]
# This way we can get the cluster names for each specified cluster

# Calculating the cluster centroid columnwise
# ouput shows the centroid of cluster one for single linkage
colMeans(subset(cereal.norm.df, ct1 == 1))
# This can be done similarly for all types of clustering 

# Function to calculate Cluster Centroid
cluster.means <- function(x, res.clust, groups)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  means <- tapply(x, list(rep(cutree(res.clust, groups), ncol(x)),
                          col(x)),
                  mean)
  dimnames(means) <- list(NULL, dimnames(x)[[2]])
  return(as.data.frame(means))
}
# x is the input data, res.clust is clustered obj,groups is # of grps to cut dendogram into
cluster.means(cereal.mat,hcl_single,max(hcl_single$height)/2)
cluster.means(cereal.mat,hcl_comp,max(hcl_comp$height)/2)


# Compare dendogram from single and complete with the cluster centroids 

# Comparing the results from complete and singl linkage 
table(ct1,ct2)
# if k is 6 
cm1 <- cutree(hcl_single, k = 6)
cm2 <- cutree(hcl_comp, k = 6)
table(cm1,cm2)
# It is visible that in complete linkage the 6 clusters differ significantly from the single linkage.
# The one big cluster in single linkage (4th cluster) is now split into 4 medium sized clusters with complete linkage.
# Complete :better - single seems to be placing outliers in its own cluster

# Number of clusters formed at a particular cutoff : draw vertical line at the attribute and count the no of lines intersectin it - # of clusters
# let cutoff be 2 or 3 since max range of single and complete is within 2 to 5

# Choosing complete linkage - 9 clusters 
# to find cereals in each cluster 
rownames(cereal.norm.df)[ct2 == 3]

# let vertical line be drawn from 6 dist - # of clusters chosen= 8

# We couldl recommend them with cereals from cluster 1,2 
rownames(cereal.norm.df)[ct2 == 1]
rownames(cereal.norm.df)[ct2 == 2]

# cluster 8 could also be suggested
rownames(cereal.norm.df)[ct2 == 8]

# Normalizing is required prior clustering the data. It is always preferable to work with scaled data to give proper results


