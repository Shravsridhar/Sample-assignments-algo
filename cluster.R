library(factoextra)
library(dendextend)
library(corrplot)

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
abline(h = 6, lty = 3)


# Compute correlation matrix
dend_list <- dendlist("Complete" = hcl2, "Single" = hcl1)
cors <- cor.dendlist(dend_list)
round(cors,2)
# Plot
corrplot(cors, "pie", "lower")

# Computing Cluster membership
cm1 <- cutree(hcl_single, k = 6)
cm1

cm2 <- cutree(hcl_comp, k = 6)
cm2

# Number of members in each cluser
table(cm1)
table(cm2)

# Getting the names for the members of cluster 1
rownames(cereal.norm.df)[cm1 == 1]
# This way we can get the cluster names for each specified cluster

# Calculating the cluster centroid columnwise
# ouput shows the centroid of cluster one for single linkage
colMeans(subset(cereal.norm.df, cm1 == 1))
# This can be done similarly for all types of clustering 

colMeans(subset(cereal.norm.df, cm2 == 1))
cereal.mat<- as.matrix(cereal.norm.df)

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
cluster.means(cereal.mat,hcl_single,6)

# Compare dendogram from single and complete with the cluster centroids 

# Comparing the results from complete and singl linkage 
table(cm1,cm2)
# It is visible that in complete linkage the 6 clusters differ significantly from the single linkage.
# The one big cluster in single linkage (4th cluster) is now split into 4 medium sized clusters with complete linkage.
# Complete :better - single seems to be placing outliers in its own cluster

# Number of clusters formed at a particular cutoff : draw vertical line at the attribute and count the no of lines intersectin it - # of clusters
