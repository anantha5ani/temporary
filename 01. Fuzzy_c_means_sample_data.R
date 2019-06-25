####################################################################
########### Fuzzy c means Clustering on a sample data ##############
####################################################################

library(ppclust)
library(factoextra)
library(cluster)
library(fclust)

data(iris)
x = iris[,-5]
x

# plot all pairs
pairs(x, col=iris[,5])

# Fuzzy C means clustering ############

res.fcm <- fcm(x, centers = 3)

# Use user generated initial matrix - example. may not use
v0 <- matrix(nrow=3, ncol=4,
             c(5.0, 3.4, 1.4, 0.3,
               6.7, 3.0, 5.6, 2.1,
               5.8, 2.7, 4.3, 1.4),
             byrow=TRUE)
print(v0)

res.fcm <- fcm(x, centers=v0)

v0 <- inaparc::kmpp(x, k=3)$v
print(v0)

res.fcm <- fcm(x, centers=v0)

# Clustering results
res.fcm <- fcm(x, centers=3)
as.data.frame(res.fcm$u)
View(as.data.frame(res.fcm$u))

# Cluster centers initial
res.fcm$v0

# Cluster centers final
res.fcm$v

# Summary of clustering results
summary(res.fcm)

# time taken for clustering run
res.fcm$comp.time

# Run fuzzy C means with multiple starts
res.fcm <- fcm(x, centers=3, nstart = 5)

res.fcm <- fcm(x, centers=3, nstart=5, fixmemb=TRUE) # with fixed membership degrees matrix for all iterations

# Display best solution
res.fcm$func.val

res.fcm$iter

res.fcm$comp.time

res.fcm$best.start

# Display the summary of clustering results
summary(res.fcm)

# Visualization of the clustering results
plotcluster(res.fcm, cp=1, trans=TRUE)

# Cluster plot with fviz_cluster
res.fcm2 <- ppclust2(res.fcm, "kmeans")
factoextra::fviz_cluster(res.fcm2, data = x,
                         ellipse.type = "convex",
                         palette = "jco",
                         repel = TRUE)

# Cluster plot with clusplot
res.fcm3 <- ppclust2(res.fcm, "fanny")

cluster::clusplot(scale(x), res.fcm3$cluster,
                   main = "Cluster plot of Iris data set",
                   color=TRUE, labels = 2, lines = 2, cex=1)

# Validation of the clustering results
res.fcm4 <- ppclust2(res.fcm, "fclust")
idxsf <- SIL.F(res.fcm4$Xca, res.fcm4$U, alpha=1)
idxpe <- PE(res.fcm4$U)
idxpc <- PC(res.fcm4$U)
idxmpc <- MPC(res.fcm4$U)

cat("Partition Entropy: ", idxpe)

cat("Partition Coefficient: ", idxpc)

cat("Modified Partition Coefficient: ", idxmpc)

cat("Fuzzy Silhouette Index: ", idxsf)
