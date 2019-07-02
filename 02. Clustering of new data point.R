#################################################################################################
########### Assigning new members to appropriate clusters using existing centroids ##############
#################################################################################################

library(data.table)
library(dplyr)
library(stats)
library(tidyr)

# Reading and Preparation ##################
# Set data locations
input_loc <- 'C:/Users/rajpurohit anantha/Documents/Cases/04. IDFC First/01. Data/Sample'

# Import the dataset
dataset = fread(file.path(input_loc, 'Mall_Customers.csv'))
dataset = mutate_if(dataset, is.integer, as.numeric)

dataset_cluster <- select(dataset, 3:5) %>% slice(1:198)
dataset_new <- select(dataset, 3:5) %>% slice(199:200)



kmeans <- kmeans(x = dataset_cluster, centers = 5)
y_kmeans = kmeans$cluster

dataset_cluster$cluster <- kmeans$cluster

summary(dataset_cluster)
table(dataset_cluster$cluster)

assign_cluster <- function(dataset_cluster, dataset_new, dist='euclidean')
{
  # Find centroids of the data by average and median. Average is for new entry, median is for analysis
  dataset_cluster2 <- dataset_cluster %>% group_by(cluster) %>% summarize_all(funs(mean))
  dataset_new$cluster <- paste("new",rownames(dataset_new), sep="_")
  
  dataset_cluster_cent <- rbind(dataset_cluster2, dataset_new)
  
  distance_matrix <- stats::dist(dataset_cluster_cent, method='euclidean')
  distance_matrix <- as.data.frame(as.matrix(distance_matrix))
  rownames(distance_matrix) <- rownames(dataset_cluster_cent)
  rownames(distance_matrix) <- dataset_cluster_cent$cluster
  colnames(distance_matrix) <- dataset_cluster_cent$cluster
  
  distance_matrix2 <- distance_matrix[,names(distance_matrix) %like% "new"]
  distance_matrix2 <- distance_matrix2[!rownames(distance_matrix2) %like% "new",]
  #distance_matrix2$row_names <- rownames(distance_matrix2)
  clusters_new <- list()
  for (name in names(distance_matrix2))
  {
    name_min <- paste0(name, "_min")
    distance_matrix2[,name_min] <- min(distance_matrix2[,name])
    clust_out <- max(ifelse(distance_matrix2[,name] == distance_matrix2[,name_min], rownames(distance_matrix2), 0))
    clusters_new[name] <- clust_out
  }
  
  dataset_new$cluster <- clusters_new
  
  return(dataset_new)
}

dataset_c <- dataset_cluster
dataset_n <- select(dataset, 3:5) %>% slice(199:200)

dataset_n_c <- assign_cluster(dataset_c, dataset_n)
