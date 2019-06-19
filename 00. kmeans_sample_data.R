##############################################################
########### K-Means Clustering on a sample data ##############
##############################################################

library(data.table)
library(ggplot2)

# Reading and Preparation ##################
# Set data locations
input_loc <- 'C:/Users/rajpurohit anantha/Documents/Cases/04. IDFC First/01. Data/Sample'

# Import the dataset
dataset = fread(file.path(input_loc, 'Mall_Customers.csv'))
dataset = mutate_if(dataset, is.integer, as.numeric)
#dataset = as.matrix(dataset)#, ncol = ncol(dataset[4:5]), nrow = nrow(dataset))
#dim(dataset)
#dataset = apply(dataset,2, as.numeric)

# Find correlations to decide which columns to omit########
dataset_corr <- cor(dataset[,-2])
#ggplot(data = dataset_corr, aes = (Var1, Var2, fill = values()))#+
  #geom_tile(color = "white")

# Code snippet to standardize data (not required for kmeans function) #####################
# It is not required here as "kmeans" function takes care of standardization while clustering
#library(psycho)
#library(tidyverse)
#dataset2 <- dataset %>% psycho::standardize()
#head(dataset2)
#head(dataset)
# End of current code



# Splitting data into test and train (not required) ############
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)


# Find optimal number of Ks for Kmeans clustering ############

# Using the elbow method to find the optimal number of clusters
{
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
#mean(sil[,3])
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Using


}

# using the Silhouette scores for different K values 
get_index<- function(dataset2, index = 'Silhouette', min_k = 2, max_k = 10) 
{ set.seed(6)
  index_score = vector()
  index_score[1] = 0
  for (i in min_k:max_k) {
    #sil <- silhouette(kmeans(dataset, i)$cluster, dist(dataset))
    #dataset_mat <- dataset
    #dataset_mat[] <- lapply(dataset, as.numeric)
    index_out <- clusterCrit::intCriteria(as.matrix(dataset2), kmeans(dataset2, i)$cluster, index) # Pass a matrix with numeric values for first argument. It is a requirement form the function
    #print(sil)
    #print(i)
    #print(sil)
    #print(dim(sil))
    #sil_out = mean(sil[,3])
    #print(sil_out)
    index_score[i] = index_out
  }
  return(index_score)
}

#mean(sil[,3])
plot(1:10,
     silh_score,
     type = 'b',
     main = paste('K selction based on',index,'score'),
     xlab = 'Number of clusters',
     ylab = paste('Average',index,'score'))

columns <- c('Annual Income (k$)','Spending Score (1-100)')
columns <- list(`Annual Income (k$)`,`Spending Score (1-100)`)
# Call funciton to get Silhoutte scores to decide on K
dataset2 <- dataset
index_out <- get_index(dataset[,columns], index = 'Silhouette')
index_out
get_index('Davies_Bouldin')
get_index('Calinski_Harabasz')

library(rlist)

# Doing the iterations with desired variables for K means
variable_iterations <- read.csv(file.path(input_loc,'VariableSelection.csv'), stringsAsFactors = FALSE)
mode(variable_iterations$Iteration1)
index_metrics <- c('Silhouette', 'Davies_Bouldin', 'Calinski_Harabasz', 'C_index', 'Ball_Hall')
indices_out <- data.frame()

itr <- 1
for(index in index_metrics)
{
  for(col in colnames(variable_iterations))
  {
    
    print(col)
    itr_cols <- variable_iterations[str_length(variable_iterations[,col])>0,col]
    index_out <- get_index(dataset[,itr_cols], index = index)
    #index = 'Silhouette'
    if(itr == 1)
    {
      indices_out <- as.data.frame(index_out)
      colnames(indices_out) <- seq(10)
      indices_out$Iteration <- col
      indices_out$Index <- index
      print(as.data.frame(index_out))
    }
    else
    {
      index_out <- as.data.frame(index_out)
      colnames(index_out) <- seq(1,10)
      index_out$Iteration <- col
      index_out$Index <- index
      indices_out <- rbind(indices_out,as.data.frame(index_out))
      print(index_out)
    }
    itr = itr + 1
  }
}

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 5)
y_kmeans = kmeans$cluster
sil <- intCriteria(dataset, y_kmeans, 'Silhouette')
print(sil)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


# Measurement metrics
# library(clusterCrit)

# fclust

