getwd()
setwd('C:/Users/Klaus70/Desktop/Academic Projos/R ML and Data Mining')

#Load the necessary libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(factoextra)
library(cluster)
library(fpc)
library(ggplot2)
library(ggpubr)
library(NbClust)

# Load the dataset
vehicles <- read_excel("Vehicles.xlsx",sheet = 1, col_names = TRUE)

str(vehicles)

#Dropping the samples column
vehicles_subset <- subset(vehicles, select = -Samples)
str(vehicles_subset)
summary(vehicles_subset)


#Checking and removing outliers
# Z-score normalization by first Droping the class variable
vehicles_scaled <- vehicles_subset %>% 
  select(-Class) %>% 
  mutate_all(scale)

#Visualizing the outliers
vehicles_scaled %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_boxplot()

# Detect and remove outliers
threshold <- 3
vehicles_outliers_removed <- vehicles_scaled %>% 
  filter_all(all_vars(abs(.) < threshold), .preserve = TRUE)

#Determining optimal number of clusters
# 1) Finding the number of clusters using NbClust
set.seed(123)
nb <- NbClust(vehicles_outliers_removed, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nb$Best.nc

#Finding the optimal number of clusters using elbow method
set.seed(123)
wss <- vector(length = 10)
for (i in 1:10) {
  kmeans_fit <- kmeans(vehicles_outliers_removed, centers = i, nstart = 25)
  wss[i] <- kmeans_fit$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of clusters", ylab = "Within cluster sum of squares")

# Finding the number of clusters using the gap statistic method
set.seed(123)
gap_stat <- clusGap(vehicles_outliers_removed, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
plot(gap_stat, main = "Gap statistic")


#Finding the number of clusters using the silhouette method
set.seed(123)
sil_width <- vector(length = 10)
for (i in 2:10) {
  kmeans_fit <- kmeans(vehicles_outliers_removed, centers = i, nstart = 25)
  sil_width[i] <- mean(silhouette(kmeans_fit$cluster, dist(vehicles_outliers_removed)))
}
plot(2:10, sil_width[2:10], type = "b", xlab = "Number of clusters", ylab = "Silhouette width")


# Perform k-means clustering using k=3
set.seed(123)
kmeans_fit <- kmeans(vehicles_outliers_removed, centers = 3, nstart = 25)
kmeans_fit

# Calculate internal evaluation metrics

bss <- sum((colMeans(vehicles_outliers_removed[kmeans_fit$cluster == 1,]) - colMeans(vehicles_outliers_removed))^2) +
  sum((colMeans(vehicles_outliers_removed[kmeans_fit$cluster == 2,]) - colMeans(vehicles_outliers_removed))^2) +
  sum((colMeans(vehicles_outliers_removed[kmeans_fit$cluster == 3,]) - colMeans(vehicles_outliers_removed))^2)
print(bss)

#wss
wss <- sum(kmeans_fit$withinss)
print(wss)

#tss
tss <- wss + bss
print(tss)

#Print BSS/TSS ratio
cat("BSS/TSS ratio:", bss/tss, "\n")

#Print BSS and WSS indices
cat("BSS:", bss, "\n")
cat("WSS:", wss, "\n")

#Calculate average silhouette width
silhouette_avg <- silhouette(kmeans_fit$cluster, dist(vehicles_outliers_removed))
cat("Average silhouette width:", mean(silhouette_avg[,3]), "\n")

#Plot silhouette
plot(silhouette_avg)



#2nd Subtask
# Perform PCA on the vehicles dataset
pca_vehicles <- prcomp(vehicles_scaled, center = TRUE, scale. = TRUE)

# Show eigenvalues and eigenvectors
summary(pca_vehicles)

# Show cumulative score per principal components (PC)
pca_var <- pca_vehicles$sdev^2
pca_var_percent <- round(pca_var/sum(pca_var)*100, 2)
cumulative_var_percent <- cumsum(pca_var_percent)

cbind(PCA_var=pca_var, `Percent Variance`=pca_var_percent, `Cumulative Variance`=cumulative_var_percent)

# Create a new transformed dataset with principal components as attributes
vehicles_pca <- as.data.frame(predict(pca_vehicles, vehicles_scaled))

# Apply the four "automated" tools to the new pca-based dataset
set.seed(123)

# NBclust
nb_results <- NbClust(vehicles_pca, min.nc=2, max.nc=10, method="kmeans", index="all")
print(nb_results)

# Elbow method
wss <- numeric(10)
for(i in 1:10) wss[i] <- sum(kmeans(vehicles_pca, centers=i, nstart=25)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Gap statistic
set.seed(123)
gap_stat <- clusGap(vehicles_pca, FUN = kmeans, nstart = 25, K.max = 30, B = 50)# I used K.max = 30 as the first 10 did not converge
plot(gap_stat, main="Gap Statistic")

# Silhouette method
sil_width <- c(NA)
for(i in 2:10){
  kmeans_fit <- kmeans(vehicles_pca, centers=i, nstart=25)
  sil_width[i] <- silhouette(kmeans_fit$cluster, dist(vehicles_pca))$avg.width
}
plot(2:10, sil_width[2:10], type="b", xlab="Number of Clusters", ylab="Silhouette Width")

###################################################################
#Perform k-means clustering using k=3
set.seed(123)
kmeans_fit_pca <- kmeans(pca_df[, chosen_pcs], centers = 3, nstart = 25)
kmeans_fit_pca

#Calculate internal evaluation metrics
bss_pca <- sum((colMeans(pca_df[, chosen_pcs][kmeans_fit_pca$cluster == 1, ]) -
                  colMeans(pca_df[, chosen_pcs][kmeans_fit_pca$cluster == 2, ]))^2) +
  sum((colMeans(pca_df[, chosen_pcs][kmeans_fit_pca$cluster == 1, ]) -
         colMeans(pca_df[, chosen_pcs][kmeans_fit_pca$cluster == 3, ]))^2) +
  sum((colMeans(pca_df[, chosen_pcs][kmeans_fit_pca$cluster == 2, ]) -
         colMeans(pca_df[, chosen_pcs][kmeans_fit_pca$cluster == 3, ]))^2)
tss_pca <- sum(apply(pca_df[, chosen_pcs], 2, var)) * (nrow(pca_df[, chosen_pcs]) - 1)
wss_pca <- sum(kmeans_fit_pca$withinss)
cat("BSS/TSS ratio for k-means clustering on the new pca-dataset:", bss_pca/tss_pca, "\n")
cat("BSS index for k-means clustering on the new pca-dataset:", bss_pca, "\n")
cat("WSS index for k-means clustering on the new pca-dataset:", wss_pca, "\n")

#Plot the silhouette
silhouette_pca <- silhouette(kmeans_fit_pca$cluster, dist(pca_df[, chosen_pcs]))
silhouette_pca
cat("Average silhouette width for k-means clustering on the new pca-dataset:", mean(silhouette_pca[, 3]), "\n")
plot(silhouette_pca)

#Calculate Calinski-Harabasz Index
ch_index_pca <- calinski.harabasz(pca_df[, chosen_pcs], kmeans_fit_pca$cluster)
cat("Calinski-Harabasz Index for k-means clustering on the new pca-dataset:", ch_index_pca, "\n")