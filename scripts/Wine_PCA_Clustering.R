# Load necessary libraries
library(readxl)      # Load 'readxl' library to enable reading from Excel files.
library(NbClust)     # Load 'NbClust' library for determining the optimal number of clusters.
library(factoextra)  # Load 'factoextra' for enhanced cluster visualization and Principal Component Analysis (PCA).
library(cluster)     # Load 'cluster' library which contains various clustering algorithms.
library(ggplot2)     # Load 'ggplot2' for data visualization through high-level plotting.
library(fpc)         # For cluster validation statistics

# Display the current working directory
getwd()              # Display the current working directory to ensure correct file paths.

# Read data from the Excel file
white_wine <- read_excel("/Users/sanch_bo/Desktop/ML_CW/Whitewine_v6.xlsx")  # Read data from specified Excel file into 'white_wine' dataframe.

# Define a function to calculate the extremeness of outliers in a column based on the IQR method
calculate_extremeness <- function(column) {
  Q1 <- quantile(column, 0.25)   # Compute the first quartile (25%) for the column.
  Q3 <- quantile(column, 0.75)   # Compute the third quartile (75%) for the column.
  IQR <- Q3 - Q1                 # Calculate the Interquartile Range (IQR).
  lower_bound <- Q1 - 1.5 * IQR  # Define the lower bound for outlier detection.
  upper_bound <- Q3 + 1.5 * IQR  # Define the upper bound for outlier detection.
  
  # Calculate extremeness values: how far data points lie outside the IQR bounds.
  extremeness <- ifelse(column < lower_bound, lower_bound - column, 
                        ifelse(column > upper_bound, column - upper_bound, 0))
  return(extremeness)
}

# Apply outlier detection to each column (except the last) and store the results
extremeness_scores <- as.data.frame(lapply(white_wine[-ncol(white_wine)], calculate_extremeness))  # Apply 'calculate_extremeness' to each column except the last and store in 'extremeness_scores'.

# Aggregate the outlier scores across all columns for each row
row_extremeness <- rowSums(extremeness_scores)  # Sum extremeness scores across columns for each row.

# Identify the top 90 rows with highest outlier scores
top_90_outliers <- order(row_extremeness, decreasing = TRUE)[1:90]  # Get indices of top 90 rows with highest outlier scores.

# Remove identified outlier rows from the dataset
white_wine_cleaned <- white_wine[-top_90_outliers, ]  # Remove top outliers from the dataset.

# Scale the cleaned data, excluding the last column
white_wine_cleaned_data <- scale(white_wine_cleaned[-ncol(white_wine_cleaned)])  # Scale the data for each column except the last.

# Convert the scaled data from a matrix to a data frame and reattach column names
white_wine_cleaned_scaled <- as.data.frame(white_wine_cleaned_data)  # Convert matrix to data frame.
colnames(white_wine_cleaned_scaled) <- colnames(white_wine_cleaned[-ncol(white_wine_cleaned)])  # Set column names from original data.

# Display a snapshot and structure of the cleaned and scaled dataset
head(white_wine_cleaned_scaled)  # Show the first few rows of the cleaned and scaled dataset.
str(white_wine_cleaned_scaled)   # Display the structure of the cleaned and scaled dataset.

# Display summary statistics of the cleaned and scaled dataset
summary(white_wine_cleaned_scaled)  # Show summary statistics for the cleaned and scaled dataset.

# Set a random seed for reproducibility in clustering
set.seed(1234)  # Set a random seed to ensure reproducibility of the clustering results.

pca_result <- prcomp(white_wine_cleaned_scaled, scale = FALSE)  # Perform PCA on the scaled dataset without additional scaling.
pca_result  # Display the PCA result object.
names(pca_result)  # List the components of the PCA result.
pca_result$center  # Display the mean value for each variable.
pca_result$rotation  # Display the matrix of variable loadings (rotation).
pca_result$rotation <- -pca_result$rotation  # Invert rotation matrix for interpretation.
pca_result$rotation  # Display the updated rotation matrix.
pca_result$x <- -pca_result$x  # Invert the PCA scores for consistent interpretation.
head(pca_result$x)  # Display the first few rows of the PCA scores.
pca_result$sdev  # Display the standard deviations of the principal components.
(VE <- pca_result$sdev^2)  # Compute variance explained by each principal component.
PVE <- VE / sum(VE)  # Calculate proportion of variance explained by each component.
round(PVE, 3)  # Display rounded proportion of variance explained.
varPercent <- PVE*100  # Convert proportion of variance to percentage.
barplot(varPercent, xlab='PC', ylab='Percent Variance',  # Create a barplot of variance explained.
        names.arg=1:length(varPercent), las=1, ylim=c(0,max(varPercent)), 
        col='gray')
abline(h=1/ncol(pca_result$x)*100, col='red')  # Add a horizontal line at the average percentage of variance.
pca_wines = as.data.frame(pca_result$x[,1:7])  # Create a dataframe of the first 7 principal components.
pca_wines
library(factoextra)
fviz_nbclust(pca_wines, kmeans, method = 'wss')  # Visualize the within-sum-of-squares criterion to help choose the number of clusters.
fviz_nbclust(pca_wines, kmeans, method = 'silhouette')  # Visualize the silhouette method for assessing clustering.
fviz_nbclust(pca_wines, kmeans, method = 'gap_stat')  # Visualize the gap statistic for determining the optimal number of clusters.
k = 2  # Specify number of clusters for k-means.
kmeans_wines = kmeans(pca_wines, centers = k, nstart = 15)  # Perform k-means clustering.
fviz_cluster(kmeans_wines, data = pca_wines)  # Visualize the clusters formed.
fit.km <- kmeans(pca_wines, 2)  # Re-fit k-means clustering.
fit.km  # Display k-means fitting results.
wss = fit.km$tot.withinss  # Total within-cluster sum of square.
bss = fit.km$betweenss  # Between-cluster sum of square.
wss  # Display within-cluster sum of square.
bss  # Display between-cluster sum of square.
bss_to_tss_ratio = bss / (wss + bss) # Display between-cluster sum of square to total sum of square ratio
bss_to_tss_ratio
plotcluster(pca_wines, fit.km$cluster)  # Plot the clusters based on PCA scores.
sil <- silhouette(fit.km$cluster, dist(pca_wines))  # Calculate silhouette width for each cluster.
fviz_silhouette(sil)  # Visualize the silhouette analysis results.

# Calculate the dissimilarity matrix based on Euclidean distance
diss <- dist(pca_wines)  # PCA-transformed dataset

range_k <- 2:10  # Define a range of cluster numbers to evaluate
calinski_values <- numeric(length(range_k))  # Initialize a vector to store CH indices

for (k in range_k) {
  set.seed(123)  # for reproducibility
  kmeans_result <- kmeans(pca_wines, centers = k, nstart = 25)
  diss <- dist(pca_wines)  # Assuming pca_wines is your PCA-transformed dataset
  calinski_values[k - 1] <- cluster.stats(diss, kmeans_result$cluster)$ch
}

# Create a dataframe for plotting
calinski_df <- data.frame(k = range_k, Calinski_Index = calinski_values)

ggplot(calinski_df, aes(x = k, y = Calinski_Index)) +
  geom_line(group = 1, colour = "blue") +  # Line plot
  geom_point(aes(size = 3), colour = "red") +  # Points on the line
  labs(title = "Calinski-Harabasz Index vs. Number of Clusters",
       x = "Number of Clusters",
       y = "Calinski-Harabasz Index") +
  theme_minimal()  # Use a minimal theme

