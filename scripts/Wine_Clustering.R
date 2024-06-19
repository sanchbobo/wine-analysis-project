# Load necessary libraries
library(readxl)      # For reading Excel files
library(NbClust)     # For determining the optimal number of clusters
library(factoextra)  # For visualizing clusters and PCA results
library(cluster)     # For clustering algorithms
library(ggplot2)     # For creating plots

# Display the current working directory
getwd()

# Read data from the Excel file
white_wine <- read_excel("/Users/sanch_bo/Desktop/ML_CW_w1936916/Whitewine_v6.xlsx")

# Define a function to calculate the extremeness of outliers in a column based on the IQR method
calculate_extremeness <- function(column) {
  Q1 <- quantile(column, 0.25)   # First quartile (25%)
  Q3 <- quantile(column, 0.75)   # Third quartile (75%)
  IQR <- Q3 - Q1                 # Interquartile range
  lower_bound <- Q1 - 1.5 * IQR  # Lower bound for outliers
  upper_bound <- Q3 + 1.5 * IQR  # Upper bound for outliers
  
  # Calculate how far values are outside the IQR bounds
  extremeness <- ifelse(column < lower_bound, lower_bound - column, ifelse(column > upper_bound, column - upper_bound, 0))
  return(extremeness)
}

# Apply outlier detection to each column (except the last) and store the results
extremeness_scores <- as.data.frame(lapply(white_wine[-ncol(white_wine)], calculate_extremeness))

# Aggregate the outlier scores across all columns for each row
row_extremeness <- rowSums(extremeness_scores)

# Identify the top 90 rows with highest outlier scores
top_90_outliers <- order(row_extremeness, decreasing = TRUE)[1:90]

# Remove identified outlier rows from the dataset
white_wine_cleaned <- white_wine[-top_90_outliers, ]

# Scale the cleaned data, excluding the last column
white_wine_cleaned_data <- scale(white_wine_cleaned[-ncol(white_wine_cleaned)])

# Convert the scaled data from a matrix to a data frame and reattach column names
white_wine_cleaned_scaled <- as.data.frame(white_wine_cleaned_data)
colnames(white_wine_cleaned_scaled) <- colnames(white_wine_cleaned[-ncol(white_wine_cleaned)])


# Display a snapshot and structure of the cleaned and scaled dataset
head(white_wine_cleaned_scaled)
str(white_wine_cleaned_scaled)

# Display summary statistics of the cleaned and scaled dataset
summary(white_wine_cleaned_scaled)

# Set a random seed for reproducibility in clustering
set.seed(1234)

# Determine the optimal number of clusters using various methods
fviz_nbclust(white_wine_cleaned_scaled, kmeans, method="wss")
fviz_nbclust(white_wine_cleaned_scaled, kmeans, method="silhouette")
fviz_nbclust(white_wine_cleaned_scaled, kmeans, method="gap_stat")
nc <- NbClust(white_wine_cleaned_scaled, min.nc=2, max.nc=10, method="kmeans")

# Perform k-means clustering with k = 2
fit.km <- kmeans(white_wine_cleaned_scaled, 2)

# Store and display within-cluster sum of squares and between-cluster sum of squares
wss = fit.km$tot.withinss
bss = fit.km$betweenss
wss
bss

# Calculate and display the ratio of between-cluster to total sum of squares
bss_over_tss_ratio = (bss / (wss + bss))
bss_over_tss_ratio

# Display cluster centers and sizes
fit.km$centers
fit.km$size

# Load additional library for cluster plotting and silhouette calculation
library(fpc)

# Plot clusters and calculate silhouette scores for visual evaluation of cluster quality
plotcluster(white_wine_cleaned_scaled, fit.km$cluster)
sil <- silhouette(fit.km$cluster, dist(white_wine_cleaned_scaled))
fviz_silhouette(sil)
