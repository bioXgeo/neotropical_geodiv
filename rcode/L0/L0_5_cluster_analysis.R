#Clustering by trait

#read in habitat data

habitat_data <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/supplemental_information/habitat_all_species.csv")

library(dplyr)
library(tidyr)

# create a new dataframe with binary-encoded habitat types
df_binary_spec <- habitat_data %>%
  distinct(species, habitat) %>%
  pivot_wider(names_from = habitat, values_from = habitat, values_fn = length, values_fill = 0)

df_binary <- df_binary_spec
df_binary$species <- NULL

#elbow test for optimal clusters
library(tidyr)
df_binary$species <-NULL
# Compute the SSE for different values of k
sse <- c()
for (k in 1:10) {
  kmeans_obj <- kmeans(df_binary, k)
  sse[k] <- kmeans_obj$tot.withinss
}

# Plot the SSE curve
plot(1:10, sse, type = "b", xlab = "Number of clusters (k)", ylab = "Sum of Squared Errors (SSE)")

# Add a vertical line at the elbow point
abline(v = 3, lty = 2)

# Jaccard distance
habitat_dist <- dist(df_binary, method="binary")
set.seed(123) # for reproducibility
kmeans_result <- kmeans(habitat_dist, centers=8)

df_binary_spec$cluster <- as.factor(kmeans_result$cluster)

#diet
#don't use clustering for diet/makes some groups I wouldn't agree with. Need to divide out obligate frugivores, folivores etc. >70%?
# Read in the diet percentage data

# load the data
df <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_mammal_database_2023_full.csv", header=TRUE)

df_diet <- na.omit(df[,12:21])

# calculate the sum of squared errors (SSE) for different values of k
sse <- sapply(1:10, function(k){
  kmeans(df_diet, k, nstart=10)$tot.withinss
})

# plot the SSE values against the number of clusters (k)
plot(1:10, sse, type="b", pch=19, frame=FALSE, xlab="Number of clusters (k)", ylab="Sum of squared errors (SSE)")

library(factoextra)
library(tidyr)
# add a vertical line at the "elbow" point to indicate the optimal number of clusters
fviz_nbclust(df_diet, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)


# Set the number of clusters
k <- 6

# Perform k-means clustering using the euclidean distance metric
set.seed(123) # for reproducibility
km <- kmeans(df_diet, centers=k)



# Add the cluster assignments to the original dataframe
df_diet$cluster <- km$cluster

# View the resulting clusters
table(km$cluster)

df_short <-df[,1:25]
df_short$cluster <-km$cluster

#mass
df <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_mammal_database_2023_full.csv", header=TRUE)

df_mass <- as.data.frame(df[,35])

# calculate the sum of squared errors (SSE) for different values of k
sse <- sapply(1:20, function(k){
  kmeans(df_mass, k, nstart=10)$tot.withinss
})

# plot the SSE values against the number of clusters (k)
plot(1:20, sse, type="b", pch=19, frame=FALSE, xlab="Number of clusters (k)", ylab="Sum of squared errors (SSE)")

library(factoextra)
library(tidyr)
# add a vertical line at the "elbow" point to indicate the optimal number of clusters
fviz_nbclust(df_mass, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)


# Set the number of clusters
k <- 3

# Perform k-means clustering using the euclidean distance metric
set.seed(123) # for reproducibility
km <- kmeans(df_mass, centers=k)



# Add the cluster assignments to the original dataframe
df_mass$cluster <- km$cluster

# View the resulting clusters
table(km$cluster)

df_short <-df[,1:25]
df_short$cluster <-km$cluster

