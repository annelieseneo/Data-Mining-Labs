# Importing the dataset
setwd('C:/Users/user/Downloads/ITS61504 Data Mining/excel datasets')
# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5] #reduce dimentionality by only using column 4 and 5
print(dataset)


# Using the elbow method to find the optimal number k of clusters
set.seed(6)
wcss = vector() 
print(wcss)
# produces an empty vector, to be puopulated using loop later on
# a measure to check quality of clusters using sum of squared distance, then
# go for the optimal set with	High similarity using distance function




# separate points and lines (represent clusters)
#	Output graph of elbow method to see reduction in sum of squares value
#	find Balance between overfit with too small similarities, and too large 
# differences with low accuracy
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b', #draw both points and lines plot
     main = paste('The Elbow Method'), #title of plot
     xlab = 'Number of clusters', #x axis label
     ylab = 'WCSS') #y axis label
#choose 5 clusters since distance between 5 and 6 is not significant




set.seed(29)
kmeans = kmeans(x = dataset, centers = 5) #5 clusters centers/centroid
print(kmeans)
# output cluster sizes, cluster means, clustering vector, within cluster 
# sum of squares, and available component names


# Fitting K-Means to the dataset using 5 clusters vectors
y_kmeans = kmeans$cluster
print(y_kmeans)


# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0, #no distance lines will appear
         shade = TRUE, 
            #ellipses are shaded in relation to their density 
            #(number of points in the cluster divided by the area of the ellipse)
         color = TRUE, #different colours for ellipses with respect to their density
         labels = 2, #all points and ellipses are labelled
         plotchar = FALSE, #plotting symbols are the same for all points in different clusters
         span = TRUE, 
            #each cluster is represented by the ellipse with 
            #smallest area containing all its points
         main = paste('Clusters of customers'), #title of plot
         xlab = 'Annual Income', #x axis label
         ylab = 'Spending Score') #y axis label