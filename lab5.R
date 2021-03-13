# Importing the dataset
setwd('C:/Users/user/Downloads/ITS61504 Data Mining/excel datasets')
# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5] #reduce dimentionality by only using column 4 and 5
print(dataset)

# plot Dendrogram find the optimal number of clusters  using euclidean distance
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'), #title of plot
     xlab = 'Customers', #x axis label
     ylab = 'Euclidean distances') #y axis label

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5) #5 clusters
print(y_hc)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,#no distance lines will appear
         shade = TRUE, 
         #ellipses are shaded in relation to their density 
         #(number of points in the cluster divided by the area of the ellipse)
         color = TRUE, #different colours for ellipses with respect to their density
         labels = 2, #all points and ellipses are labeled
         plotchar = FALSE, #plotting symbols are the same for all points in different clusters
         span = TRUE, 
            #each cluster is represented by the ellipse with 
            #smallest area containing all its points
         main = paste('Clusters of customers'), #title of plot
         xlab = 'Annual Income', #x axis label
         ylab = 'Spending Score') #y axis label
