# Importing the dataset
setwd('C:/Users/user/Downloads/ITS61504 Data Mining/rstudio codes')
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5] #reduce dimentionality
print(dataset$Purchased)
# Encoding the target feature as factor with 2 levels
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
print(dataset$Purchased)



# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123) 
split = sample.split(dataset$Purchased, SplitRatio = 0.75) #decide ratio of sampling
training_set = subset(dataset, split == TRUE) #develop model using training set
test_set = subset(dataset, split == FALSE) #test model using test set



# Feature Scaling of all selected columns except the 3rd column, for results to be normalised to smaller range of -3 to 3
training_set[-3] = scale(training_set[-3]) 
test_set[-3] = scale(test_set[-3])
print(test_set[-3])




# Fitting K-NN to the Training set and Predicting the Test set results of 
# dependent variable (y) to obtain model accuracy later
library(class)
y_pred = knn(train = training_set[, -3],
             test = test_set[, -3],
             cl = training_set[, 3], #return factor of true classifications 
             k = 5, #5 neighbours considered
             prob = TRUE) #return proportion of the votes for the winning class 
                          #as attribute prob (attr(,"prob"))
print(y_pred)





# Make Confusion Matrix based on col 3 = Purchased
#compare actual value (test set) vs y_pred
cm = table(test_set[, 3], y_pred)
print(cm)
#6 + 5 not correct, out of 100 = 89% accuracy


# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)


colnames(grid_set) = c('Age', 'EstimatedSalary')







y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Training set)', #graph title
     xlab = 'Age', ylab = 'Estimated Salary',#x and y axis labels
     xlim = range(X1), ylim = range(X2)) # x and y axis range of plotted values




#add contour lines to current plot, nrow is X1 & ncolumn is X2
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

#add points of specified colours
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))

#set background
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))