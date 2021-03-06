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
#print(test_set[-3])
# Fitting SVM to the Training set
# install.packages('e1071') 
library(e1071)

#create naive bayes classifier model using training set
classifier = naiveBayes(x = training_set[-3],
                        y = training_set$Purchased)
print(classifier) #apriori and conditional probabilities


# Predicting the results of dependent variable (y) using Test set,
#to obtain model accuracy later
y_pred = predict(classifier, newdata = test_set[-3])
print(y_pred)



# create Confusion Matrix based on col 3 = Purchased
#compare actual value (test set) vs y_pred
cm = table(test_set[, 3], y_pred)
print(cm)
#7 + 7 not correct, out of 100 = 86% accuracy


# Visualising Training set results
library(ElemStatLearn)
set = training_set
#data seq of column 1 with 0.01 interval
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
print(X1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
print(X2)



grid_set = expand.grid(X1, X2) 
#create vector consisting of age & est salary
colnames(grid_set) = c('Age', 'EstimatedSalary')
print(grid_set)



y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)', #graph title
     xlab = 'Age', ylab = 'Estimated Salary', #x and y axis labels
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
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))