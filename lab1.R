# Importing the dataset
setwd('C:/Users/user/Downloads/ITS61504 Data Mining/rstudio codes')
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5] #include columns 3 to 5
print(dataset$Purchased)
# Encoding the target feature as factor (binary)
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
print(dataset$Purchased)


# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123) #list of random numbers starting from position 123
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE) #75% of dataset used as training subset
print(training_set)
test_set = subset(dataset, split == FALSE) #25% of dataset used as training subset
print(test_set)

# Feature Scaling : normalise or standardise
#scale column 3 & 4, results are normalised between -3 & 3
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])
print(training_set[-3])
print(test_set[-3])

# Fitting Decision Tree Classification to the Training set
library(rpart)
#Specify model formula (purchased is identified as the class) & model data
classifier = rpart(formula = Purchased ~ ., data = training_set)
print(classifier)

# Predicting the Test set results, return prediction for each class
y_pred = predict(classifier, newdata = test_set[-3], type = 'class')
print(y_pred)

# Making the Confusion Matrix, compare ori (test set) vs prediction (y_pred)
cm = table(test_set[, 3], y_pred)
print(cm)
#11 + 6 not correct, out of 100 = 83% accuracy

# Visualising the Training set results
library(ElemStatLearn)






set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) #data seq of column 1 with 0.01 interval
print(X1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
print(X2)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
print(grid_set)

y_grid = predict(classifier, newdata = grid_set, type = 'class')
print(y_grid)
plot(set[, -3],
     main = 'Decision Tree Classification (Training set)', #graph title
     xlab = 'Age', ylab = 'Estimated Salary', #x & y axis labels
     xlim = range(X1), ylim = range(X2)) #range of plotted values
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
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree Classification (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Plotting the tree
plot(classifier) #plot decision tree
text(classifier) #add text inside plotting area
