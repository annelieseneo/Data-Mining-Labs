library(hwriter)
# Importing the dataset
setwd('C:/Users/user/Downloads/ITS61504 Data Mining/excel datasets')
dataset = read.csv('Salary_Data.csv')
head(dataset)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set
test_set


# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)
# equation of line of best fit
regressor

# residuals, coefficients, residual standard error, R-squared
summary(regressor)
# model is significant
# R-Squared value of training data is comparative to the original model of full data

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred

# Visualising the Training set results
library(ggplot2)
ggplot() + #plot the following layers
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') + #colour of points
  geom_line(aes(x = training_set$YearsExperience, 
                y = predict(regressor, newdata = training_set)),
            colour = 'blue') + #colour of line of best fit
  ggtitle('Salary vs Experience (Training set)') + # title of plot
  xlab('Years of experience') + #x axis label
  ylab('Salary') #y axis label

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, 
                y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

# make actuals_predicteds dataframe
actuals_preds <- data.frame(cbind(actuals=test_set$Salary, predicteds=y_pred))  
head(actuals_preds)

# correlation between actuals values and predicted values
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy # 97.1292%, almost identical directional movement

# average between the min and the max prediction as closeness metric to measure accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy #92.8008%

#mean absolute percentage deviation as an error rate
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape #7.4640%