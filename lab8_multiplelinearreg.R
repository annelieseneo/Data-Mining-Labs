#install.packages("tidyverse")
library(tidyverse) # data manipulation and visualisation
#install.packages("datarium")
library(datarium)
#install.packages("devtools")
library(devtools)

# marketing data set [datarium package] contains the sales impact of 
  # the amount of money spent on ad medias (youtube, facebook, and newspaper)
# goal is to estimate sales based on ad media budget
#devtools::install_github("kassambara/datarium")
  
# load and inspect data
data("marketing", package = "datarium")
head(marketing, 4)

# computer model coefficients
model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)
# interpret analysis by examining the F-statistic and the associated p-value
  # smaller p-value means highly significant
    # at least 1 predictor variable is significantly related to the outcome

# examine the coefficients table
  # estimate regression beta coefficients and the associated t-statistic p-values
summary(model)$coefficient
# t-statistic evaluates presence of significant association between the predictor and the outcome 
  # which beta coefficients of predictors is significantly different from zero

# remove newspaper variable since not significant
model <- lm(sales ~ youtube + facebook, data = marketing)
summary(model)

# confidence interval of the model coefficient 
confint(model)

# estimate error rate dividing RSE by the mean outcome variable
# RSE is sigma
sigma(model)/mean(marketing$sales)
