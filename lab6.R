#see lab10 pdf

#install.packages("mvoutlier")
#install.packages("Rfunspaceadventure")
library('mvoutlier') #methods of anomaly detection
library('Rfunspaceadventure')
str(humus)

x <- log(humus[ , c("Al","Co","Cu","Ni", "Pb","U")])
distances <- dd.plot(x, quan=1/2, alpha=0.025)

# The classical and Robust distances now exist as vectors
str(distances$md.cla)
## Named num [1:617] 1.86 2.6 1.78 2.66 3.03 ...
## - attr(*, "names")= chr [1:617] "1" "2" "3" "4" ...

str(distances$md.rob)
## Named num [1:617] 2.28 2.64 1.94 2.99 3.83 ...
## - attr(*, "names")= chr [1:617] "1" "2" "3" "4" ...

x <- log(humus[,c("U","Pb")])
colorData <- color.plot(x, quan=1/2, alpha=0.025)

str(colorData$md)
## Named num [1:617] 1.508 0.908 1.226 0.944 0.896 ...
## - attr(*, "names")= chr [1:617] "1" "2" "3" "4" ...

str(colorData$euclidean)
## Named num [1:617] 5.12 4.25 3.83 4.76 4.34 ...
## - attr(*, "names")= chr [1:617] "1" "2" "3" "4" ...

str(colorData$outliers)
## Named logi [1:617] FALSE FALSE FALSE FALSE FALSE FALSE ...
## - attr(*, "names")= chr [1:617] "1" "2" "3" "4" ...

# Generate the data frame with sleected features
x <- log(humus[ , c("Al","Co","Cu","Ni", "Pb","U")])
# Execute
outliers <- aq.plot(x, delta=qchisq(0.975, df = ncol(x)),
                    quan = 1/2, alpha = 0.05)
## Projection to the first and second robust principal components.
## Proportion of total variation (explained variance): 0.6449667

# Inspect outliers
str(outliers)
## List of 1
## $ outliers: Named logi [1:617] FALSE FALSE FALSE FALSE FALSE FALSE ...
## ..- attr(*, "names")= chr [1:617] "1" "2" "3" "4" ...

x <- log(humus[ , c("Al","Co","Cu","Ni", "Pb","U")])
par(mfrow = c(1,1))
chisq.plot(x, quan = 0.5, ask = TRUE)

## Remove outliers with left-click, stop with right-click on plotting device
## $outliers
## NULL

res <- chisq.plot(log(humus[ , c("Al","Co","Cu","Ni", "Pb","U")]), quan = 0.5, ask = TRUE)
res$outliers
# [1] 75 28 67 35 87 613 449 173 116

# kola background is a map from which humus samples were taken
kola <- rbind(kola.background$coast,kola.background$boundary,
              kola.background$bordes)
# XY coordinates within humus dataset
XY <- humus[,c("XCOO","YCOO")]
# The mvoutlier.CoDa function returns all pertinent information needed for further analysis
data <- mvoutlier.CoDa(humus[ , c("Al","Cu", "Ni", "Co")])
# execute:
plot(data,coord=XY,map=kola,onlyout=TRUE,which="map",bw=FALSE,
     symb=TRUE,symbtxt=TRUE)

plot(data,onlyout=TRUE,which="biplot",bw=FALSE,
     symb=TRUE,symbtxt=TRUE)
