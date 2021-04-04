# maintain reproducibility of the results since file does not have any headers 
# and is tab-separated
set.seed(786)
# find dataset
setwd('C:/Users/user/Downloads/ITS61504 Data Mining/excel datasets')
file_loc <- 'seeds_dataset.txt'
# import dataset into a dataframe
seeds_df <- read.csv(file_loc,sep = '\t',header = FALSE, 
                     na.strings=c("NA","NaN", " ", "?"))
seeds_df <- seeds_df[complete.cases(seeds_df), ]

# apply column names from the data description
feature_name <- c('area','perimeter','compactness','length.of.kernel',
                  'width.of.kernal','asymmetry.coefficient',
                  'length.of.kernel.groove','type.of.seed')
colnames(seeds_df) <- feature_name
# find basic information on dimensions, data types and distributions, 
# and number of missing values
str(seeds_df)
summary(seeds_df)
any(is.na(seeds_df))
# all numerical values, no missing values, labeled data
# different feature scales -> normalise

# store labels in a separate variable
seeds_label <- seeds_df$type.of.seed
# exclude true labels, use them later to check accuracy
seeds_df$type.of.seed <- NULL
str(seeds_df)

# scale all attribute values to mean 0 and SD 1
seeds_df_sc <- as.data.frame(scale(seeds_df))
summary(seeds_df_sc)

# distance matrix using euclidean distance for continuous numerical values
dist_mat <- dist(seeds_df_sc, method = 'euclidean')
# average linkage method used
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

# cut dendrogram for 3 clusters to represent 3 types of wheat
cut_avg <- cutree(hclust_avg, k = 3)
plot(hclust_avg)
# superimpose rectangular compartments to enclose each cluster
rect.hclust(hclust_avg , k = 3, border = 2:6)

# cut tree with horizontal line at 3
abline(h = 3, col = 'red')

# visualise tree with different coloured branches
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

# append cluster results back in the original dataframe
suppressPackageStartupMessages(library(dplyr))
seeds_df_cl <- mutate(seeds_df, cluster = cut_avg)
# count number of observations assigned to each cluster
count(seeds_df_cl,cluster)

# analyse and evaluate trend between 2 features to extract useful insights
# for perimeter vs area, linear relationship was found
suppressPackageStartupMessages(library(ggplot2))
ggplot(seeds_df_cl, aes(x=area, y = perimeter, 
                        color = factor(cluster))) + geom_point()

# cross check results with true labels
table(seeds_df_cl$cluster,seeds_label)