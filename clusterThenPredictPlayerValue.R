### Fifa: Perform Clustering to Predict Player Valuation
## Import the libraries you might need:
library(readr)
library(readxl)
library(data.table)
library(dummies)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(mclust)
library(cluster)
library(dendextend)
library(plyr)

## Import
setwd('~/Codestorage/fifa')
fifa = read.csv('fifaCleaned', header=TRUE , sep=',', na.strings=c("","NA"));

## Sanity checks
dim(fifa) # 18207 x 91
str(fifa)
head(fifa)
colnames(fifa)
summary(fifa)

## Split data
library(caret)
set.seed(100)
split = createDataPartition(fifa$value,p = 0.7,list = F)
train = fifa[split,]
test = fifa[-split,]

dim(train) # 12746 x 91
dim(test) # 5461 x 91

## Data Cleaning
# Rm irrelevant vars
str(train)
train = dplyr::select(train, -c(x, name, firstName, lastName, photo, flag, clubLogo, joined, nationality, club,
                                              preferredFoot, offensiveWorkRate, defensiveWorkRate, bodyType, realFace, position))
test = dplyr::select(test, -c(x, name, firstName, lastName, photo, flag, clubLogo, joined, nationality, club,
                                            preferredFoot, offensiveWorkRate, defensiveWorkRate, bodyType, realFace, position))

# Omit rows with NA from cluster data 
train = na.omit(train)
test = na.omit(test)

dim(train) # 11162 x 75
dim(test) # 4764 x 75

### Build 2 basic predictive models: linear regression, random forest
## Linear Regression model
linear = lm(value~.,train)
predLinear = predict(linear,newdata=test) # Note: Long run-time alert
rmseLinear = sqrt(mean((predLinear-test$value)^2)); rmseLinear
summary(linear)

## Random Forest model
set.seed(100)
forest = randomForest(value~., data=train, ntree = 500) # Note: if this step takes a while, run `ntree=2` as a sample
predForest = predict(forest,newdata=test)
rmseForest = sqrt(mean((predForest-test$value)^2)); rmseForest
names(forest)
summary(forest)
plot(forest)
varImpPlot(forest); importance(forest)

### Cluster then predict
# For clustering, remove the dependent variable
trainMinusDV = subset(train,select=-c(value))
testMinusDV = subset(test,select=-c(value))

# Normalise the data as cluster analysis can be sensitive to scale
preproc = preProcess(trainMinusDV)
trainNorm = predict(preproc,trainMinusDV)
testNorm = predict(preproc,testMinusDV)
mean(trainNorm$slidingTackle)
mean(testNorm$slidingTackle)

str(trainNorm)

## Hierarchical clustering
d = dist(trainNorm,method = 'euclidean') # Note: Takes a min
clusters = hclust(d = d,method = 'ward.D2')
plot(color_branches(as.dendrogram(clusters),k = 2,groupLabels = F)) # Note: Takes a few mins
clusterGroups = cutree(clusters,k=2)

## Check cophenetic correlation
cor(cophenetic(clusters),d)

## K-means clustering
set.seed(100)
km = kmeans(x=trainNorm,centers = 2,iter.max=10000,nstart=100)
mean(km$cluster==clusterGroups) # % match between hclust and kmeans

## Run "total sums of squares plot" to see optimal number of clusters
within_ss = sapply(1:10,FUN = function(x) kmeans(x = trainNorm,centers = x,iter.max = 100,nstart = 25)$tot.withinss) # Note: Takes a few mins
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

## Run "ratio plot" to see optimal number of clusters
ratio_ss = sapply(1:10,FUN = function(x) {km = kmeans(x = trainNorm,centers = x,iter.max = 10,nstart = 2)
km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

## Based on the above, 2 or 3 cluster solution might be best. Let's visualise both
plot(color_branches(as.dendrogram(clusters),k = 2,groupLabels = F))
plot(color_branches(as.dendrogram(clusters),k = 3,groupLabels = F))

## Apply clustering from train to test
library(flexclust)
km_kcca = as.kcca(km,trainNorm)
clusterTrain = predict(km_kcca)
clusterTest = predict(km_kcca,newdata=testNorm)

## Check distributions across clusters
table(clusterTrain)
table(clusterTest)

## Re-split train and test based on cluster solution
train1 = subset(train,clusterTrain==1)
train2 = subset(train,clusterTrain==2)
test1 = subset(test,clusterTest==1)
test2 = subset(test,clusterTest==2)

dim(train1) # 5114 x 75
dim(train2) # 6048 x 75
dim(test1) # 2174 x 75
dim(test2) # 2590 x 75

str(train1)

## Predict for each cluster, then combine
# Linear Regression model
lm1 = lm(value~.,train1)
lm2 = lm(value~.,train2)
pred1 = predict(lm1,newdata=test1)
pred2 = predict(lm2,newdata=test2)
rmse1 = sqrt(mean((pred1-test$value)^2)); rmse1
rmse2 = sqrt(mean((pred2-test$value)^2)); rmse2
predOverall = c(pred1,pred2)
qualityOverall = c(test1$value,test2$value)
sseOverall = sum((predOverall - qualityOverall)^2); sseOverall
rmseOverall = sqrt(mean((predOverall-test$value)^2)); rmseOverall

# Random Forest model
forest1 = randomForest(value~., data=train1, ntree = 500)
forest2 = randomForest(value~., data=train2, ntree = 500)
pred1 = predict(forest1,newdata=test1)
pred2 = predict(forest2,newdata=test1)
rmse1 = sqrt(mean((pred1-test$value)^2)); rmse1
rmse2 = sqrt(mean((pred2-test$value)^2)); rmse2
predOverall = c(pred1,pred2)
rmseOverall = sqrt(mean((predOverall-test$value)^2)); rmseOverall


## Plot feature importance charts
library(ggRandomForests)
plot(gg_vimp(forest1, nvar=10)) + 
  labs( x='Feature', y= 'Relative Feature Importance', title='Forest Model 1')
plot(gg_vimp(forest2, nvar=10)) + 
  labs( x='Feature', y= 'Relative Feature Importance', title='Forest Model 2')
