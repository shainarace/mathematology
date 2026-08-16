#' Age
#' Workclass -  Federal-gov, local-gov, never-worked, self-emp-inc, self-emp-not-inc, 
#'              private, without-pay
#' Fnlwgt -  Final sampling weight. Two implied decimals in this variable. Inverse 
#'              of sampling fraction adjusted for non-response and over or under sampling
#'              particular groups. Not an input variable, but a weight variable.
#' Education -  Level of education
#' EducationNumeric - Numeric version of education
#' MaritalStatus
#' Occupation - Category of occupation types
#' Relationship - Relationship to head of household (now called "householder") which is defined
#'              as the person (or one of the persons) in whose name the house/apt is owned or 
#'              rented/maintained.
#' Race
#' Sex
#' capitalGain 
#' capitalLoss 
#' hoursPerWeek - Average hours worked per week over past year
#' nativeCountry
#' incomeLevel - Binary value, either <=50K or >50k.
#' 
library(psych) #pairs.panels
library(ggplot2) #pairs.panels
library(dplyr) 
library(tidyr) #gather
load("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Datasets and Code/UCI Adult data/adult.RData")


#####################################################
###############  Explore your data
#####################################################
# num.vars = c("age","educationNumeric","hoursPerWeek","capitalGain","capitalLoss")
# par(mfrow=c(1,1))
# pairs.panels(adult[,num.vars],, method = 'pearson', density=T,ellipses = T)
#####################################################
###############  Transform your data
#####################################################
adult$capitalGain = log(adult$capitalGain+1)
adult$capitalLoss = log(adult$capitalLoss+1)
# hist(scale(adult$capitalGain))
# par(mfrow=c(1,1))
# pairs.panels(adult[,num.vars],, method = 'pearson', density=T,ellipses = T)
#####################################################
###############  Explore your data
#####################################################
# classVars = c( "workclass","education","maritalStatus","occupation","relationship","race","sex",
#                   "nativeCountry","incomeLevel")
# par(mfrow=c(3,3),las=2,mar=c(5,8,4,2))
# 
# for(var in classVars){
# counts <- table(adult[,var])
# barplot(counts, main=paste(var), horiz=T,cex.names=0.75, col = 'violet')
# }
#####################################################
###############  Transform your data
#####################################################
adult$nativeCountry2=as.character(adult$nativeCountry)
adult$nativeCountry2[adult$nativeCountry2!=' United-States'] = 'notUS'
adult$nativeCountry2[adult$nativeCountry2==' United-States'] = 'US'
adult$nativeCountry=factor(as.character(adult$nativeCountry2))
adult$nativeCountry2=NULL
counts <- table(adult[,'nativeCountry'])
barplot(counts, main='nativeCountry', horiz=T,cex.names=0.75, col = 'violet')
#####################################################
###############  Dummy Code the factors
#####################################################
adult.x = model.matrix(~. ,contrasts.arg = lapply(adult[,sapply(adult,is.factor) ], 
                                                  contrasts, contrasts=FALSE),
                       data = adult)
dim(adult.x)
# Get rid of constant columns
adult.x = adult.x[,apply(adult.x, 2, sd)>0 ]
dim(adult.x)
#####################################################
###############  Go to Principal Components
#####################################################
pca = prcomp(adult.x, scale = T)
#Screeplot
par(mfrow=c(1,1))
plot(pca$sdev^2)
#Cumulative % Variance Explained
plot(cumsum(pca$sdev^2)/sum(pca$sdev^2),ylab = '% Variance Explained', xlab='Number of Components')
#####################################################
###############  Explore Principal Components
#####################################################
samplePoints = sample(1:45222, 8000, replace=F)
plot(pca$x[samplePoints,1:2], col=adult$k6v1[samplePoints] )
par(mfrow=c(3,3),mar=c(4,4,1,1))
plot(pca$x[samplePoints,c(1,3)])
plot(pca$x[samplePoints,c(1,4)])
plot(pca$x[samplePoints,c(1,5)])
plot(pca$x[samplePoints,c(2,3)])
plot(pca$x[samplePoints,c(2,4)])
plot(pca$x[samplePoints,c(2,5)])
plot(pca$x[samplePoints,c(3,4)])
plot(pca$x[samplePoints,c(3,5)])
plot(pca$x[samplePoints,c(4,5)])

#####################################################
#####################################################
###############  Explore Number of Clusters 
###############  via k-means objective function
#####################################################
#####################################################
obj = vector()
for(k in 2:20){
  obj[(k-1)] = kmeans(pca$x[,1:4],k)$tot.withinss
}

# Single Clustering view of SSE diagram
plot(obj, xlab = 'Number of Clusters k', ylab = 'Objective Function',
     pch=16, type='b', col='violet')
#####################################################
#####################################################
###############  Explore Number of Clusters 
###############  via k-means objective function
###############  (Again, this time with multiple 
###############   multiple runs of kmeans)
#####################################################
#####################################################
obj = matrix(NA, nrow=10, ncol = 19)
for(k in 2:20){
  iter=1
  while(iter<11){
    obj[iter,(k-1)] = kmeans(pca$x[,1:4],k)$tot.withinss
    iter=iter+1
  }
}
colnames(obj) = paste('k',2:20,sep='')
rownames(obj) = paste('iter',1:10,sep='')

# Use output to create data frame for boxplot visual 
# To see what this is doing, observe the output obj and 
# the transformed output in obj2
obj = data.frame(obj)
obj2 = gather(obj,key = 'K',value = 'SSE', 
              k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15,k16,k17,k18,k19,k20  )
obj2$K = gsub("k",'',obj2$K)
obj2$K = as.numeric(obj2$K)

# Create the boxplot visual
par(mfrow=c(1,1))
boxplot(SSE~K,data=obj2, ylab = 'SSE Objective Function', 
        xlab='Number of clusters, k', col='violet',    
        main = 'Box plots of SSE for 10 runs of k-means with each k')

##################################################################
##################################################################
##################################################################
###############  USE HIERARCHICAL CLUSTERING ON MANY
###############  CENTROIDS FROM A K-MEANS OUTPUT
##################################################################
##################################################################
##################################################################

#####################################################
###############  Cluster Principal Components with 
###############  large number of clusters
#####################################################
# Single Clustering k=20 clusters:
set.seed(11117)
k6v1 = kmeans(pca$x[,1:4],20)
#####################################################
###############  Visualize Clusters via PCA 
#####################################################
adult$k6v1 = k6v1$cluster

samplePoints = sample(1:45222, 8000, replace=F)
plot(pca$x[samplePoints,1:2], col=adult$k6v1[samplePoints] )
hc = hclust(dist(k6v1$centers))
par(mfrow=c(1,1))
plot(hc)
c1 = c(2,15,4,14,16)
c2 = c(11,18,3,5)
c3 = c(7,8,17)
c4 = c(1,10,13,19,20,6,9,12)
adult$hc.clust[adult$k6v1 %in% c1]=1
adult$hc.clust[adult$k6v1 %in% c2]=2
adult$hc.clust[adult$k6v1 %in% c3]=3
adult$hc.clust[adult$k6v1 %in% c4]=4
plot(pca$x[samplePoints,1:2], col=adult$hc.clust[samplePoints] )


