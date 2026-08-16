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
plot(cumsum(pca$sdev^2)/sum(pca$sdev),ylab = '% Variance Explained', xlab='Number of Components')
#####################################################
###############  Explore Principal Components
#####################################################
samplePoints = sample(1:45222, 8000, replace=F)
plot(pca$x[samplePoints,1:2], col=adult$k6v1[samplePoints] )
par(mfrow=c(3,3),mar=c(4,4,1,1))
plot(pca$x[samplePoints,1:3])
plot(pca$x[samplePoints,1:4])
plot(pca$x[samplePoints,1:5])
plot(pca$x[samplePoints,2:3])
plot(pca$x[samplePoints,2:4])
plot(pca$x[samplePoints,2:5])
plot(pca$x[samplePoints,3:4])
plot(pca$x[samplePoints,3:5])
plot(pca$x[samplePoints,4:5])


#####################################################
###############  Cluster Principal Components
#####################################################
k6v1 = kmeans(pca$x[,1:4],6)

#####################################################
###############  Visualize Clusters via PCA 
#####################################################

adult$k6v1 = k6v1$cluster
plot(pca$x[samplePoints,1:2], col=adult$k6v1[samplePoints] )

#####################################################
###############  Profile and Interpret Clusters
#####################################################
varsToProfile = c("age" , "workclass","education","maritalStatus","occupation","relationship","race","sex",
                  "capitalGain","capitalLoss","hoursPerWeek","nativeCountry","incomeLevel")
par(mfrow=c(3,3))
clusterProfile(df=adult, clusterVar ='k6v1',varsToProfile)
#####################################################
###############  Examine skewed variables more closely
#####################################################

adult2 = adult[!(adult$capitalLoss==0 & adult$capitalGain==0),]

varsToProfile = c("capitalGain","capitalLoss")

clusterProfile(df=adult2, clusterVar ='k6v1',varsToProfile)

adult$zeroGainAndLoss=0
adult$zeroGainAndLoss[(adult$capitalLoss==0 & adult$capitalGain==0)]=1
adult$zeroGainAndLoss = factor(adult$zeroGainAndLoss)
par(mfrow=c(1,1))
clusterProfile(df=adult, clusterVar ='k6v1','zeroGainAndLoss')

#####################################################
###############  Cluster Profile Function
#####################################################

clusterProfile = function(df, clusterVar, varsToProfile){
  k = max(df[,clusterVar])
  for(j in varsToProfile){
    if(is.numeric(df[,j])){
    for(i in 1:k){
      hist(as.numeric(df[df[,clusterVar]==i ,j ]), breaks=50, freq=F, col=rgb(1,0,0,0.5),
       xlab=paste(j), ylab="Density", main=paste("Cluster",i, 'vs all data, variable:',j))
      hist(as.numeric(df[,j ]), breaks=50,freq=F, col=rgb(0,0,1,0.5), xlab="", ylab="Density", add=T)
  
      legend("topright", bty='n',legend=c(paste("cluster",i),'all observations'), col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), pt.cex=2, pch=15 )}
      }
    if(is.factor(df[,j])&length(levels(df[,j]))<5){
      (counts = table( df[,j],df[,clusterVar]))
      (counts = counts%*%diag(colSums(counts)^(-1)))
      barplot(counts, main=paste(j, 'by cluster'),
              xlab=paste(j),legend = rownames(counts), beside=TRUE)
    }
  }
}
