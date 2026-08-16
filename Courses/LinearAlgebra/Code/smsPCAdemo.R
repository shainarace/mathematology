#' Original data consisted of 5574 Text messages, containing 7K+ unique words AFTER basic preprocessing
#' We then removed words that happened infrequently. The tm package made this easy.
#' This is feature selection. Only included words that occured at least 4 times in collection.
# That left 1833 words/features
load('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Linear Algebra 2018/Code/sms_subset.Rdata')
# Some preliminary analysis with PCA:

pca = prcomp(sms_subset) 

# Screeplot
plot(pca$sdev^2)


# Plot of the 1833 dimensional data projected onto a plane
plot(pca$x[,1:2], col=c("red","blue")[sms_raw$type], pch=10)
legend(x='topleft', c('Ham','Spam'), pch='o', col=c('red','blue'), pt.cex=1.5)

# Plot of the 1833 dimensional data projected onto 3-dimensional subspace

library(rgl)
library(car)
scatter3d(pca$x[,1],pca$x[,2],pca$x[,3], groups=sms_raw$type, surface=F)

# Let's see if we can use some principal components as input to a model.
# Let k be the number of components used:

k=3
new_data = data.frame(pca$x[,1:k])
new_data = cbind(sms_raw$type, new_data)
colnames(new_data)[1]="type"

# Make a logistic regression model:

model = glm(type ~ . , family="binomial", data=new_data)
summary(model)


pred=predict(model, new_data, type="response")

c=table(pred>0.5,new_data$type) 
c
misclass=(c[1,2]+c[2,1])/5574
misclass

#' Technical note: This is not necessarily the prescribed method for modelling this problem. 
#' It is merely an illustration of the power of dimension reduction to force related observations
#' and variables close to one another. 
#' 
#' We will revisit this dataset later in the semester. Naive Bayes Classifiers tend to be well suited
#' for this type of problem, although they can be far slower to implement with new data, which can be 
#' problematic in a fast-paced solution environment.
#' 



