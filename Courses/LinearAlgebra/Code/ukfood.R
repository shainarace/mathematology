
food=read.csv("http://www4.ncsu.edu/~slrace/LinearAlgebra2018/Code/ukfood.csv", header=TRUE,row.names=1)
#'  In the table is the average consumption of 17 types of food in grams per 
#' person per week for every country in the UK.
#'  The table shows some interesting variations across different food types, 
#' but overall differences aren't so notable. Let's see if PCA can eliminate 
#' dimensions to emphasize how countries differ.

food=as.data.frame(t(food))
head(food)
food=data.frame(food)

#' princomp function can only be used with more observations than variables. 
#' prcomp can be used with any dataset. To use Covariance PCA, option is scale=F
#' (That is the default, but if you specify you don't have to remember)

pca=prcomp(food, scale=F)

# first plot just looks at magnitudes of eigenvalues
plot(pca)

# next plot views our four datapoints (locations) projected onto the 2-dimensional subspace
# that captures as much information (i.e. variance) as possible
plot(pca$x, col="lightblue", pch=16, cex=3, xlim=c(-350,550))
text(pca$x[,1], pca$x[,2],row.names(food))

# examine the loadings to see what variables are important to the separations
# shown in the plot:
pca$rotation
# Let's hide anything that's below a threshold so we can see large numbers
# more easily:
look=pca$rotation
look[abs(look)<0.3]=NA
# What can we conclude?


# All of the above information (and more) is summarized visually in a biplot.
# Here, we view our original variable axes projected down onto that same space!
# a visual you can relate this to: Take a plane (piece of poster board) running at an angle
# through the origin in 3 space. Think of the unit axis vectors being projected orthogonally
# onto this poster board... The closer the plane comes to that axis, the longer that projection will be.
# Long projections means that those principal components run close to the original variable -
# they are highly correlated. Shorter projections indicate less correlation with PCs. Less correlation
# with major PCs may simply mean there isn't much variance along those variables - In covariance PCA,
# it is reasonable that the variables with the most variance are likely to dominate the first components.

  
biplot(pca)
