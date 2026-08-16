########################################################################################
#' Read in the data. The load() function reads in a dataset that has 20532 columns
#' and may take some time. You may want to clear your environment (or open a new RStudio 
#' window) if you have other work open.
########################################################################################
load('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Linear Algebra/Linear Algebra 2019/Code/geneCancerUCI.RData')
table(cancerlabels$Class)
# Original Source: The cancer genome atlas pan-cancer analysis project
# BRCA = Breast Invasive Carcinoma
# COAD = Colon Adenocarcinoma
# KIRC = Kidney Renal clear cell Carcinoma
# LUAD = Lung Adenocarcinoma
# PRAD = Prostate Adenocarcinoma
########################################################################################
########################################################################################
########################################################################################
#' We are going to want to plot the data points according to their different 
#' classification labels. We should pick out a nice color palette for categorical
#' attributes.
########################################################################################
library(RColorBrewer)
display.brewer.all()
palette(brewer.pal(n = 8, name = "Dark2"))
########################################################################################
########################################################################################
########################################################################################
#' The first step is typically to explore the data. Obviously we can't look at ALL the 
#' scatter plots of input variables. For the fun of it, let's look at a few of these 
#' scatter plots which we'll pick at random. First pick two column numbers at random,
#' then draw the plot, coloring by the label.
########################################################################################
randomColumns = sample(2:20532,2)
plot(cancer[,randomColumns],col = cancerlabels$Class)
########################################################################################
########################################################################################
########################################################################################
#' Now let's compute the first two principal components and examine the data
#' projected onto these axes. We can then look in 3 dimensions.
########################################################################################
#' Take out columns where variance is 0.
sds = apply(cancer[,2:20532],2,sd)
remove.ind=which(sds==0)+1
cancer[,remove.ind]=NULL
cancerlabels[,remove.ind]=NULL
pcaOut = prcomp(cancer[,2:20265],3, scale=T)
plot(pcaOut$x[,1],pcaOut$x[,2],col = cancerlabels$Class, x.lab = "Principal Component 1", y.lab = "Principal Component 2")
########################################################################################
# 3-dimensional plot. Problem is to get the plot points colored by group, we need to 
# write a function that creates a vector of colors 
colors = factor(palette())
colors = colors[cancerlabels$Class]
#' make sure the rgl package is installed for the 3d plot.
library(rgl)
plot3d(x = pcaOut$x[,1], y = pcaOut$x[,2],z= pcaOut$x[,3],col = colors, xlab = "Principal Component 1", ylab = "Principal Component 2", zlab = "Principal Component 3")
########################################################################################
########################################################################################
########################################################################################
# Proportion of Variance explained by 2 components
sum(pcaOut$sdev[1:2]^2)/sum(pcaOut$sdev^2)
# Proportion of Variance explained by 3 components
sum(pcaOut$sdev[1:3]^2)/sum(pcaOut$sdev^2)


########################################################################################
# FROM HELP(PRCOMP):
# prcomp returns a list with class "prcomp" containing the following components:
# sdev:	
# the standard deviations of the principal components (i.e., the square roots of the eigenvalues of the covariance/correlation matrix, though the calculation is actually done with the singular values of the data matrix).
# rotation:	
# the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors). The function princomp returns this in the element loadings.
# x:	
# if retx is true the value of the rotated data (the centred (and scaled if requested) data multiplied by the rotation matrix) is returned. Hence, cov(x) is the diagonal matrix diag(sdev^2). For the formula method, napredict() is applied to handle the treatment of values omitted by the na.action.
# center, scale:	
# the centering and scaling used, or FALSE. 