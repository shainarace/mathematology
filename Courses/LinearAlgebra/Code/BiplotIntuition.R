#' This code is meant to give the intuition behind the feature arrows in a biplot.
#' If we rotate the box until one corner of the axis appears exactly the same as it
#' does on the two-dimensional plane, then we can see the exact biplot that we see in PCA

library(rgl)
set.seed(0)
library(MASS)
mu=c(0,0,0)
Sigma=matrix(c(1,.5,.5,.5,1,.9,.5,.9,1),3)
dat=data.frame(mvrnorm(50,mu=mu,Sigma=Sigma))
colnames(dat)=c("feature1","feature2","feature3")

attach(dat)
pr.out=prcomp(dat,scale.=T)

plot(feature1,feature2)
plot(feature2,feature3)
plot(feature1,feature3)

plot3d(feature1, feature2, feature3, type = 'n')
points3d(feature1, feature2, feature3, color = 'red', size = 10, transparency=0.9)
shift <- matrix(c(-.1, .1, 0), 12, 3, byrow = TRUE)
text3d(dat+shift,texts=1:50)
grid3d(c("x", "y", "z"))


biplot(pr.out)
grid()
#basis axes
segments3d(c(0,1),c(0,0),c(0,0),lwd=5)
segments3d(c(0,0),c(0,1),c(0,0),lwd=5)
segments3d(c(0,0),c(0,0),c(0,1),lwd=5)
