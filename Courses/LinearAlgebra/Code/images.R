setwd('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Linear Algebra/Linear Algebra 2020/MSA2020Photos/')
#install.packages('imager')
library('imager')
#################################################################################
#################################################################################
# Read all the images into a dataframe. They will be columns in this data frame.
# Grayscale pixels have value = 1 for white and 0 for black
#################################################################################
#################################################################################
Names = read.csv('lastnames.csv',header=T,colClasses = c("character"))
pics=as.data.frame(grayscale(load.image(paste(Names[1,],".jpg",sep=''))))
for (i in 2:119) {
  print(Names[i,])
  g=as.data.frame(grayscale(load.image(paste(Names[i,],".jpg",sep=''))))
  pics=cbind(pics,g[,3])
}
colnames(pics)=c("x","y",Names[,1])
#################################################################################
#################################################################################
# Plot a test image, enter a students' last name below
#################################################################################
#################################################################################
lastName=""
j=data.frame(x=pics$x,y=pics$y,value=pics[,lastName])
test.image=as.cimg(j)
plot(test.image)
#################################################################################
#################################################################################
# Create a matrix where each column is one student image. Don't need x and y
# Decompose this matrix using the SVD
#################################################################################
#################################################################################
X=as.matrix(pics[,3:121])
dim(X) #SVD will run faster if you keep your observations as columns (fewer columns)
#tic=Sys.time()
out=svd(X)
#(toc=Sys.time()-tic)  
# 5.719315 secs
u=out$u
d=out$d
v=out$v
vt=t(out$v)
rownames(v)=Names[,1]
colnames(vt)=Names[,1]
#################################################################################
#################################################################################
# plot the projection of the data into 2 dimensions (from 160,000)
#################################################################################
#################################################################################
plot(x=v[,1],y=v[,2],xlab='Singular Component 1',ylab='Singular Component 2', col='purple')
text(x=v[,1],y=v[,2],Names[,1])
#################################################################################
#################################################################################
# plot the re-creation of the any one column of matrix (i.e. one image)
#################################################################################
#################################################################################
#lastName = ""
value =  u %*% diag(d) %*% vt[,lastName] 
im=as.cimg(data.frame(x=pics$x,y=pics$y,value))
plot(im)
#################################################################################
#################################################################################
# Now let's approximate someone using only a few (say n) singular components
#################################################################################
#################################################################################
#lastName = ""
n=50
value =  u[,1:n] %*% diag(d[1:n]) %*% vt[1:n,lastName] 
im=as.cimg(data.frame(x=pics$x,y=pics$y,value))
plot(im)
#################################################################################
#################################################################################
# We can run a loop and see the progression as we increase the number
# of components
#################################################################################
#################################################################################
par(mfrow=c(3,4))
#lastName=""
ind=seq(2,121,by=floor(121/12))
for (n in ind){
  value =  u[,1:n] %*% diag(d[1:n]) %*% vt[1:n,lastName] 
  im=as.cimg(data.frame(x=pics$x,y=pics$y,value))
  plot(im,xlim=c(0,400), main=paste("first",n,"dimensions"))
}
#################################################################################
#################################################################################
# What happens when we use the components in the opposite order, starting with 
# the last one, then the last 11, and ending with all? The first few pictures 
# in this sequence is what we are losing when we exclude components. It's 
# the error in our approximation. It is mostly noise.
#################################################################################
#################################################################################
par(mfrow=c(3,4))
#lastName=""
ind=seq(1,111,by=10)
for (n in ind){
  value =  u[,(111-n):111] %*% diag(d[(111-n):111]) %*% vt[(111-n):111,lastName] 
  im=as.cimg(data.frame(x=pics$x,y=pics$y,value))
  plot(im,xlim=c(0,400), main=paste("LAST",n,"dimensions"))
}

#################################################################################
#################################################################################
#'  EIGENFACES.
#' Note that our previous recreations of a person were linear combinations of
#' "basis images" weighted by entries in the corresponding column of vt. 
#' Now let's take a look at the basis components we are adding to form our
#' re-creations. These are often called Eigenfaces.
#' 
#################################################################################
#################################################################################
par(mfrow=c(3,3))
ind=1:9
for (n in ind){
  value =  u[,n]
  im=as.cimg(data.frame(x=pics$x,y=pics$y,value))
  plot(im,xlim=c(-50,450), main=paste("Singular Component",n))
}
#################################################################################
#################################################################################
#' Looks like the second singular vector/component is picking out whether a 
#' candidate is displaying has long hair, whereas the third component identifies
#' the height/size of the shoulders. This is somewhat descriptive of a person but
#' also involves the camera angle and the person's posture in the image.
#' 
#' The first singular component indicates that the image is a "person" 
#' This is the main practical difference between the SVD and PCA in high-dimensional
#' data. Since PCA involves centered data, it essentially assumes this "mean" face
#' at the origin. SVD will often have some representation of a mean image in the 
#' first component.


#################################################################################
#################################################################################
# Let's now try to approximate MY headshot with this basis and see how well we can 
# recreate an image that we've NEVER SEEN. Want to solve the system UDx=race.
# This is simple to do given the nature of U and D. Using U' to be U-Transpose:
# x = D^{-1} U' race
# inverse of a diagonal matrix is diag(1/d)
# This will form the orthogonal projection of my face onto the span of the 
# 111 principal components from the facial universe of the class.
#################################################################################
#################################################################################
race = as.data.frame(grayscale(load.image("Race.jpg")))
raceScores=t(u)%*%race$value
# Recreate the approximated image using the scores:
value =  u %*% raceScores 
im=as.cimg(data.frame(x=pics$x,y=pics$y,value))
par(mfrow=c(1,2))
plot(im,xlim=c(0,400), main = 'Approximation of Shaina using Student Components')
plot(grayscale(load.image("Race.jpg"))) #Original


#################################################################################
#################################################################################
#################################################################################
#' How about Principal Components? Do we get something drastically different than
#' we do with the singular value decomposition?  I would expect the first 
#' component to change, since we're assuming that a "mean face" is at the center
#' of the vector space rather than the zero vector which corresponds to all black
#' pixels
#################################################################################
#################################################################################
Xt=t(X)
#tic=Sys.time()
pca=prcomp(Xt)
#(toc=Sys.time()-tic)
# 8.441261 secs
# Screeplot
par(mfrow=c(1,1))
plot(pca$sdev^2, ylab = 'Eigenvalue',col='purple',pch=16)
# The scores are not normalized to have unit norm. The diagonal matrix of singular
# values given by "sdev" is already multiplied into the orthogonal matrix V to make
# the output variable x containing the PC scores. We'll indicate this by naming the matrix
# dvt2 indicating that it is equal to D*V^T in the singular value decomposition.
u2=pca$rotation
dvt2=t(pca$x)
colnames(vt2)=Names[,1]
# Plots of the 'eigenfaces' or basis images
par(mfrow=c(3,3))
ind=1:9
for (n in ind){
  value =  u2[,n]
  im=as.cimg(data.frame(x=pics$x,y=pics$y,value))
  plot(im,xlim=c(0,400),, main=paste("Principal Component",n))
}

#################################################################################
#################################################################################
# Plot the mean image which will need to be added back into 
# any approximation of a specific face:
#################################################################################
#################################################################################
meanImage = rowMeans(X)
im=as.cimg(data.frame(x=pics$x,y=pics$y,value=meanImage))
plot(im,xlim=c(0,400))
#################################################################################
# Re-creation of one student in the PCA space:
#################################################################################
#################################################################################
#lastName=""
par(mfrow=c(3,4))
ind=seq(1,111,by=10)
for (n in ind){
  if (n==1) {value =  u2[,1]*dvt2[1,lastName]+meanImage}
  else {value =  u2[,1:n] %*% dvt2[1:n,lastName]+meanImage}
  im=as.cimg(data.frame(x=pics$x,y=pics$y,value))
  plot(im,xlim=c(0,400), main=paste("first",n,"dimensions"))
}
#################################################################################
#################################################################################
# Compute which two images are most similar in the Euclidean sense.
# Note, since the data is essentially 111 dimensional (even though 
# it has 160,000 variables), Using the SVD coordinates will save us a
# LOT of time in these distance calculations. Could even cluster the student images!
# 
#################################################################################
# FINDING THE IMAGES THAT ARE "CLOSEST" USING ALL DIMENSIONS (NO APPROXIMATION OF DATA)
# AND VARIOUS NORMS TO MEASURE DISTANCE.
#################################################################################
# Euclidean Distance (2-Norm)
#################################################################################
coord=v%*%diag(d)
D=as.matrix(dist(coord))
D[D==0]=10000
(who = which(D==min(D), arr.ind = T))
value1=pics[,who[1,1]+2] #Adding 2 because of x/y columns at beginning of data frame
value2=pics[,who[1,2]+2]
im1=as.cimg(data.frame(x=pics$x,y=pics$y,value=value1))
im2=as.cimg(data.frame(x=pics$x,y=pics$y,value=value2))
par(mfrow=c(1,2))
plot(im1)
plot(im2)
#################################################################################
# City Block Distance (1-Norm)
#################################################################################
D=as.matrix(dist(coord, method="manhattan"))
D[D==0]=10000
(who = which(D==min(D), arr.ind = T))
value1=pics[,who[1,1]+2] #Adding 2 because of x/y columns at beginning of data frame
value2=pics[,who[1,2]+2]
im1=as.cimg(data.frame(x=pics$x,y=pics$y,value=value1))
im2=as.cimg(data.frame(x=pics$x,y=pics$y,value=value2))
par(mfrow=c(1,2))
plot(im1)
plot(im2)
#################################################################################
# Max Distance (Infinity-Norm)
#################################################################################
D=as.matrix(dist(coord, method="maximum"))
D[D==0]=10000
(who = which(D==min(D), arr.ind = T))
value1=pics[,who[1,1]+2] #Adding 2 because of x/y columns at beginning of data frame
value2=pics[,who[1,2]+2]
im1=as.cimg(data.frame(x=pics$x,y=pics$y,value=value1))
im2=as.cimg(data.frame(x=pics$x,y=pics$y,value=value2))
par(mfrow=c(1,2))
plot(im1)
plot(im2)
#################################################################################

# Reset your plot window with: (Erases all previous plots)
#dev.off()
