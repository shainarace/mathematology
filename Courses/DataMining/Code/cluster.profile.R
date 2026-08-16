
#############################################################

cluster.profile = function(data,clusterID,colsToProfile){
#############################################################
#############################################################
### Model inputs:
#############################################################
#######' data = data.frame containing your data
#######' clusterID = column vector containing the 
#######'      cluster IDs for each observation in data
#######' colsToProfile = character vector listing 
#######'      the column names from data of variables 
#######'      you wish to see profiles on.
#############################################################
#############################################################

# 25 bins for each histogram, but you can customize
# this with a vector containing the number of bins 
# for each variable in colsToProfile.
nbins = rep(25,length(colsToProfile))
names(nbins) = colsToProfile
k=length(unique(clusterID))

# Determine numeric columns
nums = sapply(data[,colsToProfile],is.numeric)
num.colsToProfile = colsToProfile[nums]
# Determine factor columns
facs = sapply(data[,colsToProfile],is.factor)
fac.colsToProfile = colsToProfile[facs]
# Create dual histograms comparing numeric columns
# for each cluster vs. the population
for(coli in num.colsToProfile){
  # binwidths for histograms
  max = max(data[,coli])
  min = min(data[,coli])
  range = max-min
  binwidth = range/nbins[coli]
  step = 0:nbins[coli]
  breaks = min+step*binwidth
  
  # output plots will form one column for each cluster, 
  # one row for each variable. That can be changed here.
  
  par(mfrow=c(1,k))
  par(oma = c(7, 3, 3, 1.2),mar=c(3,3,3,1.2))
  
  for(idi in unique(clusterID[order(clusterID)])){
    hist(data[, coli],freq=F, breaks = breaks,
        col=rgb(1,0,0,0.5), xlab=paste(coli),
         ylab="Density", main=paste(coli, 'ClusterID:', idi) ,cex.main=2.5)
    hist(data[clusterID == idi, coli], freq=F,  breaks = breaks,
        col=rgb(0,0,1,0.5), xlab="",ylab="",add=T)
 } 
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom",xpd=T,inset=c(0,0),legend=c("Population",paste('Cluster')), pch=15, col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horiz=TRUE, bty='n', cex=2)
 }
# Create grouped bar plots comparing factor columns
# for each cluster vs. the population
  for(coli in fac.colsToProfile){
    par(mfrow=c(1,k))
    par(oma = c(7, 3, 3, 1.2),mar=c(3,3,3,1.2))
    for(idi in unique(clusterID[order(clusterID)])){
      # Grouped Bar Plot
      nlev = length(levels(data[,coli]))
      pop.counts = table(data[,coli])/nrow(data)
      cl.counts = table(data[clusterID==idi,coli])/sum(clusterID==idi)
      counts = rbind(pop.counts,cl.counts)
      barplot(counts, main=paste(coli, 'ClusterID:', idi),cex.main=2.5, 
              xlab=paste(coli), col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), beside=TRUE)
   }
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("bottom",xpd=T,inset=c(0,0),legend=c("Population",paste('Cluster')), pch=15, col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),horiz=TRUE, bty='n', cex=2)
  }
  
}
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
### Example 1: Iris Dataset 
#############################################################

library(datasets)
data(iris)
t=kmeans(iris[,1:4],3)
data=data.frame(iris[,1:4],t$cluster)
clusterID=data$t.cluster
colsToProfile=colnames(iris)[1:4]
###   ###   ###   ###   ###   ###  ###   ###   ###
cluster.profile(data,clusterID,colsToProfile)
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
### Example 2: TeenSNS Dataset 
#############################################################
load("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Data Mining 2020/Code/TeenSNS.RData")
colsToProfile = colnames(teens)[c(1:40)]
###   ###   ###   ###   ###   ###  ###   ###   ###
cluster.profile(teens,teens$cluster,colsToProfile)


