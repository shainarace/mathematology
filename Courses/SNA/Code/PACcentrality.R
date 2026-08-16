setwd('/Users/shaina/Desktop/Data Sets/CampaignFin16')
PACdata=read.csv("PACgraphUndirectedNodes.csv", sep=',',header=T)
PACdata$degree = data$degree/2932

# scatterplot matrix
pairs(PACdata[,c(5,7,16,18)])

# just 4 plots
par(mfrow=c(2,2))
plot(PACdata$closnesscentrality,PACdata$harmonicclosnesscentrality,ylab="Harmonic Closeness Centrality",xlab="Closeness Centrality")
plot(PACdata$closnesscentrality, PACdata$betweenesscentrality, ylab="Betweenness Centrality",xlab="Closeness Centrality")
#text(PACdata$closnesscentrality, PACdata$betweenesscentrality, PACdata$id)
plot(PACdata$closnesscentrality, PACdata$degree,ylab="Degree Centrality", xlab="Closeness Centrality")
plot(PACdata$betweenesscentrality, PACdata$degree,ylab="Degree Centrality", xlab="Betweenness Centrality")

# Closeness Centrality is high in the smaller components of the graph.
#

dev.off()
g=data[(PACdata$closenesscentrality>0.6 & PACdata$degree<0.04), ]


