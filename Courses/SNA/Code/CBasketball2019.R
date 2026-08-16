#setwd("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis 2016/Potential Data Sources/CBasketball2013")
#' Network of college basketball games in 2012-2013 season.
#' Originally coded to have arrows point to the losers,
#' but in terms of eigenvector centrality we need the edges 
#' directed toward the winners. This code has been updated
#' to reflect that.
library(glue)
read.fwf(file, widths, header = FALSE, sep = "\t",
         skip = 0, row.names, col.names, n = -1,
         buffersize = 2000, fileEncoding = "", ...)
widths = c(12,24,5,24,4,10,20)
scores=read.fwf("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis/Potential Data Sources/CBasketball2013/CBasketballScores.csv",
                widths,fill=T,header=F,stringsAsFactors=F )
scores[,6:7]=NULL
colnames(scores)=c('date','teamA','scoreA','teamB','scoreB')
scores$teamA=trim(scores$teamA)
scores$teamB=trim(scores$teamB)
scores$source=NA
scores$target=NA
scores$source[scores$scoreA<scores$scoreB]=scores$teamA[scores$scoreA<scores$scoreB]
scores$source[scores$scoreA>scores$scoreB]=scores$teamB[scores$scoreA>scores$scoreB]
scores$target[scores$scoreA<scores$scoreB]=scores$teamB[scores$scoreA<scores$scoreB]
scores$target[scores$scoreA>scores$scoreB]=scores$teamA[scores$scoreA>scores$scoreB]
ties = scores$scoreA==scores$scoreB
scores=scores[!ties,]
scores$points=pmax(scores$scoreA,scores$scoreB)-pmin(scores$scoreA,scores$scoreB)

edges = scores[,c('source','target','points')]

library("igraph")
basketball = graph.data.frame(edges,directed=T)
write.graph(basketball,"/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis/Potential Data Sources/CBasketball/basketball2019.graphml",format="graphml")
save(basketball,file="/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis/Potential Data Sources/CBasketball/basketball2019.RData")

l <- layout.fruchterman.reingold(basketball)*10000
plot(basketball,vertex.size=4, vertex.label.cex=1, layout=l, edge.arrow.size=0.2)
Nodes2 = Nodes
Nodes2[,1] = Nodes[,1]-1
Edges2= Edges
Edges2[,c(1,2)]=Edges[,c(1,2)]-1
library("networkD3")
forceNetwork(Links=Edges2, Nodes=Nodes2, Source = "winner",
               Target = "loser", NodeID="Team", Group = "group",
               fontSize=6, opacity = 0.8, zoom=T, legend=T,charge=-1000)
j=forceNetwork(Links=Edges, Nodes=Nodes, Source = "winner",
               Target = "loser", NodeID="Team", Group = "group",
               fontSize=6, opacity = 0.8, zoom=T, legend=T)