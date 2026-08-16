# Set your working directory to wherever you store the FIigraph data file that you download.
setwd('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis 2016/Potential Data Sources/FI Consulting')
#######################################################
#######################################################
load("FIigraph") # The name of the file, which contains a graph object called FIgraph.
vertex_attr(FIgraph) # Lists all the vertex data
plot(FIgraph)
plot(FIgraph, edge.arrow.size = .3, vertex.label=NA,vertex.size=10,
     vertex.color='gray',edge.color='blue')
plot(FIgraph, edge.arrow.size = .3, vertex.label=NA,vertex.size=10,
     vertex.color='lightblue',layout=layout.fruchterman.reingold)
############################################################
### Other potential layouts
############################################################
?layout
l = layout.sphere(FIgraph) 
l2 = layout_nicely(FIgraph)
l3 = layout.mds(FIgraph)
l4 = layout.davidson.harel(FIgraph)
par(mfrow=c(2,2),mar=c(1,1,1,1)) # Tells the graphic window to use the
# following plots to fill out a 2x2 grid with margins of 1 unit 
# on each side. Must reset these options with dev.off() when done!
plot(FIgraph, edge.arrow.size = .3, vertex.label=NA,vertex.size=10,
     vertex.color='lightblue', layout=l,main="Sphere")
plot(FIgraph, edge.arrow.size = .3, vertex.label=NA,vertex.size=10,
     vertex.color='lightblue', layout=l2,main="Nicely")
plot(FIgraph, edge.arrow.size = .3, vertex.label=NA,vertex.size=10,
     vertex.color='lightblue', layout=l3,main="MDS")
plot(FIgraph, edge.arrow.size = .3, vertex.label=NA,vertex.size=10,
     vertex.color='lightblue', layout=l4,main = "Davidson Harel")
dev.off() #resets the graphic window options.


############################################################
# SOME NICE COLOR PACKAGES, AS POINTED OUT IN THE VIZGUIDE #
############################################################
install.packages("RColorBrewer")
library(RColorBrewer) 
display.brewer.all()
display.brewer.pal(12,"Set3")
#############################################################
# COLORING THE GRAPH BY VERTEX ATTRIBUTE "COLLEGE FOOTBALL" #
# BY SETTING THE COLOR ATTRIBUTE DIRECTLY IN GRAPH          #
#############################################################
colors=brewer.pal(12,"Set3")
V(FIgraph)$color=colors[as.factor(V(FIgraph)$CollegeFootball)]

plot(FIgraph, edge.arrow.size = .3, vertex.label=V(FIgraph)$First, vertex.size=10)

###################################################################
# CREATING A LEGEND, WHICH IS PERHAPS NOT AS EASY AS IT SHOULD BE #
###################################################################

V(FIgraph)$CollegeFootball
unique(V(FIgraph)$CollegeFootball)
legend(x=-1.5,y=0,unique(V(FIgraph)$CollegeFootball),pch=21,
       pt.bg=colors,pt.cex=3,bty="n",ncol=1)
# pch =21 makes circles 
# pt.cex controls size of circles
# bty="n" means no frame around it (switch to "y" for frame)
# Then you'll need to tweak the legend and move it around...
# recreate the graph each time as you can see here:
legend(x=-1.5,y=0,unique(V(FIgraph)$CollegeFootball),pch=21,
       pt.bg=colors,pt.cex=3,bty="n",ncol=1)

#######################################################
#######################################################
# IF TIME PERMITS:    PACKAGE NETWORKD3
#######################################################
#######################################################
# This package insists that the label names (indices)
# of your nodes start from zero. That is something to be
# aware of when moving between R and python as well!!
# If they don't start from zero the graph just won't render
# you won't even get a warning message!! 

# To use this package, you need a data frame containing
# the edge list and a data frame containing the node data.

edges=data.frame(get.edgelist(FIgraph)) #data frame with edge list
colnames(edges)=c("source","target")
edges$source=edges$source-1 # make the numbering start from 0!!
edges$target=edges$target-1

nodes=data.frame(vertex_attr(FIgraph)) #data frame with node data

install.packages("networkD3")
library("networkD3")

forceNetwork(Links=edges, Nodes=nodes, Source = "source",
             Target = "target", NodeID="First", Group="CollegeFootball",
             fontSize=12, opacity = 0.8, zoom=T, legend=T)

# You can use the "Charge" of the Algorithm to change the spread of the layout
# A negative charge creates a repulsive force between the nodes, a positive
# charge creates an attractive force.

forceNetwork(Links=edges, Nodes=nodes, Source = "source",
             Target = "target", NodeID="First", Group="CollegeFootball",
             charge=-1000,fontSize=12, opacity = 0.8, zoom=T, legend=T)

#######################################################
#######################################################
# SAVING THE HTML FILE WITH THE D3 GRAPH
#######################################################
#######################################################
j=forceNetwork(Links=edges, Nodes=nodes, Source = "source",
               Target = "target", NodeID="First", Group="CollegeFootball",
               fontSize=12, opacity = 0.8, zoom=T, legend=T)
library(magrittr)
saveNetwork(j, file = 'FIgraph.html')






