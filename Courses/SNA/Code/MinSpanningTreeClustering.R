install.packages("igraph")
install.packages("RColorBrewer")
library("igraph")
library("RColorBrewer")

############################################################
############################################################
############################################################
############################################################
####                                             ###########
#### THIS CHUNK OF CODE CREATES A RANDOM NETWORK ###########
####                                             ###########
############################################################
############################################################
############################################################
############################################################
# The idea here is to create something with clear communities
# for illustrative purposes.
# NOTE: WE'RE CLUSTERING A WEIGHTED GRAPH HERE USING WEIGHTS OF THE
# EDGES AS SIMILARITIES -- LATER WE WILL NEED TO CONSTRUCT AN 'OPPOSITE' GRAPH
# WITH THE EDGES REPRESENTING DISTANCE BETWEEN EACH PAIR OF NODES.

# THERE IS MORE THAN 1 WAY TO USE MSTs TO CLUSTER ANY GIVEN GRAPH!!  

set.seed(7515)
a= rep(0,51^2)
A=matrix(a,51,51)
dept=vector()

# Group 1
for(i in 1:11)
{ for(j in 1:11)
{
  A[i,j] = max(floor(rnorm(1,1,3)),0)
}
dept[i]='Sales'}
# Group 2
for(i in 12:15)
{ for(j in 12:15)
{
  A[i,j] = max(floor(rnorm(1,2,2)),0)
}
dept[i]='IT1'}
# Group 3
for(i in 16:21)
{ for(j in 16:21)
{
  A[i,j] = max(floor(rnorm(1,1,1)),0)
}
dept[i]='IT2'}
# Group 4
for(i in 22:31)
{ for(j in 22:31)
{
  A[i,j] = max(floor(rnorm(1,1,4)),0)
}
dept[i]='HR'}
#Group 5
for(i in 32:36)
{ for(j in 32:36)
{
  A[i,j] = max(floor(rnorm(1,5,2)),0)
}
dept[i]='R&D'}
#Group 6
for(i in 37:48)
{ for(j in 37:48)
{
  A[i,j] = max(floor(rnorm(1,1,2)),0)
}
dept[i]='Mgmt'}
#Group 7
for(i in 49:51)
{ for(j in 49:51)
{
  A[i,j] = max(floor(rnorm(1,2,1)),0)
}
dept[i]='Acct'}
#Noise
for(i in 1:51)
{ for(j in 1:51)
{
  if(runif(1,0,1)>0.99) A[i,j] = A[i,j]+1
}}
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################

A=A-diag(diag(A)) # remove self loops 

graph = graph_from_adjacency_matrix(A, mode = c( "undirected"), weighted = T, diag = TRUE, add.colnames = NULL, add.rownames = NA)
colors=brewer.pal(12,"Set3")
V(graph)$color = colors[as.factor(dept)]
plot(graph)

############################################################
############ NOW CREATE A DISTANCE GRAPH FOR MST  ##########
## COMPUTE THE MINIMUM SPANNING TREE FOR DISTANCE GRAPH ####
############################################################

#(14 is max similarity -- want to make that the minimum distance of 1.)
# subtract all entries in adjacency from that max similarity, add 1 
D=max(A)-A+1
D=D-diag(diag(D))
distgraph = graph_from_adjacency_matrix(D, mode = c( "undirected"), weighted = T, diag = TRUE, add.colnames = NULL, add.rownames = NA)
V(distgraph)$color = colors[as.factor(dept)]
plot(distgraph)
x=minimum.spanning.tree(distgraph,weights=E(distgraph)$weight)

############################################################
############################################################
##    PLOT THE GRAPH, THE MST, AND CLUSTERS FOR EACH   #####
#############      LEVEL OF CUTTING THE TREE    ############
############################################################
############################################################

# layout(matrix(c(1,1,2,3,4,5),3,2,byrow=T))
par(mfrow=c(3,2), mai=c(.2,.2,.5,.2))
plot(graph,edge.width=2,main="Original Graph")
plot(x,edge.width=6,main="Minimum Spanning Tree (MST)")
y1=delete_edges(x, E(x)[E(x)$weight>14])
plot(y1,edge.width=6,main="MST drop edges > 14")
y2=delete_edges(x, E(x)[E(x)$weight>13])
plot(y2,edge.width=6,main="MST drop edges > 13")
y3=delete_edges(x, E(x)[E(x)$weight>12])
plot(y3,edge.width=6,main="MST drop edges > 12")
y4=delete_edges(x, E(x)[E(x)$weight>11])
plot(y4,edge.width=6,main="MST drop edges > 11")

dev.off()
