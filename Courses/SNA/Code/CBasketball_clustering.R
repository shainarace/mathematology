
############################################################
######                   BASKETBALL DATA               #####
############################################################
setwd('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis/Potential Data Sources/CBasketball/2021/')
load("basketball2021.RData")
library(igraph)
basketball=as.undirected(basketball2021)
vertex_attr(basketball)
basketball = simplify(basketball, remove.multiple = TRUE)
#' Storing some layouts with which i can plot all the clusterings 
#' (if want to visually compare clusterings to each other, it will
#' help to have the nodes located in the same spot on the visual!)

#l = layout_with_drl(basketball,  dim = 2, options = list("expansion.attraction" = 20,"expansion.temperature" = 10))
l = layout_with_fr(basketball,  dim = 2)
?layout

############################################################
######      CLUSTERING METHODS IN IGRAPH PACKAGE       #####
############################################################

#' In iGraph Package: The different methods for finding 
#' communities, they all return a communities object: 
#' cluster_edge_betweenness, cluster_fast_greedy, 
#' cluster_label_prop, cluster_leading_eigen, 
#' cluster_louvain, cluster_optimal, cluster_spinglass, 
#' cluster_walktrap.

# FAST, efficient, decent clusters 
# (Modularity Maximization Algorithm):
c=cluster_leading_eigen(basketball)
c$membership
# Nice start to graphics with the bounding shapes but layout imperfect with no good solution:
plot(c,basketball,edge.arrow.size=0.1, layout = l, vertex.size = 3, font.size=5, edge.color = "gray")

# Another method
# No one promised you nice clusters...
c2=cluster_infomap(basketball)
plot(c2,basketball,edge.arrow.size=0.1, layout = l,  vertex.size = 3, font.size=7, edge.color = "gray")

c3=cluster_label_prop(basketball)
plot(c3,basketball,edge.arrow.size=0.1, layout = l,  vertex.size = 3, font.size=7, edge.color = "gray")

c4=cluster_fast_greedy(basketball)
plot(c4,basketball,edge.arrow.size=0.1, layout = l,  vertex.size = 3, font.size=7, edge.color = "gray")


# SLOW: cluster_optimal, cluster_edge_betweenness


#' Modularity is the fraction of the edges that fall 
#' within the given groups minus the expected such 
#' fraction if edges were distributed at random. The 
#' value of the modularity lies in the range (âˆ’1,1). 
#' It is positive if the number of edges within groups 
#' exceeds the number expected on the basis of chance.
#' 
#' 
#' The clustering with the highest Modularity is supposed to be the best

modularity(c)
modularity(c2)
modularity(c3)
modularity(c4)

# Interestingly enough the modularity maximization algorithm in R does not
# always give the highest value of modularity among the clustering algorithms in R
# #MathIsPerfectAppliedMathIsImperfect



# Spectral Clustering using KERNLAB package. If A is an adjacency matrix, 
# then the following code will create k clusters.

install.packages("kernlab")
library("kernlab")
k=2
A=as.matrix(get.adjacency(basketball))
A2=as.kernelMatrix(A)
c=specc(A2,k)

