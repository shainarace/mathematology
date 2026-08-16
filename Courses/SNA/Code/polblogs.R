setwd('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis 2016/Potential Data Sources/polblogs')
load('polblogs.Rdata')
library('igraph')
##############################################################
# This code tests the hypothesis that the liberal blogs are  #
# more "cliquish" than the conservative blogs, i.e. that the #
# difference in their clustering coefficient is significant. #
##############################################################

# polblogs=upgrade_graph(polblogs)

list.vertex.attributes(polblogs)

gsize(polblogs) #number of edges
vcount(polblogs) # number of vertices
graph.attributes(polblogs)
transitivity(polblogs, type="global")
##############################################################
#  separate graph into two subgraphs according to whether  ###
#            they are liberal/conservative                 ###
##############################################################
lib=induced_subgraph(polblogs, V(polblogs)$LeftRight==0 )
con=induced_subgraph(polblogs, V(polblogs)$LeftRight==1 )
#remove vertices w/degree 0
lib=induced_subgraph(lib, degree(lib)>0)
con=induced_subgraph(con, degree(con)>0)
# check out #edges and #vertices in each component
gsize(lib)
vcount(lib)
gsize(con)
vcount(con)

# Compute the clustering coefficient of the liberal blogs
tl=transitivity(lib,type="global")
# Compute the clustering coefficient of the conservative blogs
tc=transitivity(con,type="global")

# This is the difference. We're going to test to see if it is significant
transitivity_difference_observed = tl-tc

# We'll do that by simulating random graphs, one with the same degree
# distribution as the liberal network, one with the same degree distribution 
# of the conservative network. The following commands calculate the
# degree distribution for each subgraph.
dl = degree(lib)
dc = degree(con)

transitivity_diff_simulations=vector()
for(i in 1:1000){
  #generate 1000 #random networks w/same degree distributions and no self-loops
  ran_lib = degree.sequence.game(dl,method="vl")
  ran_con = degree.sequence.game(dc, method="vl")
  tl_ran = transitivity(ran_lib, type="global")
  tc_ran = transitivity(ran_con,type="global")
  
  transitivity_diff_simulations[i] = tl_ran-tc_ran
}
hist(transitivity_diff_simulations,xlim=c(0,.15),col='gray',main="1000 Simulated Differences in Transitivity",xlab = 'Difference in Lib/Con Transitivity', ylab = 'Number of Simulations')
points(transitivity_difference_observed, 3, pch = "X", cex=2)

# What's kind of interesting about this is that we do in fact expect that
# the clustering coefficient of the liberal blogs is greater than that
# of the conservative blogs based only on their degree distribution.
# The histogram of simulated differences is NOT centered around 0, in fact it's
# centered around 0.09 or so. So yes, a CC difference of 0.11 is statistically 
# significant, but not as numerically significant as it sounds!
