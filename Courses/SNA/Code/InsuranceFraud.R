setwd("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis 2016/Potential Data Sources/InsuranceFraud")
load("Edgelist.Rdata")
load("Employees.Rdata")
load("Claims.Rdata")
load("InsuranceNetwork.Rdata")
library("networkD3")
library("igraph")
claims = claims[,1:8]
save(Employees,claims, file="CarInsuranceFraud.Rdata")
load("CarInsuranceFraud.Rdata")

# # Creation of binary flag variables that characterize the edge list.
# hist(claims$policy_tenure_months)
# claims$rareTenure = as.numeric(claims$policy_tenure_months<48)
# hist(claims$claim_ratio)
# claims$rareRatio = as.numeric(claims$claim_ratio>0.8)
# hist(claims$appraiser_dist)
# claims$rareDist = as.numeric(claims$appraiser_dist>22)
# claims$numFlags = rowSums(cbind(claims$rareDist,claims$rareRatio,claims$rareTenure,claims$cov_inc_6_mos),na.rm = T)
# hist(claims$numFlags)
# claims$anyFlags = as.numeric(claims$numFlags>0)


##################################################################
##################################################################
############# FORCE DIRECTED DRAWING OF ENTIRE GRAPH #############
##################################################################
##################################################################

Employees1=Employees
Employees1$size=1
# Source is Adjuster, Target is Appraiser.
edges$Adjuster_ID = edges$Adjuster_ID-1
edges$Appraiser_ID = edges$Appraiser_ID-1
forceNetwork(Links = edges, Nodes = Employees1, Source="Adjuster_ID", Target="Appraiser_ID", NodeID = "Employee_ID", Group = "Office", linkWidth = 1, Nodesize="size",
             linkColour = "#afafaf", fontSize=12, zoom=T, legend=T,opacity = 0.8)
# Look only at edges where the numFlags >0
# Size nodes by Degree
net2 = delete.edges(net,E(net)[E(net)$numFlags==0])
Employees1$size=degree(net2)
forceNetwork(Links = edges[edges$numFlags>0,], Nodes = Employees1, Source="Adjuster_ID", Target="Appraiser_ID", NodeID = "Employee_ID", Group = "Office", linkWidth = 1, Nodesize="size",
             linkColour = "#afafaf", fontSize=12, zoom=T, legend=T,opacity = 0.8)

#####################################################################################
#####################################################################################
# BEFORE A FRAUD ACCUSATION, GOOD TO CONVINCE OURSELVES THIS IS NOT RANDOM EVENT! ###
#####################################################################################
#####################################################################################
# A person's degree in the "flagged" network is probably proportional to 
# their degree in original claims network.
cor(degree(net),degree(net2))
# Let's see the relationship and see what we're claiming is irregular
plot(degree(net),degree(net2))
hist(degree(net2)/degree(net))
# Can we test the hypothesis that a person's degree in the "flagged" network
# could be 
max(degree(net2))
# just by chance?