setwd("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis 2016/Code")

## Hiring Practices at Fortune Interactive
## The nonexistent edges are not non-hires. won't work...
## Consultants allowing others to join projects?
#########################################
#  Names
Mfn = read.table("MaleNames.csv",sep=",")
Mfn$Gender = "M"
colnames(Mfn)=c("First","Last","Gender")
Ffn = read.table("FemaleNames.csv",sep=",")
Ffn$Gender = "F"
colnames(Ffn)=c("First","Last","Gender")
Employees = rbind(Mfn,Ffn)
Tenure = rpois(71, lambda = 5)
Employees$Tenure=Tenure
#########################################
#Dept = c("R&D","Testing","IT_R&D","IT_Office","HR","Sales","Administration", "Marketing","Finance")
#DeptSize = c(15,5,4,6,3,29,4,8,7)
CollegeFootball = c("UNC", "NCSU","Duke","ECU","Georgia","Alabama","Auburn","Clemson","S.Carolina","None","Other")
RelSizeCF = c(10,15,5,1,6,3,2,1,1,6,8)
CollegeBasketball = c("UNC", "NCSU","Duke","Kentucky","Florida","Wake Forest","Auburn","Clemson","S.Carolina","Kansas","Ohio St","None","Other")
# Michigan, Texas (both sports)
RelSizeCB = c(15,10,25,15,10,5,4,1,1,3,3,8,8)
#########################################
Employees$CollegeFootball = sample(CollegeFootball, 71, replace=T,prob=RelSizeCF)
Employees$CollegeBasketball = sample(CollegeBasketball, 71,replace=T,prob=RelSizeCB)
#########################################
# Pick Leaders on 152 projects based on their tenure.
ProjectLeads = sample(c(1:71),152,replace=T,prob=Tenure)
NumLeads = vector()
for (i in 1:71) {NumLeads[i]=sum(ProjectLeads==i)}
Employees$NumLeads=NumLeads
#########################################
#########################################
########### Network Var: Trust ##########
#########################################
#########################################
# Some symmetric trust relationships
t = sample(c(0,1),71*71,replace=T,prob=c(0.85,0.15))
Trust=matrix(t,nrow=71,ncol=71)
Trust=(Trust+t(Trust))
# Some nonsymmetric trust relationships
p=sample(c(0,1),71*71,replace=T,prob=c(0.97,0.03))
Pert=matrix(p,71,71)
Trust=Trust+Pert
Trust=Trust-diag(diag(Trust))
Trust=as.numeric(Trust>0)
sum(Trust)/(71^2)
Trust=matrix(Trust,71,71)
#########################################
Prob = matrix(0,nrow=71,ncol=71)
for (i in 1:71) {
  for (j in 1:71) {
    logit=-1.75+.9*(Employees$CollegeBasketball[i]==Employees$CollegeBasketball[j])+
          .8*(Employees$CollegeFootball[i]==Employees$CollegeFootball[j])+
          0.4*(Employees$Gender[i]==Employees$Gender[j])+1.8*Trust[i,j]
    Prob[i,j]=1/(1+exp(-1*logit))
  }
}
Prob=Prob-diag(diag(Prob))
#########################################
# Create List of Neighbors
Neighbors=list()
for (i in 1:71) {
  if(Employees$NumLeads[i]>0) {Neighbors[[i]]=sample(1:71,3*Employees$NumLeads[i],replace=T,prob=Prob[i,])} else {Neighbors[[i]]=NULL}
}
Employees$Chosen=Neighbors
P=matrix(0,71,71)
for (i in 1:71) {
  P[i,Employees$Chosen[[i]]]=1
}


net=graph_from_adjacency_matrix(P)
plot(net)
d1=degree(net,mode="in")
hist(d1,max(d1),xlim=range(0:max(d1)),ylim=range(0:20),xlab="In Degree")
d2=degree(net,mode="out")
hist(d2,max(d2),xlim=range(0:max(d2)),ylim=range(0:20),xlab="Out Degree")
d3=degree(net)
hist(d3,max(d3),xlim=range(0:max(d3)),ylim=range(0:20),xlab="Total Degree")

#########################################
#########################################



#########################################
#########################################
#########################################

# Save the original Dataset
setwd("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis 2016/Potential Data Sources/FI Consulting/")
save(Employees, file="Employees")
load("Employees")
save(Trust,file="FITrustMatrix")
#########################################
#########################################
#########################################
# CREATE STATNET NETWORK DATASET

FInet=network(P)
list.network.attributes(FInet)
FInet%n%'directed'
FInet%n%'loops'
FInet%n%'Network Name' = "Fortune Interactive: Consultants' choosing other consultants to join projects"
FInet%v%colnames(Employees)[1:7]=Employees[,1:7]
set.edge.value(FInet,"Trust",Trust)

list.vertex.attributes(FInet)
# Save the network Dataset
save(FInet, file="FInetwork")
FItrust=network(Trust)
save(FItrust,file="FITrustNetwork")

# CREATE IGRAPH GRAPH DATASET
load("Employees")
load("FITrustMatri")
FIgraph=graph_from_adjacency_matrix(P)
V(FIgraph)$First=as.character(Employees[,"First"])
V(FIgraph)$Last=as.character(Employees[,"Last"])
V(FIgraph)$Tenure=Employees[,"Tenure"]
V(FIgraph)$CollegeBasketball=Employees[,"CollegeBasketball"]
V(FIgraph)$CollegeFootball=Employees[,"CollegeFootball"]
V(FIgraph)$NumLeads=Employees[,"NumLeads"]
#### DID NOT FINISH HERE
# E(FIgraph)$Trust = c(t(Trust))[c(t(Trust))!=0]
# set_edge_attr(FIgraph,"trust",value=Trust)
#### DID NOT FINISH HERE

save(FIgraph,file="FIigraph")
write.graph(FIgraph,"FIgraph.gml",format="gml")

# Save the graph as gml using igraph package


