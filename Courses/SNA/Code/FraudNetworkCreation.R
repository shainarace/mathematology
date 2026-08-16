# # Years on job
# # Distance to claim ==> Create probability of adjuster
# # Value of the claim (relative to KBB value of car)
# # Coverage change within 6 months prior to claim
# # Age of policy
# # Claim ID
# # Adjuster ID
# # Appraiser ID (if any)
# 
# # Bipartite network of Adjusters to Claims to Appraisers
# # 24 Adjusters
# # 978 Claims
# # 11 Appraisers
# setwd('/Users/shaina/Desktop/Data Sets/Race_Fraud')
# 
# # # Employee Data
# 
# # Adjusters
# names = read.csv("names.csv",sep=",",header=T)
# Employee_ID = 1:24
# offices = c("Braintree", "Plymouth", "Boston", "Brockton", "New Bedford", "Fall River", "Taunton")
# Office=offices[c(1,1,1,2,2,2,3,3,3,3,3,4,4,4,5,5,5,5,6,6,6,7,7,7)]
# Role = rep("Adjuster",24)
# Adjusters = data.frame(cbind(Employee_ID,Office,Role,as.character(names$first_name[1:24]),as.character(names$last_name[1:24])))
# colnames(Adjusters)[4:5]=c("First","Last")
# 
# # Appraisers
# Employee_ID = 25:35
# Office=offices[c(1,2,3,3,3,4,5,5,6,6,7)]
# Role = rep("Appraiser",11)
# Appraisers=data.frame(cbind(Employee_ID,Office,Role,as.character(names$first_name[25:35]),as.character(names$last_name[25:35])))
# colnames(Appraisers)[4:5]=c("First","Last")
# 
# Employees = rbind(Adjusters,Appraisers)
# Tenure=c( 7,  5, 11, 22,  4,  6 , 4,  3,  8,  5,  1,  7,  5, 11, 22,  4,
#           6 , 4,  3 , 8,  5,  1,  7 , 5,  7 , 5 ,11,22 , 4,  6 , 4,  3 , 8 , 5 , 1)
# Employees$Tenure = Tenure
# 
# write.xlsx(Employees,'Employees.xlsx')
# write.table(Employees, file = "employees.csv", append = FALSE, quote = F, sep = ",",
#             eol = "\n", na = "", dec = ".", row.names = F,
#             col.names = TRUE)
# save(Employees, file="Employees")
# 
# #####################
# # Claim Data
# set.seed(7515)
# # Which Appraiser:
# oSize = c(1,1,3,1,2,2,1)
# # Most claims don't require Appraiser
# aSize = c(1,1,3,1,2,2,1,20)
# propAdj = oSize[c(1,1,1,2,2,2,3,3,3,3,3,4,4,4,5,5,5,5,6,6,6,7,7,7)]
# probAdj = propAdj/sum(propAdj)
# Adjuster = sample(1:24,978,replace=T,prob=probAdj)
# # Appraiser depends on adjuster. Matrix indicating same workplace
# D=matrix(nrow=24,ncol=11)
# for(i in 1:24){for(j in 1:11) {D[i,j]=(Adjusters[i,2]==Appraisers[j,2])}}
# # Turn into probabilities. For whatever reason, apply transposes matrix!
# D = t(apply(D,1,function(x) x/sum(x)))
# # For each row of D, 50% chance of using those probs, 45% of none and 5% distributed evenly.
# Appraiser=vector()
# for (i in 1:978){
#   prob = 0.5*D[Adjuster[i],]+0.05/11*rep(1,11)
#   probApp=c(prob,.45)
#   Appraiser[i] = sample(c(25:35,NA),1,prob=probApp)
# }
# Claim_ID = round(1000000000*runif(978,min=0.1,max=1),digits=7)
# claims=data.frame(cbind(Claim_ID,Adjuster,Appraiser))
# colnames(claims) = c("Claim_ID","Adjuster_ID","Appraiser_ID")
# appraiser_dist = rnorm(978,mean=15, sd=4)
# appraiser_dist[is.na(claims$Appraiser_ID)] = NA
# claims$appraiser_dist=appraiser_dist
# claim_amt = rnorm(978,mean=675,sd = 100)
# claim_amt[!is.na(claims$Appraiser_ID)] = rnorm(sum(!is.na(claims$Appraiser_ID)),mean=8600, sd = 2000)
# claims$claim_amt = claim_amt
# claim_ratio = rep(NA,978)
# claim_ratio[!is.na(claims$Appraiser_ID)] = abs(rnorm(sum(!is.na(claims$Appraiser_ID)),mean=0.5, sd = 0.2))
# claims$claim_ratio=claim_ratio
# # select the number of tenure months with varying windows of probability
# policy_tenure_months = sample(1:186, 978, replace = T, prob=c(runif(30,min=0.06,max=0.1),runif(90,min=0.06,max=0.7),runif(30,min=0.06,max=0.3),runif(30,min=0.06,max=0.2),runif(6,min=0.06,max=0.1)))
# hist(policy_tenure_months)
# cov_inc_6_mos = sample(c(0,1),978,replace=T,prob=c(0.97,0.03))
# claims$cov_inc_6_mos=cov_inc_6_mos
# claims$policy_tenure_months=policy_tenure_months
# 
# ## Factor in some bad stuff
# # Change some of the NAs for Adj's 8 and 11 to be working with App's 25 and 35 with some probability
# claims$Appraiser_ID[(claims$Adjuster_ID==8|claims$Adjuster_ID==11)&is.na(claims$Appraiser_ID)] = sample(c(NA,35,25),length(claims$Appraiser_ID[(claims$Adjuster_ID==8|claims$Adjuster_ID==11)&is.na(claims$Appraiser_ID)]),replace=T,prob=c(.6,.2,.2))
# bad = (claims$Adjuster_ID==8 |claims$Adjuster_ID==11) & (claims$Appraiser_ID==27|claims$Appraiser_ID==29|claims$Appraiser_ID==25|claims$Appraiser_ID==35)
# bad[is.na(bad)]=FALSE
# claims$appraiser_dist[bad]=rnorm(sum(bad),mean=20,sd=4)
# claims$claim_ratio[bad] = rnorm(sum(bad),mean=0.8, sd=0.2)
# claims$cov_inc_6_mos[bad] = sample(c(0,1),sum(bad),replace=T,prob=c(0.7,0.3))
# claims$policy_tenure_months[bad]= sample(1:186, sum(bad), replace = T, prob=c(runif(30,min=0.1,max=0.9),runif(90,min=0.2,max=0.7),runif(30,min=0.06,max=0.3),runif(30,min=0.06,max=0.2),runif(6,min=0.06,max=0.1)))
# 
# 
# 
# # Create binary variables
# hist(claims$policy_tenure_months)
# claims$rareTenure = as.numeric(claims$policy_tenure_months<48)
# hist(claims$claim_ratio)
# claims$rareRatio = as.numeric(claims$claim_ratio>0.8)
# hist(claims$appraiser_dist)
# claims$rareDist = as.numeric(claims$appraiser_dist>22)
# claims$numFlags = rowSums(cbind(claims$rareDist,claims$rareRatio,claims$rareTenure,claims$cov_inc_6_mos),na.rm = T)
# hist(claims$numFlags)
# claims$anyFlags = as.numeric(claims$numFlags>0)
# 
# write.xlsx(claims,'Claims.xlsx')
# write.table(claims, file = "claims.csv", append = FALSE, quote = F, sep = ",",
#             eol = "\n", na = "", dec = ".", row.names = F,
#             col.names = TRUE)
# save(claims, file="Claims.Rdata")
# 
# edges=claims[!is.na(claims$Appraiser_ID),c(2,3,1,4:13)]
# #colnames(edges)[1:2]=c("Source","Target")
# write.table(edges, file = "edges.csv", append = FALSE, quote = F, sep = ",",
#             eol = "\n", na = "", dec = ".", row.names = F,
#             col.names = TRUE)
# 
# 
# tst=claims[claims$numFlags>1,]
# table(tst$Adjuster_ID)
# table(tst$Appraiser_ID)
# 
# # Working toward a network viz:
# edgelist=as.matrix(edges[!is.na(edges$Target),1:2])
# net=graph_from_edgelist(edges,directed=F)
# 
# V(net)$Role = Employees$Role
# V(net)$Office = Employees$Office
# 
# E(net)$ClaimID = edges[!is.na(edges$Appraiser_ID),"Claim_ID"]
# E(net)$AppDist = edges[!is.na(edges$Appraiser_ID),"appraiser_dist"]
# E(net)$numFlags = edges[!is.na(edges$Appraiser_ID),"numFlags"]
# E(net)$anyFlags = edges[!is.na(edges$Appraiser_ID),"anyFlags"]
# plot(net, vertex.color=V(net)$Office, vertex.shape = c("circle","square")[V(net)$Role])
# save(net, file="InsuranceNetwork.Rdata")
# #install.packages("RColorBrewer")
# #library("RColorBrewer")
# pal3 = brewer.pal(7, "Set3")
#                    
# plot(net,vertex.size=10, vertex.color=pal3[V(net)$Office], vertex.shape = c("circle","square")[V(net)$Role], edge.color=c("black","red")[E(net)$anyFlags])
# # Legend doesn't work because the offices are out of order. Need to first determine order. ForceNetwork does this automatically
# #legend(x=-3, y=1, c("Braintree", "Plymouth", "Boston", "Brockton", "New Bedford", "Fall River", "Taunton"), pch=21, col="#777777",pt.bg=pal3, pt.cex=2, cex=.8, bty="n", ncol=1)
# 
# save(edges, file="Edgelist.Rdata")


load("Edgelist.Rdata")
load("Employees.Rdata")
load("Claims.Rdata")
load("InsuranceNetwork.Rdata")
Employees1=Employees
Employees1$size=1
# Source is Adjuster, Target is Appraiser.
edges$Adjuster_ID = edges$Adjuster_ID-1
edges$Appraiser_ID = edges$Appraiser_ID-1
forceNetwork(Links = edges, Nodes = Employees1, Source="Adjuster_ID", Target="Appraiser_ID", NodeID = "Employee_ID", Group = "Office", linkWidth = 1, Nodesize="size",
             linkColour = "#afafaf", fontSize=12, zoom=T, legend=T,opacity = 0.8)

