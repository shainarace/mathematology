load("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis/Code/FIgraph.Rdata")


##################################################################
##################################################################
########### Create Adjacency Matrix for Sports Teams #############
##################################################################
##################################################################
# Create upper triangular portion since it is symmetric
 
Football=matrix(0,71,71)
for (i in 1:70){ for (j in (i+1):71) {
  Football[i,j]=as.numeric(Employees$CollegeFootball[i]==Employees$CollegeFootball[j])
}}
# Fill in the lower triangle of the matrix by adding the transpose
Football=Football+t(Football)
##################################################################
##################################################################
# Is there any relationship regarding Football and Trust? 

chisq.test(c(Football),c(Trust))
observed = chisq.test(c(Football),c(Trust))
##################################################################
##################################################################
# Let's simulate this situation 1000 times, renaming the individuals in
# the Football network to see what the distribution of the chi-squared
# statistic looks like under the null hypothesis that there is no 
# relationship 
##################################################################
##################################################################
SimulatedValues = vector()
for (sim in 1:1000){
  randperm = sample(1:71,71,replace=F,prob=NULL)
  SimFootball = Football[randperm,randperm]
  chisq = chisq.test(c(SimFootball),c(Trust))
  SimulatedValues[sim] = chisq$statistic
}
hist(SimulatedValues,xlim=c(0,20),col='gray',main="",xlab = 'Chi-Squared Statistic', ylab = 'Number of Simulations')
points(observed$statistic, 0, pch = "X", cex=2)
# Now calculate the true p-value as the proportion of simulations >= observed statistic
cat('p-value for Football/Trust association', sum(SimulatedValues>=observed$statistic)/1000)
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
# What about for Football and invitation to join a project?
Invites = as.matrix(get.adjacency(FIgraph))
chisq.test(c(Football),c(Invites))
observed=chisq.test(c(Football),c(Invites))
##################################################################
##################################################################
# Let's simulate this situation 1000 times, renaming the individuals 
# the Football network to see what the distribution of the chi-squared
# statistic looks like under the null hypothesis that there is no 
# relationship 
##################################################################
##################################################################
SimulatedValues = vector()
for (sim in 1:1000){
  randperm = sample(1:71,71,replace=F,prob=NULL)
  SimFootball = Football[randperm,randperm]
  chisq = chisq.test(c(SimFootball),c(Invites))
  SimulatedValues[sim] = chisq$statistic
}
hist(SimulatedValues,xlim=c(0,20),col='gray',main="",xlab = 'Chi-Squared Statistic', ylab = 'Number of Simulations')
points(observed$statistic, 0, pch = "X", cex=2)
cat('p-value for Football/Invites association', sum(SimulatedValues>=observed$statistic)/1000)

table(c(Football),c(Invites))
# Calculate an odds ratio...
(108/804)/(310/3819)
##################################################################

