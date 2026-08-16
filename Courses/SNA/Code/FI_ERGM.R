######################################################################
######################################################################
##########                   READ IN DATA                   ########## 
######################################################################
######################################################################
install.packages("ergm")
install.packages("statnet")
install.packages("network")
library("ergm","statnet","network")

setwd("/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Social Network Analysis 2017/Potential Data Sources/FI Consulting")

load("FIergmData.Rdata")

# Note this is now a "network" class object, no longer an "igraph" object. There are
# differences in how you set/list attributes. Unfortunately. You can still use the plot command though!
plot(FInet)
list.vertex.attributes(FInet)
FInet%v%"CollegeBasketball"
FInet%v%"NewVariable" = 1:71
######################################################################
#        FUNCTION TO CREATE TABLE OF ODDS RATIOS FOR A MODEL.        #
######################################################################
or = function(model){
  or = exp( model$coef )
  ste = sqrt( diag( model$covar ) )
  lci = exp( model$coef-1.96*ste )
  uci = exp( model$coef+1.96*ste )
  oddsratios = rbind( round( lci,digits = 4 ),round( or,digits = 4 ),round( uci,digits = 4 ) )
  oddsratios = t( oddsratios )
  colnames(oddsratios)=c( "Lower 95%","OR","Upper 95%" )
  return(oddsratios) }

######################################################################
######################################################################
##########                     NULL MODEL                   ########## 
######################################################################
######################################################################
#'  'edges' is the intercept term. As for change statistics, obviously
#'  if an edge is added to the model, it always changes the number of edges
#'  in the network by 1. The underlying data is a column of ones ==> Intercept.
#'  This gives the baseline odds of an edge between two nodes. The density of
#'  the graph is the baseline probability, so this coefficient is precisely
#'  log(density/[1-density]). (i.e. the log-odds).
nullmodel = ergm(FInet ~edges) 
summary(nullmodel)
exp(-2.38784)
p=network.density(FInet)
p/(1-p)
# One measure of goodness of fit is to see how the number of triangles in the  
# simulated networks compare to the number of triangles in the observed network.
simtri = simulate(nullmodel, nsim=100, monitor = ~triangles, statsonly=T,
                  control=control.simulate.ergm(MCMC.burnin=1000, MCMC.interval=1000),seed=7515)
obs.tri = summary(FInet ~triangle)

dev.off()
par( mar=c(4,4,1,1), cex.main = .9, cex.lab = .9, cex.axis = .75)
hist(simtri[,"triangle"],xlim=c(0,500),col='gray',main="",xlab = 'Number of Triangles', ylab = 'Number of Simulations')
points(obs.tri, 3, pch = "X", cex=2)

# Since this is the null model, where edges are generated at random with prob = density
# we can see that our observed network has slightly more triangles than you would expect at random.

######################################################################
######################################################################
##########                 MAIN EFFECTS MODEL               ########## 
######################################################################
######################################################################


model1 = ergm(FInet ~ edges + nodecov('Tenure') + nodecov('NumLeads')
              + nodefactor('CollegeBasketball')+nodefactor('CollegeFootball')+nodefactor('Gender'))
summary(model1)

# AIC and BIC improve. Residual Deviance (similar to -2*LogLikelihood) also improves.
# However, it appears we have an issue similar to Quasi-complete separation, where
# some of our parameter estimates are infinite. The factors of College Sports and Gender
# do not appear to be significant so we'll remove them. Tenure and NumLeads are likely
# highly correlated. So we can just remove Tenure.
#plot(FInet%v%'NumLeads'~FInet%v%'Tenure')

model1 = ergm(FInet ~ edges +  nodecov('NumLeads'))
summary(model1)

# Not much different on the standard goodness-of-fit measures, actually better on AIC because so many
# fewer variables in model. Let's see how the number of triangles compare:


simtri = simulate(model1, nsim=100, monitor = ~triangles, statsonly=T, control=control.simulate.ergm(MCMC.burnin=1000, MCMC.interval=1000),seed=7515)
dev.off()
par( mar=c(4,4,1,1), cex.main = .9, cex.lab = .9, cex.axis = .75)
hist(simtri[,"triangle"],xlim=c(0,max(simtri[,"triangle"])),col='gray',main="",xlab = 'Number of Triangles', ylab = 'Number of Simulations')
points(obs.tri, 3, pch = "X", cex=2)

# Better!
######################################################################
######################################################################
##########          MAIN EFFECTS + EDGE TERMS MODEL         ########## 
######################################################################
######################################################################


model2 = ergm(FInet ~ edges +nodecov('NumLeads')+ edgecov(Trust))
summary(model2)

simtri = simulate(model2, nsim=100, monitor = ~triangles, statsonly=T, control=control.simulate.ergm(MCMC.burnin=1000, MCMC.interval=1000))
dev.off()
par( mar=c(4,4,1,1), cex.main = .9, cex.lab = .9, cex.axis = .75)
hist(simtri[,"triangle"],xlim=c(0,max(simtri[,4])),col='gray',main="",xlab = 'Number of Triangles', ylab = 'Number of Simulations')
points(obs.tri, 3, pch = "X", cex=2)

# Even Better!
or(model2)

######################################################################
######################################################################
##########          MAIN EFFECTS + HOMOPHILY TERMS          ########## 
######################################################################
######################################################################


model3 = ergm(FInet ~ edges + nodecov('NumLeads') + edgecov(Trust)
              +nodematch('CollegeFootball')+nodematch('CollegeBasketball')+nodematch('Gender'))
summary(model3)

simtri = simulate(model3, nsim=100, monitor = ~triangles, statsonly=T, control=control.simulate.ergm(MCMC.burnin=1000, MCMC.interval=1000),seed=7515)
dev.off()
par( mar=c(4,4,1,1), cex.main = .9, cex.lab = .9, cex.axis = .75)
hist(simtri[,"triangle"],xlim=c(0,max(simtri[,"triangle"])),col='gray',main="",xlab = 'Number of Triangles', ylab = 'Number of Simulations')
points(obs.tri, 3, pch = "X", cex=2)

# This is really good!
or(model3)

######################################################################
######################################################################
##########     OTHER CONSIDERATIONS FOR GOODNESS OF FIT     ########## 
######################################################################
######################################################################

#' verbose = provide info on progress of simulation
model3_gof = gof(model3, GOF = ~ idegree +odegree + espartners + distance, 
                 verbose = F, burnin=10000, interval = 10000, seed=7515)
model3_gof$pval.ideg
#' First column is indegree. Second column is observed number of vertices with that indegree.
#' Then we have min/mean/max number of vertices across the simulations that have that indegree/
#' Finally the p-value is the proportion of simulated vertices that are at least as extreme
#' as the observed value. Large values => the simulated networks are similar to the observed network.
#' i.e. LARGE P-VALUES GOOD.
model3_gof$pval.odeg # doesn't model this feature quite as well.
model3_gof$pval.esp
#' The distribution of edgewise shared partners (over edges). In other words, from the table
#' we can see that 183 edges in our original network have no shared partners and 143 of the edges 
#' in our network have 1 shared partner. The simulated networks match this well.
model3_gof$pval.dist # definite room for improvement here. Too much connection in simulated networks. 
