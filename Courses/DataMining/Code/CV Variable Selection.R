housing = read.csv('http://www4.ncsu.edu/~slrace/DataMining2018/Code/ameshousing.csv')
#########################################################################
#########################################################################
###############       DATA PREP FOR CV AND MODELING        ##############
#########################################################################
#########################################################################
# Keep only the columns we want to use for prediction
# Retain only complete cases.
housing=housing[,c("SalePrice", "Basement_Area", "Age_Sold", "Garage_Area",
                    "Overall_Qual", "Overall_Cond", "Central_Air",
                    "Gr_Liv_Area", "Lot_Area", "Fireplaces", 
                    "Bedroom_AbvGr", "House_Style",
                    "Heating_QC", "Masonry_Veneer",
                    "Full_Bathroom", "Half_Bathroom", "Deck_Porch_Area",
                    "Season_Sold")]
housing=housing[complete.cases(housing), ]
housing[,"Season_Sold"]=factor(housing[,"Season_Sold"])

set.seed=45
test.index = sample(c(T,F), 2928, p=c(.25,.75), replace=T)
housing.test = housing[test.index,]
housing.train = housing[!test.index,]

# Assign each observation to a CV Fold (10 Folds). 
housing.train$CVfold = sample(1:10, nrow(housing.train), replace=T)

library(leaps)
#########################################################################
#########################################################################
###############       PREDICT.REGSUBSETS() FUNCTION        ##############
#########################################################################
#########################################################################
#' The stepwise seletion procedures in regsubsets() function from the leaps 
#' library do not have a corresponding predict() function.
#'  So we will write our own here:
predict.regsubsets = function(output.object, newdata,nvars){
  eqn = as.formula(output.object$call[[2]]) # outputs formula as y ~ x1+x2+...etc
  data.matrix = model.matrix(eqn, newdata) # creates model matrix X as in y = X*beta
  coefi = coef(output.object, id=nvars)
  xvars = names(coefi)
  data.matrix[,xvars]%*%coefi # creates predictions y_hat = X*beta
}

#########################################################################
#########################################################################
#################        BEST SUBSET SELECTION         ##################
#########################################################################
#########################################################################
val.errors = matrix(NA,10,19)
for(fold in 1:10) {
  regfit.best = regsubsets(SalePrice~.,data=housing.train[housing.train$CVfold!=fold, ],nvmax=19)
  for (nvar in 1:19){
  pred = predict.regsubsets(regfit.best, housing.train[housing.train$CVfold==fold, ], nvar)
  val.errors[fold,nvar] = sqrt(mean((housing.train[housing.train$CVfold==fold,"SalePrice" ] - pred)^2))
  }
  }
avg.val.errors = colMeans(val.errors)
plot(avg.val.errors, ylab = 'Average MSE across 10 CV folds', xlab = 'Number of Variables in Model', main='Cross Validation Summary - Best Subset Selection')
abline(h = min(avg.val.errors)+sd(avg.val.errors))
plot(5:19,avg.val.errors[5:19],, ylab = 'Average RMSE across 10 CV folds', xlab = 'Number of Variables in Model', main='Cross Validation Summary - Best Subset  Selection (Plot Zoom)')
#' Forward subset selection indicates 11 variables is the best balance between 
#' bias and variance. It is justifiable to take a simpler model within one 
#' standard error of that minimum RMSE value. In this case 5-11 variables.
# Final model to send to testing phase:
final.best = regsubsets(SalePrice~.,data=housing.train,nvmax=19)

#########################################################################
#########################################################################
####################        STEPWISE SELECTION        ###################
#########################################################################
#########################################################################
val.errors = matrix(NA,10,19)
for(fold in 1:10) {
  regfit.step = regsubsets(SalePrice~.,data=housing.train[housing.train$CVfold!=fold, ],nvmax=19,method="seqrep")
  for (nvar in 1:19){
    pred = predict.regsubsets(regfit.step, housing.train[housing.train$CVfold==fold, ], nvar)
    val.errors[fold,nvar] = sqrt(mean((housing.train[housing.train$CVfold==fold,"SalePrice" ] - pred)^2))
  }
}
avg.val.errors = colMeans(val.errors)
plot(avg.val.errors, ylab = 'Average MSE across 10 CV folds', xlab = 'Number of Variables in Model', main='Cross Validation Summary - Stepwise Selection')
abline(h = min(avg.val.errors)+sd(avg.val.errors))
plot(5:19,avg.val.errors[5:19],, ylab = 'Average RMSE across 10 CV folds', xlab = 'Number of Variables in Model', main='Cross Validation Summary - Stepwise Selection (Plot Zoom)')
#' Stepwise subset selection indicates 11 variables is the best balance between
#' bias and variance. It is justifiable to take a simpler model within one 
#' standard error of that minimum RMSE value. In this case 6-11 variables.
# Final model to send to testing phase:
final.stepwise = regsubsets(SalePrice~.,data=housing.train,nvmax=17,method="seqrep")
#########################################################################
final.SAS = lm(SalePrice~ Basement_Area+Age_Sold+Garage_Area+Overall_Qual+
                 Overall_Cond+Gr_Liv_Area+Lot_Area+Bedroom_AbvGr+Fireplaces+
                 Full_Bathroom+Deck_Porch_Area+Central_Air+Heating_QC, data=housing.train)
#########################################################################
#########################################################################
#############       FINAL COMPARISON ON TEST DATA         ###############
#########################################################################
#########################################################################
pred.best = predict.regsubsets(final.best,housing.test,16)
(rmse.best = sqrt(mean((housing.test[,"SalePrice" ] - pred.best)^2)))
pred.stepwise = predict.regsubsets(final.stepwise, housing.test,16)
(rmse.s = sqrt(mean((housing.test[,"SalePrice" ] - pred.stepwise)^2)))
pred.sas = predict(final.SAS, housing.test)
(rmse.sas = sqrt(mean((housing.test[,"SalePrice" ] - pred.sas)^2)))
# Note the way that regsubsets() deals with categorical variables!!