#install.packages(c("rlist","gridExtra","gbm", "mgcv", "pdp", "ALEPlot", "lime", "iml"))
#install.packages('h2o')
library(h2o)
library(MASS)
library("ggplot2")
library("tm")
library("tidyverse")
library("rpart")
library("rpart.plot")
library("rlist")
library("gridExtra")
library("randomForest")
library("gbm")
library("mgcv")
library("pdp")
library("ALEPlot")
library("lime")
library("iml")
library(e1071) # Where the naiveBayes function is found
library(gmodels) # Where the CrossTable function is found
data("Boston", package = "MASS")
################################################################
# VARIABLES IN THE DATA
################################################################
#' crim --  per capita crime rate by town.
#' zn --  proportion of residential land zoned for lots over 25,000 sq.ft.
#' indus --  proportion of non-retail business acres per town.
#' chas --  Charles River dummy variable (= 1 if tract bounds river; 0 otherwise). nox nitrogen oxides concentration (parts per 10 million).
#' rm --  average number of rooms per dwelling.
#' age --  proportion of owner-occupied units built prior to 1940.
#' dis --  weighted mean of distances to five Boston employment centres. rad index of accessibility to radial highways.
#' tax --  full-value property-tax rate per \$10,000.
#' ptratio --  pupil-teacher ratio by town.
#' black --  1000(Bk − 0.63)2 where Bk is the proportion of blacks by town. lstat lower status of the population (percent).
#' medv -- median value of owner-occupied homes in \$1000s.
################################################################
train = sample(c(T,F),nrow(Boston),replace=T,p=c(0.75,0.25))
################################################################
# MAKE DEFAULT RANDOM FOREST, CHECK PERFORMANCE ON VALIDATION
################################################################
forest = randomForest(nox~.,data=Boston[train,])
pred.rf = predict(forest,Boston[!train,])
(mape.forest = mean(abs(pred.rf-boston[!train,"nox"])/abs(boston[!train,"nox"])))

################################################################
# Linear Model for Comparison
################################################################
f = lm(nox~., data=Boston[train,])
pred.lm = predict(f,Boston[!train,] )
(mape.lm = mean(abs(pred.lm-boston[!train,"nox"])/abs(boston[!train,"nox"])))

################################################################
# Feature Importance - Forest
# randomly permute each column, see how much worse that makes the model. More worse => More important
X = as.matrix(Boston[train, !names(Boston) %in% c("nox")])
forest_predictor = Predictor$new(forest, 
                                 data = as.data.frame(X), 
                                 y = Boston[train,"nox"], 
                                 type = "response")
imp = FeatureImp$new(forest_predictor, loss = "mse")
plot(imp) + theme_bw()
################################################################
# Compare with version from Random Forest package
varImpPlot(forest) 
################################################################
# Feature Importance: linear model
linear_predictor = Predictor$new(f, 
                                 data = as.data.frame(X), 
                                 y = Boston[train,"nox"], 
                                 type = "response")
imp = FeatureImp$new(linear_predictor, loss = "mse")
plot(imp) + theme_bw()
################################################################
#' Individual Conditional Expectation (ICE) Plots 
#'
set.seed(13)
pdps = FeatureEffects$new(forest_predictor, method='ice')
pdps$plot()  #All charts
pdps$plot(c("dis")) #Subset of Charts
################################################################
#' Partial Dependence Plots - Linear Model 
#' They're linear! Surprise, no surprise.
################################################################
pdp = FeatureEffect$new(linear_predictor, feature = 1, method = "pdp")
p1= pdp$plot() + theme_bw()
pdp = FeatureEffect$new(linear_predictor, feature = 2, method = "pdp")
p2= pdp$plot() + theme_bw()
grid.arrange(p1,p2)
summary(f)
################################################################
#' Partial Dependence Plots - Forest Model
#' Calculate for all effects in model with FeatureEffects()
#' Calculate for a single effects in model with FeatureEffect()
################################################################
set.seed(11)
pdps = FeatureEffects$new(forest_predictor, method='pdp')
pdps$plot()  #All charts
pdps$plot(c("tax")) #Subset of Charts
################################################################
#' ICE + PDP in same chart Plots 
#'
set.seed(13)
pdps = FeatureEffects$new(forest_predictor, method='pdp+ice')
pdps$plot()  #All charts
pdps$plot(c("tax")) #Subset of Charts
################################################################
#' Acculumated Local Effects (ALE) Plots 
#'
set.seed(13)
ale = FeatureEffects$new(forest_predictor, method='ale')
ale$plot()  #All charts
ale$plot(c("tax","age")) #Subset of Charts
ale2d = FeatureEffect$new(forest_predictor, 
                          feature = c("age","tax"), 
                          method = "ale")
ale2d$plot()
ale2d = FeatureEffect$new(forest_predictor, 
                          feature = c("age","medv"), 
                          method = "ale")
ale2d$plot()
################################################################
#' LIME Package requires some H2O nonsense - In order to  apply
#' LIME to a model that is not supported, you have to first define 
#' a predict_model function and a model_type function. Turns out
#' this isn't too hard to hack - you can easily change the functions
#' below to adapt to a model of your choice.
################################################################
################################################################
predict_model.randomForest <- function(x, newdata, ...) {
  # Function performs prediction and returns data frame with Response
  pred <- predict(x, newdata)
  return(as.data.frame(pred))
}
model_type.randomForest <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  
  return("regression")
}
predict_model.randomForest(forest,Boston[1:5,])
model_type.randomForest(forest,Boston[1:5,])

################################################################
#' LIME explanations for the first 4 observations in the data
################################################################
explainer = lime(Boston[train,!names(Boston) %in% c("nox")],forest)
explanation= lime::explain(Boston[1:4,!names(Boston) %in% c("nox")], explainer,n_features=ncol(Boston)-1)
plot_features(explanation, ncol = 2) 

################################################################
################################################################
# Shapley explanations
################################################################

point=37
shapley = Shapley$new(forest_predictor, x.interest = Boston[point,-5])
g1 = shapley$plot() + theme_bw() + ggtitle("Observation 37")

point=265
shapley = Shapley$new(forest_predictor, x.interest = Boston[point,-5])
g2 =shapley$plot() + theme_bw() + ggtitle("Observation 265")
grid.arrange(g1,g2)
################################################################
# Compare to LIME
################################################################
explanation= lime::explain(Boston[c(37,265),!names(Boston) %in% c("nox")], explainer,n_features=ncol(Boston)-1)
plot_features(explanation, ncol = 2) 

