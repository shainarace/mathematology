
install.packages('earth')
install.packages("mgcv")
install.packages("gam")
library(earth)
library(mgcv)
library(splines)
library(gam)

###################################################################################################
# LOAD DATA, CREATE TRAINING/VALIDATION SPLIT
###################################################################################################
path='/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Data Mining 2020/Code'
load(paste(path,"/concrete.RData",sep=''))
set.seed(7515)
train = sample(c(T,F),nrow(concrete),replace=T, p=c(0.8,0.2))
###################################################################################################
# PIECEWISE LINEAR FUNCTIONS (LINEAR SPLINES)
###################################################################################################
mars1 = earth(strength ~ .,  data = concrete[train,], degree=1)  
print(mars1)
# Training Error
pred = predict(mars1,concrete[train,])
mean((pred-concrete[mars1,'strength'])^2)
# Validation Error
pred = predict(mars1,concrete[!train,])
mean((pred-concrete[!train,'strength'])^2)
###################################################################################################
# PIECEWISE CUBIC FUNCTIONS (CUBIC SPLINES)
# second argument in bs() function is the desired degrees of freedom for the variable, which 
# effectively specifies the number of knots in the spline.
###################################################################################################
cubic1 = lm(strength ~
                bs(cement      ,3)+
                bs(slag        ,3)+
                bs(ash         ,3)+
                bs(water       ,3)+
                bs(superplastic,3)+
                bs(fineagg     ,6)+
                bs(age         ,6), data = concrete[train,])
  

summary(cubic1)
#Training Error
pred = predict(cubic1,concrete[train,])
mean((pred-concrete[train,'strength'])^2)
#Validation Error
pred = predict(cubic1,concrete[!train,])
mean((pred-concrete[!train,'strength'])^2)
###################################################################################################
# SMOOTHING SPLINES 
###################################################################################################
smoothing1 = mgcv::gam(strength ~
              s(cement      )+
              s(slag        )+
              s(ash         )+
              s(water       )+
              s(superplastic)+
              s(fineagg     )+
              s(age         ), data = concrete[train,])

summary(smoothing1)
#Training Error
pred = predict(smoothing1,concrete[train,])
mean((pred-concrete[train,'strength'])^2)
#Validation Error
pred = predict(smoothing1,concrete[!train,])
mean((pred-concrete[!train,'strength'])^2)

###################################################################################################
###################################################################################################
# CLASSIFICATION MODEL EXAMPLES USING THE CUSTOMER CHURN DATA FROM A CELL PHONE COMPANY
###################################################################################################
###################################################################################################
# Linear Splines (MARS)
load("churn.Rdata")
mars2 = earth(churn ~ . - area_code,  data = churnTrain, glm=list(family=binomial), degree=1)  
print(mars2)
mean(((mars2$fitted.values<0.5) == (churnTrain$churn=='yes'))) #Training Accuracy
pred=predict(mars2, churnTest, type = 'class')
mean(pred == churnTest$churn) #Test Accuracy
###################################################################################################
# For baseline comparison, a logistic model with no splines:
###################################################################################################
lm1 = glm(churn ~ . - area_code,  data = churnTrain,family='binomial')
# Test Accuracy
pred=predict(lm1, churnTest, type = 'response')
mean((pred>0.5) == (churnTest$churn=='no')) 
###################################################################################################
# Smoothing Splines from the GAM package
###################################################################################################
smoothing1 = mgcv::gam(churn ~
                         s(account_length      )+
                         s(total_eve_charge        )+
                         s(total_eve_calls         )+
                         s(total_eve_minutes       )+
                         s(total_day_charge    )+
                         s(total_day_calls     )+
                         s(total_day_minutes   )+
                         s(total_night_charge    )+
                         s(total_night_calls     )+
                         s(total_night_minutes   )+
                         s(total_intl_charge    )+
                         s(total_intl_calls     )+
                         s(total_intl_minutes   )+
                         s(number_customer_service_calls)+
                         international_plan+
                         state, data = churnTrain, family = binomial(link='logit'))

summary(smoothing1)
#Training Accuracy
# (The event in this model is churn=='no')
mean((smoothing1$fitted.values>0.5)==(churnTrain$churn=='no'))
#Validation Accuracy
pred = predict(smoothing1,churnTest,type='response')
mean((pred>0.5)==(churnTest$churn=='no'))
###################################################################################################