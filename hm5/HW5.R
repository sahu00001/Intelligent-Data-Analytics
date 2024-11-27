library(ggplot2)
library(caret)
library(tidyverse)
summary(housingData)
glimpse(housingData)
install.packages("caret")
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
#dummies<-dummyVars(housingData)

##checking columns with most of the na values
colSums(is.na(housingData))

## removing the columns with na values more than 50%
hd <- subset(housingData, select = -c(Alley, PoolQC, Fence, MiscFeature,Id, FireplaceQu))
hd

sort(colSums(is.na(hd)))

#Garage
hd <- hd %>% 
  mutate(GarageType = ifelse(is.na(GarageType), "Not present", GarageType))
hd <- hd %>% 
  mutate(GarageFinish = ifelse(is.na(GarageFinish), "Not present", GarageFinish))
hd <- hd %>% 
  mutate(GarageQual = ifelse(is.na(GarageQual), "Not present", GarageQual))
hd <- hd %>% 
  mutate(GarageCond = ifelse(is.na(GarageCond), "Not present", GarageCond))
hd <- hd %>% 
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt))
hd
#Basement
hd<- hd%>% mutate(BsmtQual = ifelse(is.na(BsmtQual), 'none', BsmtQual), BsmtCond = ifelse(is.na(BsmtCond), 'none', BsmtCond),BsmtExposure = ifelse(is.na(BsmtExposure), 'No', BsmtExposure),BsmtFinType1 = ifelse(is.na(BsmtFinType1), 'Unf', BsmtFinType1),BsmtFinType2 = ifelse(is.na(BsmtFinType2), 'Unf', BsmtFinType2))
hd
#Electrical
summary(factor(hd$Electrical))
hd[is.na(hd$Electrical), ]$Electrical <- "SBrkr"
#MasVnrType
hd <- hd %>% 
  mutate(MasVnrType = ifelse(is.na(MasVnrType), "None", MasVnrType))
#MasVnrArea
hd <- hd %>% 
  mutate(MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea))
#LotFrontage
pmm_imp <- mice(hd, m = 1, method = "pmm")
hd <- complete(pmm_imp)
sort(colSums(is.na(hd)))
View(hd)
##checking the structure of data
str(hd)


##to check the data
head(hd)
##o have an understanding of the various statistical features of our labels like mean, median, 1st Quartile value
summary(hd)

hd$SalePrice<-log(hd$SalePrice)
hd
##spliting the data into two sets 100 and 900
hd100<-head(hd,100)
hd100
hd900<-hd[101:1000,]
hd900
##olsmodelcalculation
Linearols1=lm(SalePrice~.,hd)
Linearols1
summary(Linearols1)
##plotols
plot(Linearols1)
##making prediction on test data
Predict1<-predict(Linearols1,hd100)
Predict1
##Calculating the RMSE
RMSE1<-RMSE(Predict1,hd100$SalePrice)
RMSE1

AIC(Linearols1)
BIC(Linearols1)
VIF(Linearols1)

install.packages("car")
library(car)
##Multiple linear regression
hd
##Multilinear OLS Regression
Linearols2 = lm(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF,hd)
Linearols2

summary(Linearols2)
##plotols
plot(Linearols2)
##making prediction on test data
Predict2<-predict(Linearols2,hd100)
Predict2
##Calculating the RMSE
RMSE2<-RMSE(Predict2,hd100$SalePrice)
RMSE2

AIC(Linearols2)
BIC(Linearols2)
VIF(Linearols2)
##pls
set.seed(123)
custom <- trainControl(method = "repeatedcv", number = 5, repeats = 5, verboseIter = T)
plsmodel<-train(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF,data=hd,method="pls",trControl = custom,tuneLength = 10)
plsmodel$results
plspred<-predict(plsmodel,hd)
RMSE(hd$SalePrice,predictpls)


summary(plsmodel)
plot(varImp(plsmodel, scale = F))
##lasso
set.seed(1234)
lasso <- train(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, 
               data = hd, method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001,1,length=5)), trControl = custom)

lassopred1 <- predict(lasso, hd)
RMSE(hd$SalePrice,lassopred1)
lasso$results
plot(varImp(lassomodel, scale = F))
plot(lasso)

plot(lasso$finalModel, xvar = "lambda", label = T)
plot(lasso$finalModel, xvar = "dev", label = T)
##Ridge Regression
set.seed(1234)
ridge <- train(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, 
               data = hd, method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001,1,length=5)), trControl = custom)
ridge <- train(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, 
               data = hd, method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0, lambda = 0.0001), trControl = custom)
ridge$results
ridgepred <- predict(ridge, hd)
RMSE(hd$SalePrice,ridgepred)

##Elasticnet
set.seed(124)
en<- train(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, 
            data = hd, method = 'glmnet',
            tuneGrid = expand.grid(alpha = seq(0,1,length=10), lambda = seq(0.0001,0.2,length=5)), trControl = custom)

en <- train(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, 
            data = hd, method = 'glmnet',
            tuneGrid = expand.grid(alpha = 0.1, lambda = 0.0001))

summary(en)
en$results
enpred <- predict(en, hd)
RMSE(hd$SalePrice,enpred)

##pcrmodel
pcrmodel <- pcr(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, data = hd900, scale=TRUE,5)
pcr_pred <- predict(model,hd100, ncomp = 4)
summary(model)
RMSEPcr<-RMSE(pcr_pred,hd100$SalePrice)
RMSEPcr
install.packages("earth")
library(earth)
##marsmo0del
set.seed(1234)
marsFit <- earth(SalePrice~OverallQual+GrLivArea+GarageArea+GarageCars+TotalBsmtSF+X1stFlrSF, 
                 data = hd,
                 degree=2,nk=49,pmethod="cv",nfold=5,ncross=5)
plot(marsFit)
predmars<- predict(marsFit, hd)
predmars
RMSEmars<-RMSE(hd$SalePrice,pred)
RMSEmars
marsFit



