##Project 6
##Author - Sujata Sahu
library(tidyverse) 
library(lubridate)
library(dplyr)
library(glmnet)
library(earth)
library(mltools)
library(caret)
library(pls)
library(dlookr)

##To find the numeric value of Traindata
TrainNumeric<- select_if(Train, is.numeric) %>% as_tibble()

TrainNumeric
diagnose(TrainNumeric)%>%arrange(desc(missing_percent))
##diagnosing the numeric variables
diagnose_numeric(Train)
##
diagnose_numeric(Train) %>% filter(minus>0|zero>0)
diagnose_category(Train)
diagnose_category(Train) %>% 
  filter(is.na(levels))
diagnose_outlier(Train)
##Numeric variables that contained outliers
diagnose_outlier(Train)%>%filter(outliers_cnt>0)
##finds a numeric variable with an outlier ratio of 5% or more, and
##then returns the result of dividing mean of outliers by overall mean in descending order
diagnose_outlier(Train) %>% 
  filter(outliers_ratio > 5) %>% 
  mutate(rate = outliers_mean / with_mean) %>% 
  arrange(desc(rate)) %>% 
  select(-outliers_cnt)
##visualisation
Train %>%
  plot_outlier(revenue) 
##visualisation
Train %>%
  plot_na_pareto()

###Data Preparation
##Checking the total number of na values present in each column of  the data by using the following command
colSums(is.na(Train))

#Changed Dates to R date format using lubridate package
#To lump the categorial variables used forcats package
##preprocessing the train Data
PreprocessTrainData<-Train %>% 
  mutate(date=ymd(date)) %>%                                              
  mutate(country = fct_lump(fct_explicit_na(country), n = 11)) %>%         
  mutate(medium = fct_lump(fct_explicit_na(medium), n = 5)) %>%
  mutate(browser = fct_lump(fct_explicit_na(browser), n = 4)) %>%
  mutate(operatingSystem = fct_lump(fct_explicit_na(operatingSystem), n = 2)) %>%
  group_by(custId) %>%                                   
  summarize(                                            
    channelGrouping = max(ifelse(is.na(channelGrouping) == TRUE, -9999, channelGrouping)),
    maxVisitNum = max(visitNumber, na.rm = TRUE),
    browser = first(browser),
    operatingSystem = first(operatingSystem),
    country = first(country),
    medium = first(medium),
    isTrueDirect = mean(ifelse(is.na(isTrueDirect) == TRUE, 0, 1)),
    bounce_sessions = sum(ifelse(is.na(bounces) == TRUE, 0, 1)),
    pageviews_sum = sum(pageviews, na.rm = TRUE),
    pageviews_mean = mean(ifelse(is.na(pageviews), 0, pageviews)),
    pageviews_min = min(ifelse(is.na(pageviews), 0, pageviews)),
    pageviews_max = max(ifelse(is.na(pageviews), 0, pageviews)),
    pageviews_median = median(ifelse(is.na(pageviews), 0, pageviews)),
    
  )
##preprocessing the test Data
PreprocesstestData <- Test %>% 
  mutate(date=ymd(date)) %>%                                              
  mutate(country = fct_lump(fct_explicit_na(country), n = 11)) %>%         
  mutate(medium = fct_lump(fct_explicit_na(medium), n = 5)) %>%
  mutate(browser = fct_lump(fct_explicit_na(browser), n = 4)) %>%
  mutate(operatingSystem = fct_lump(fct_explicit_na(operatingSystem), n = 2)) %>%
  group_by(custId) %>%                                   
  summarize(                                            
    channelGrouping = max(ifelse(is.na(channelGrouping) == TRUE, -9999, channelGrouping)),
    maxVisitNum = max(visitNumber, na.rm = TRUE),
    browser = first(browser),
    operatingSystem = first(operatingSystem),
    country = first(country),
    medium = first(medium),
    isTrueDirect = mean(ifelse(is.na(isTrueDirect) == TRUE, 0, 1)),
    bounce_sessions = sum(ifelse(is.na(bounces) == TRUE, 0, 1)),
    pageviews_sum = sum(pageviews, na.rm = TRUE),
    pageviews_mean = mean(ifelse(is.na(pageviews), 0, pageviews)),
    pageviews_min = min(ifelse(is.na(pageviews), 0, pageviews)),
    pageviews_max = max(ifelse(is.na(pageviews), 0, pageviews)),
    pageviews_median = median(ifelse(is.na(pageviews), 0, pageviews)),
    
  )

#finding the target variable
targetRevenue<-Train %>% 
  group_by(custId) %>%
  summarize(
    custRevenue = sum(revenue)
  ) %>% 
  mutate(logSumRevenue = log(custRevenue+1)) %>% dplyr::select(-custRevenue)

#Appending the preprocessed train Data withthe target revenue column
trainTransformed <- PreprocessTrainData %>% inner_join(targetRevenue, by = "custId")

#olsModel
Linearols<-lm(logSumRevenue ~ 
                channelGrouping + operatingSystem+medium+
                maxVisitNum+ browser +
                country + 
                bounce_sessions + bounce_sessions*pageviews_sum +
                pageviews_sum +pageviews_mean + pageviews_min + 
                pageviews_median, 
              data = trainTransformed)
olsModelPred <- predict(Linearols, PreprocesstestData)
summary(Linearols)
plot(Linearols)
Linearols$results
##To make the prediction on test data
Predictols<-predict(Linearols,PreprocesstestData)
Predictols

#mars model
marsModel <- earth(logSumRevenue ~ 
                   channelGrouping + operatingSystem+medium+
                   maxVisitNum+ browser +
                   country + 
                   bounce_sessions + bounce_sessions*pageviews_sum +
                     pageviews_sum +pageviews_mean + pageviews_min + 
                       pageviews_median, 
                 data = trainTransformed,
                 degree=2,nk=49,pmethod="cv",nfold=10,ncross=10)

plot(marsModel)

marsModelPred <- predict(marsFit, PreprocesstestData)
rmse(trainTransformed$logSumRevenue,marsModelPred)
#control prameters
custom <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = T)

#Ridge Regression Model
set.seed(1234)
rR <- train(logSumRevenue ~ 
              channelGrouping + operatingSystem+medium+
              maxVisitNum+ browser +
              country + 
              bounce_sessions + bounce_sessions*pageviews_sum +
              pageviews_sum +pageviews_mean + pageviews_min + 
              pageviews_median, 
            data = trainTransformed, method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0:1, lambda = c(0.00001, 0.0001, 0.001,0.01)), trControl = custom)
rR$finalModel
rR$results
plot(rR)

# To generate results
predrR <- predict(rR, PreprocesstestData)
predrR[predrR < 0] <- 0
finalSubmissions <- data.frame(custId=PreprocesstestData$custId,predRevenue=predrR)
View(finalSubmissions)
write.csv(finalSubmissions, "Submission.csv", row.names = F)
summary(rR)
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = "dev", label = T)
plot(varImp(rR, scale = F))

#Lasso regression Model
set.seed(1234)
lassoModel <- train(logSumRevenue ~ 
                      channelGrouping + operatingSystem+medium+
                      maxVisitNum+ browser +
                      country + 
                      bounce_sessions + bounce_sessions*pageviews_sum +
                      pageviews_sum +pageviews_mean + pageviews_min + 
                      pageviews_median, 
                    data = trainTransformed, method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001,1,length=5)), trControl = custom)

lassoModel$results
plot(lassoModel)
plot(lasso$finalModel, xvar = "lambda", label = T)
plot(lasso$finalModel, xvar = "dev", label = T)
plot(varImp(lassoModel, scale = F))
LassoPredict <- predict(lassoModel,PreprocesstestData )

# To generate results
predrR <- predict(rR, PreprocesstestData)
predrR[predrR < 0] <- 0
finalSubmissions <- data.frame(custId=PreprocesstestData$custId,predRevenue=predrR)
View(finalSubmissions)
write.csv(finalSubmissions, "Submission.csv", row.names = F)
