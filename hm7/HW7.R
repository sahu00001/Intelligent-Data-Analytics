#Group-8
#homework7

library(tidyverse)
library(rpart)
library(MLmetrics)
library(randomForest)
library(caret)
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(C50)
library(earth)
library(neuralnet)
library(pROC)


hosData <- hm7train

# Stuff to skip ---------------------------
colSums(is.na(hosData))
table(hosData$num_lab_procedures)
# I don't think trying to fill in payer code will be helpful, at least right now
table(hosData$payer_code, hosData$readmitted)
table(hosData$readmitted[is.na(hosData$payer_code)])

numData <- hosData %>% select(where(is.numeric))
colnames((numData))

# I only want to look at the actually numeric data, not
# ordinal / nominal values
numData <- subset(numData, select = -c(patientID, admission_type,
                              discharge_disposition, admission_source,
                              readmitted))

# Remove the few incomplete cases in the name of a heatmap
numData <- numData[complete.cases(numData),]
correl <- cor(numData)
correl
heatmap(correl)
# Main idea: time in hospital correlates w/ num medications / procedures

# Preliminary analysis on specific variables
# Check some variables to see if any look good on their own
table(hosData$race, hosData$readmitted) # not really
table(hosData$gender, hosData$readmitted) # not really
table(hosData$age, hosData$readmitted)
# Intuitively, age looks a little better than the others
table(hosData$admission_type, hosData$readmitted)
# There might be some small differences on type of admission, but not huge
table(hosData$discharge_disposition, hosData$readmitted)
# Again, a little bit of distinguishing power; probably will see more with interactions?
table(hosData$number_diagnoses, hosData$readmitted)
# Definitely see odds of readmission increase with num diagnoses

table(hosData$medical_specialty, hosData$readmitted)
# Maybe? If I choose some good factor groupings?

table(hosData$num_medications, hosData$readmitted)
# Again, definitely a trend between number of medications and readmission rates

newFit <- glm(data = hosData, readmitted ~ num_medications + number_diagnoses, 
              family = "binomial")
summary(newFit)
# minimal interaction effect here; only priamry effects are significant

ageFit <- glm(data = hosData, readmitted ~ age + number_diagnoses, 
              family = "binomial")
summary(ageFit)

# Factor Collapsing ----------------------------
# Modify incorrect values on "indicator level"
hosData$indicator_level[hosData$indicator_level == 999] <- NA
hosData$indicator_level[hosData$indicator_level == -869] <- NA
# Indicator level doesn't seem to have much predictive power


# Discharge level first
hosData$discharge_disposition <- as.factor(hosData$discharge_disposition)
print(fct_count(hosData$discharge_disposition), n = 22)

hosData$discharge_disposition <- fct_collapse(hosData$discharge_disposition,
                      home = c('1'),
                      moreCare = c('2','3','4','5','8','9','10','14','15','16',
                                   '17','22','23','24','27','28'),
                      AMA = c('7'),
                      homeHealth = c('6','12','13'),
                      other_level = c('25')
                      )

# Now admission types / sources
hosData$admission_type <- as.factor(hosData$admission_type)
fct_count(hosData$admission_type)
hosData$admission_type <- fct_collapse(hosData$admission_type,
                          ER = c('1'),
                          Urgent = c('2'), 
                          Elec = c('3'),
                          Newborn = c('4'),
                          Trauma = c('7'),
                          other = c('5','6','8'))

# Sources
hosData$admission_source <- as.factor(hosData$admission_source)
fct_count(hosData$admission_source)
hosData$admission_source <- fct_collapse(hosData$admission_source,
                            ER = c('7','13'),# ER + birth complications
                            birth = c('11', '14'),
                            refer = c('1','2','3'),
                            transfer = c('4','5','6','10','22','25'),
                            other = c('9','17','8', '20')
                            )
colSums(is.na(hosData))

# Diagnoses ---------------------------------------- 

hosData$diagnosis[is.na(hosData$diagnosis)] <- 428

# I've gotta collapse these factors somehow
newDiag <- hosData$diagnosis
newDiag[(newDiag >= 249) & (newDiag < 251)] <- 250 # Diabetes
newDiag[(newDiag>= 240) & (newDiag < 247)] <- 240 # thyroid
newDiag[is.na(newDiag)] <- 428 # 428 = heart disease
newDiag[(newDiag >= 520) & (newDiag < 580)] <- 550 # Digestion
newDiag[(newDiag >= 390) & (newDiag < 460)] <- 428 # Heart disease
newDiag[(newDiag >= 460) & (newDiag < 520)] <- 480 # Respiration
newDiag[(newDiag >= 320) & (newDiag < 390)] <- 350 # Nervous system
newDiag[(newDiag >= 1) & (newDiag < 140)] <- 1 # infectious
newDiag[(newDiag >= 740) & (newDiag < 780)] <- 760 # genetic / birth
newDiag[(newDiag >= 140) & (newDiag < 240)] <- 160 # Neoplasms
newDiag[(newDiag >= 251) & (newDiag < 280)] <- 270 # Endocrine
newDiag[(newDiag >= 580) & (newDiag < 630)] <- 600 # Genito-urinary
newDiag[(newDiag >= 710) & (newDiag < 740)] <- 730 # Musculo
newDiag[(newDiag >= 780) & (newDiag <= 999)] <- 930 # Misc. / injury
newDiag[(newDiag >= 630) & (newDiag < 680)] <- 650 # childbirth
newDiag[(newDiag >= 680) & (newDiag < 710)] <- 695 # Skin
newDiag[(newDiag >= 290) & (newDiag < 320)] <- 300 # Mental
newDiag[(newDiag >= 280) & (newDiag < 390)] <- 285 # Blood
newDiag[newDiag == "V57"] <- 57 # Rehab

my_entries <- c('E909', 'V25',  'V26', 'V43', 'V45',  'V51', 'V53',   
                'V54', 'V55', 'V56', 'V58', 'V63', 'V66', 'V67', 'V71')
for (i in my_entries) {
  newDiag[newDiag == i] <- 930
}

hosData$diagnosis <- newDiag
hosData$diagnosis <- as.factor(hosData$diagnosis)

# Missing data work---------------------------
# Let's look at medical specialty
miss_spec <- hosData$admission_source[is.na(hosData$medical_specialty)]
fct_count(miss_spec)

levels(hosData$medical_specialty) = c(levels(hosData$medical_specialty),
                                      "ER", "other")

hosData$medical_specialty[hosData$admission_source == 'ER'] <- 'ER'
hosData$medical_specialty[is.na(hosData$medical_specialty)] <- "other"

# Dealing with remaining missing values:
table(hosData$gender) # assign missing to female (I don't think this'll matter)
hosData$gender[is.na(hosData$gender)] <- 'female'

table(hosData$race) # missing race assign to other
hosData$race[is.na(hosData$race)] <- 'Other'

table(hosData$time_in_hospital) # missing assign to 3
hosData$time_in_hospital[is.na(hosData$time_in_hospital)] <- 3

table(hosData$indicator_level) # assign missing to 50
hosData$indicator_level[is.na(hosData$indicator_level)] <- 50

# age
table(hosData$age) # guess at 50-60 years old
hosData$age[is.na(hosData$age)] <- '[50-60)'

# Payer code
fct_count(hosData$payer_code[hosData$admission_source == 'ER'])
fct_count(hosData$admission_source[is.na(hosData$payer_code)])

# number of labs
hosData$num_lab_procedures[is.na(hosData$num_lab_procedures)] <- 1

# Dataset creation  ----------------------------------------
#First thing, figuring out what's important

# with medications
Meds <- hosData[,-c(9,1)]

#without medications
noMeds <- hosData[,-(22:44)]
# Also, remove payer code because I don't have a good interpolation idea
noMeds <- noMeds[,-c(9,1)] # and get rid of patient ID


# TestData Prep -------------------------------
hosDataTest <- hm7test

# Factor Collapsing 
# Discharge level first
hosDataTest$discharge_disposition <- as.factor(hosDataTest$discharge_disposition)
print(fct_count(hosDataTest$discharge_disposition), n = 22)

hosDataTest$discharge_disposition <- fct_collapse(hosDataTest$discharge_disposition,
                                                  home = c('1'),
                                                  moreCare = c('2','3','4','5','8','9','10','14','15','16',
                                                               '17','22','23','24','27','28'),
                                                  AMA = c('7'),
                                                  homeHealth = c('6','12','13'),
                                                  other_level = c('25')
)

# Now admission types / sources
hosDataTest$admission_type <- as.factor(hosDataTest$admission_type)
fct_count(hosDataTest$admission_type)
hosDataTest$admission_type <- fct_collapse(hosDataTest$admission_type,
                                           ER = c('1'),
                                           Urgent = c('2'), 
                                           Elec = c('3'),
                                           Newborn = c('4'),
                                           Trauma = c('7'),
                                           other = c('5','6','8'))

# Sources
hosDataTest$admission_source <- as.factor(hosDataTest$admission_source)
fct_count(hosDataTest$admission_source)
hosDataTest$admission_source <- fct_collapse(hosDataTest$admission_source,
                                             ER = c('7','13'),# ER + birth complications
                                             birth = c('11', '14'),
                                             refer = c('1','2','3'),
                                             transfer = c('4','5','6','10','22','25'),
                                             other = c('9','17','8', '20')
)
colSums(is.na(hosDataTest))

# Diagnoses

# Figure out the most frequently occurring diagnosis (heart disease)
hosDataTest$diagnosis[is.na(hosDataTest$diagnosis)] <- "428"

# I've gotta collapse these factors somehow
newDiag <- hosDataTest$diagnosis
newDiag[(newDiag >= 249) & (newDiag < 251)] <- 250 # Diabetes
newDiag[(newDiag>= 240) & (newDiag < 247)] <- 240 # thyroid
newDiag[is.na(newDiag)] <- 428 # 428 = heart disease
newDiag[(newDiag >= 520) & (newDiag < 580)] <- 550 # Digestion
newDiag[(newDiag >= 390) & (newDiag < 460)] <- 428 # Heart disease
newDiag[(newDiag >= 460) & (newDiag < 520)] <- 480 # Respiration
newDiag[(newDiag >= 320) & (newDiag < 390)] <- 350 # Nervous system
newDiag[(newDiag >= 1) & (newDiag < 140)] <- 1 # infectious
newDiag[(newDiag >= 740) & (newDiag < 780)] <- 760 # genetic / birth
newDiag[(newDiag >= 140) & (newDiag < 240)] <- 160 # Neoplasms
newDiag[(newDiag >= 251) & (newDiag < 280)] <- 270 # Endocrine
newDiag[(newDiag >= 580) & (newDiag < 630)] <- 600 # Genito-urinary
newDiag[(newDiag >= 710) & (newDiag < 740)] <- 730 # Musculo
newDiag[(newDiag >= 780) & (newDiag <= 999)] <- 930 # Misc. / injury
newDiag[(newDiag >= 630) & (newDiag < 680)] <- 650 # childbirth
newDiag[(newDiag >= 680) & (newDiag < 710)] <- 695 # Skin
newDiag[(newDiag >= 290) & (newDiag < 320)] <- 300 # Mental
newDiag[(newDiag >= 280) & (newDiag < 390)] <- 285 # Blood
newDiag[newDiag == "V57"] <- 57 # Rehab

new_entries <- c('E909', 'V25',  'V26', 'V43', 'V45',  'V51', 'V53',   
                'V54', 'V55', 'V56', 'V58', 'V63', 'V66', 'V67', 'V71', 'V60',
                'V70', 'V07')
for (i in new_entries) {
  newDiag[newDiag == i] <- 930
}

hosDataTest$diagnosis <- newDiag
hosDataTest$diagnosis <- as.factor(hosDataTest$diagnosis)

# Missing data work
# Let's look at medical specialty
miss_spec <- hosDataTest$admission_source[is.na(hosDataTest$medical_specialty)]
fct_count(miss_spec)

levels(hosDataTest$medical_specialty) = c(levels(hosDataTest$medical_specialty),
                                          "ER", "other")

hosDataTest$medical_specialty[hosDataTest$admission_source == 'ER'] <- 'ER'
hosDataTest$medical_specialty[is.na(hosDataTest$medical_specialty)] <- "other"

# Dealing with remaining missing values:
table(hosDataTest$gender) # assign missing to female (I don't think this'll matter)
hosDataTest$gender[is.na(hosDataTest$gender)] <- 'female'

table(hosDataTest$race) # missing race assign to other
hosDataTest$race[is.na(hosDataTest$race)] <- 'Other'

table(hosDataTest$time_in_hospital) # missing assign to 3
hosDataTest$time_in_hospital[is.na(hosDataTest$time_in_hospital)] <- 3

table(hosDataTest$indicator_level) # assign missing to 50
hosDataTest$indicator_level[is.na(hosDataTest$indicator_level)] <- 50

# age
table(hosDataTest$age) # guess at 50-60 years old
hosDataTest$age[is.na(hosDataTest$age)] <- '[50-60)'

# Payer code
fct_count(hosDataTest$payer_code[hosDataTest$admission_source == 'ER'])
fct_count(hosDataTest$admission_source[is.na(hosDataTest$payer_code)])

# number of labs
hosDataTest$num_lab_procedures[is.na(hosDataTest$num_lab_procedures)] <- 1

# Let's explore some trees 
#First thing, figuring out what's important

# But first, let's ignore specific medications
noMedsTest <- hosDataTest[,-(22:44)]
# Also, remove payer code because I don't have a good interpolation idea
noMedsTest <- noMedsTest[,-c(9,1)] # and get rid of patient ID

# New Models --------------------------------------------
# Neural Network
#since neuralnet works with only numeric values
noMeds$admission_type <- as.numeric(noMeds$admission_type)
noMeds$diagnosis <- as.numeric(noMeds$diagnosis)

NN<-neuralnet(readmitted~number_diagnoses+diagnosis+admission_type,data = noMeds, hidden = 3,linear.output = FALSE)
plot(NN)

noMedsTest$admission_type<-as.numeric(noMedsTest$admission_type)
noMedsTest$diagnosis<-as.numeric(noMedsTest$diagnosis)

Predict<-compute(NN,noMedsTest)
Predic_result<-Predict$net.result
Predic_result

Predic_result[Predic_result  < 0.5] <- 0
Predic_result[Predic_result  > 0.5] <- 1

confusionMatrix(as.factor(Predic_result), as.factor(noMeds$readmitted), mode="everything")


mat = confusion.matrix(obs,pred,threshold=0.5)

##xgboost model
parametersGrid <-  expand.grid(eta = c(0.02,0.05,0.06), 
                               colsample_bytree=c(1,0.75),
                               max_depth=c(1,2,3,5,6,7,8,9,10),
                               nrounds=300,
                               gamma=1,
                               subsample = c(0.5, 0.75, 1),
                               min_child_weight=0 )

ControlParamteres <- trainControl(method = "cv",
                                  number = 3,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

modelxgboost <- train(readmitted~., 
                                  data = noMeds,
                                  method = "xgbTree",
                                  trControl = ControlParamteres,
                                  tuneGrid=parametersGrid)

pred <- predict(modelxgboost,test, type = "prob")
                              
# Random Forest models
my1Forest <- randomForest(x = noMeds[,-21],
                         y = noMeds$readmitted,
                         ntree = 500, 
                         mtry = 5)

my2Forest <- randomForest(x = Meds[,-44],
                          y = Meds$readmitted,
                          ntree = 500, 
                          mtry = 5)
# Quality of the forest prediction
medspred <- predict(my2Forest, newdata = Meds, type = "prob")
qual <- LogLoss(medspred[,2], Meds$readmitted)

medspred <- medspred[,2]
medspred[medspred >= 0.5] <- 1
medspred[medspred < 0.5] <- 0
cohen.kappa(x = cbind(medspred, hosData$readmitted))

# C50 Tree
actual <- as.factor(Meds$readmitted)
c50Tree <- C5.0.default(noMeds[,-21], actual, trials = 100)
c50Tree
summary(c50Tree)
plot(c50Tree)
# assess performance (overall) on training data
c50pred <- predict(c50Tree, newdata = noMeds, type = "prob")
c50result <- LogLoss(c50pred[,2], hosData$readmitted)
c50result

perf<- performance(c50pred)

# also compute kappa
c50pred <- c50pred[,2]
c50pred[c50pred < 0.5] <- 0
c50pred[c50pred >= 0.5] <- 1
cohen.kappa(x = cbind(c50pred, hosData$readmitted))

confusionMatrix(as.factor(c50pred), as.factor(hosData$readmitted), positive="1", mode="everything")
c50pred <- c50pred[,2]
r <- multiclass.roc(hosData$readmitted, c50pred, percent = TRUE)
roc<-r[['rocs']]
r1<-roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

# Tree for data insights
inTree <- C5.0.default(noMeds[,-21], noMeds$readmitted, trials = 1)
summary(inTree)


# Logistic Regression
logReg1 <- glm(readmitted ~ ., data = noMeds, family = "binomial")
summary(logReg1)

# test efficacy of model on training data
logPred <- predict(logReg1, type = "response")
log_result <- LogLoss(logPred, hosData$readmitted)

# Compute kappa
logPred[logPred >= 0.5] <- 1
logPred[logPred < 0.5] <- 0
cohen.kappa(x = cbind(logPred, hosData$readmitted))

confusionMatrix(as.factor(logPred), as.factor(hosData$readmitted), positive="1", mode="everything")


# MARS for Classification
# Set tuning parameters
myControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
myGrid <- expand.grid(
  degree = 1:2,
  nprune = seq(2, 30, 2)
)

# Change "readmitted" to factor ensure classification model
noMeds$readmitted <- as.factor(noMeds$readmitted)

marsClass <- train(readmitted ~ ., 
                   data = noMeds,
                   method = "earth",
                   trControl = myControl,
                   tuneGrid = myGrid,
                   glm=list(family='binomial'))

# Check how well the model did on test data
marspred <- predict(marsClass, newdata = noMeds, type = "prob")
marsout <- LogLoss(marspred[,2],hosData$readmitted)

marspred <- marspred[,2]
marspred[marspred >= 0.5] <- 1
marspred[marspred < 0.5] <- 0
cohen.kappa(x = cbind(marspred, hosData$readmitted))

# What's the best output?
marsClass$bestTune

