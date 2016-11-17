library(mice)
library(gender)
library(genderdata)
library(randomForest)
library(VIM)
library(caret)
library(rpart)
library(xgboost)

####SCOPE: Starting with OOB of 32.18%...

#load test/train data
trainShelter = read.csv('C:/Users/prrathin.ORADEV/Desktop/hacks/kaggle/US_animal_shelter/train.csv')
testShelter = read.csv('C:/Users/prrathin.ORADEV/Desktop/hacks/kaggle/US_animal_shelter/test.csv')

#Used for test,CV creation
ltrain = dim(trainShelter)[1]
ltest = dim(testShelter)[1]

#see what's maximum outcometype values in the train dataset and blindly set the same in test dataset.
prop.table(table(trainShelter$OutcomeType))
prop.table(table(trainShelter$OutcomeSubtype))

#making the outcome as string, just for ease of formatting..
trainShelter$OutcomeType = as.character(trainShelter$OutcomeType)
trainShelter$OutcomeSubtype = as.character(trainShelter$OutcomeSubtype)

#add the target label in the test dataset.
testShelter$OutcomeType = "Adoption"
testShelter$OutcomeSubtype = "Partner"

#note testShelter has ID which is not the same as AnimalID in trainingset, hence renaming.
names(testShelter)[1] = "AnimalID"
testShelter$AnimalID = as.factor(testShelter$AnimalID)

#combining train and test data set to perform missing value and other common operations.
combi = rbind(trainShelter,testShelter)

#releasing some memory
trainShelter = 0
testShelter = 0

combi$OutcomeType = as.factor(combi$OutcomeType)
combi$OutcomeSubtype = as.factor(combi$OutcomeSubtype)

#####################FEATURE ENGG##########################

#1.making clear features
#weekday
combi$DayName = weekdays(as.Date(combi$DateTime))
combi$DayName = as.factor(combi$DayName)

#Name Field
combi$NumChar = sapply(as.character(combi$Name),function(x) {nchar(x)})

#Named or not
combi$HasName = 1
combi$HasName[combi$Name == ""] = 0
combi$HasName = as.factor(combi$HasName)

#AnimalTypeCat
combi$AnimalTypeCat = 0
combi$AnimalTypeCat[combi$AnimalType == "Cat"] = 1
combi$AnimalTypeCat = as.factor(combi$AnimalTypeCat)


combi$WeekDaySunday = 0
combi$WeekDaySunday[combi$DayName == "Sunday"] = 1 
combi$WeekDaySunday = as.factor(combi$WeekDaySunday)


combi$WeekDaySaturday = 0
combi$WeekDaySaturday[combi$DayName == "Saturday"] = 1
combi$WeekDaySaturday = as.factor(combi$WeekDaySaturday)


combi$ColorChars = sapply(as.character(combi$Color),function(x) {nchar(x)})

combi$PitBullMix = 0
combi$PitBullMix[(combi$Breed == "Pit Bull Mix")] = 1


#Number of words
combi$NumWords = sapply(as.character(combi$Name),function(x) {length(strsplit(x,split=' ')[[1]])})

#seperating the datatime field to date,month,year
combi$Year = sapply(as.character(combi$DateTime), FUN=function(x) {strsplit(x, split='-')[[1]][1]})
combi$Month = sapply(as.character(combi$DateTime), FUN=function(x) {strsplit(x, split='-')[[1]][2]})
combi$Day = sapply(as.character(combi$DateTime), FUN=function(x) {strsplit(strsplit(x, split='-')[[1]][3],split=" ")[[1]][1]})
combi$Hour = sapply(as.character(combi$DateTime), FUN=function(x) {strsplit(x, split='[- :]')[[1]][4]})
combi$Mins = sapply(as.character(combi$DateTime), FUN=function(x) {strsplit(x, split='[- :]')[[1]][5]})

combi$Year = as.factor(combi$Year)
combi$Month = as.factor(combi$Month)
combi$Day = as.factor(combi$Day)
combi$Hour = as.factor(combi$Hour)
combi$Mins = as.integer(combi$Mins)


#Binning Mins
combi$Mins[(combi$Mins<=10)] = 10
combi$Mins[(combi$Mins>10) & (combi$Mins<=20)] = 20
combi$Mins[(combi$Mins>20) & (combi$Mins<=30)] = 30
combi$Mins[(combi$Mins>30) & (combi$Mins<=40)] = 40
combi$Mins[(combi$Mins>40) & (combi$Mins<=50)] = 50
combi$Mins[(combi$Mins>50)] = 51
combi$Mins = as.factor(combi$Mins)

#sex
combi$SexuponOutcome = as.character(combi$SexuponOutcome)
combi$SexuponOutcome[combi$SexuponOutcome==""] = "Unknown"

combi$Sex = sapply(combi$SexuponOutcome, FUN=function(x) {
  c = strsplit(x,split = ' ')[[1]][2]
  if(is.na(c)){NA}
  else if(c == "Female"){"Female"}
  else if(c == "Male"){"Male"}
})

combi$Sex = as.factor(combi$Sex)


#reproduce
combi$Reproduce = sapply(as.character(combi$SexuponOutcome), FUN=function(x) { 
  if(grepl("Intact",x)){1}
  else if(grepl("Neutered",x)| grepl("Spayed",x)){0}
  else if(grepl("Unknown",x)| grepl("",x)){NA}
})

combi$Reproduce = as.factor(combi$Reproduce)


combi$Age = sapply(as.character(combi$AgeuponOutcome), FUN=function(x) {
  if(grepl("day",x,ignore.case = T)){as.integer(strsplit(as.character(x),split=" ")[[1]][1])}
  else if(grepl("week",x,ignore.case = T)){as.integer(strsplit(as.character(x),split=" ")[[1]][1])*7}
  else if(grepl("month",x,ignore.case = T)){as.integer(strsplit(as.character(x),split=" ")[[1]][1])*30}
  else if(grepl("year",x,ignore.case = T)){as.integer(strsplit(as.character(x),split=" ")[[1]][1])*365}
  else if(grepl("",x,ignore.case = T)){NA} #NA for empty fields
})

#0 years makes lessthan year..but dont know in days..it will be considered when doing binning
combi$Age[combi$AgeuponOutcome == "0 years"] = NA #mice would difficult to impute this value

#custom imputation using rpart, from reproduce and sex who are less than 0years.
fit = rpart(Age ~ Reproduce + Sex, data=combi[(!is.na(combi$Age) & (combi$Age <365)),])
predage = predict(fit, newdata=combi[is.na(combi$Age),])
combi$Age[is.na(combi$Age)] = round(predage)


#TODO: more feature engg required for color
#color
table(combi[1:ltrain,]$Color,combi[1:ltrain,]$OutcomeType)
#considering above 100 values in the type of color..
combi$newColor = as.character(combi$Color)
s = sum(summary(combi$Color) >95)
list = names(summary(combi$Color) >95)[1:s]
combi$newColor[!(combi$Color %in% list)] = "Others"
combi$newColor = as.factor(combi$newColor)

#Major color
combi$MajorColor =  sapply(as.character(combi$Color), FUN=function(x) {
  strsplit(x,split = "[/ ]")[[1]][1]
})
combi$MajorColor = as.factor(combi$MajorColor)

#Mixed color
combi$MixColor = sapply(as.character(combi$Color), FUN=function(x) { 
  if(grepl("/",x)){1}
  else {0}
})

#Breed - how many subwords
combi$MixBreed = sapply(as.character(combi$Breed), FUN=function(x) { 
  length(strsplit(x,"[/ ]")[[1]])
})


#Dark color or Light color
# combi$Intensity[combi$MajorColor %in% c("Apricot","Blue","Buff","Cream","Fawn","Flame","Gold","Gray","Lilac","Lynx","Orange","Pink","Silver","White","Yellow")] = "Light"
# combi$Intensity[combi$MajorColor %in% c("Agouti","Black","Brown","Calico","Chocolate","Liver","Red","Tan","Sable","Seal","Torbie","Tortie","Tricolor")] = "Dark"
# 
# combi$Intensity = as.factor(combi$Intensity)

#Minor color
combi$MinorColor =  sapply(as.character(combi$Color), FUN=function(x) {
  strsplit(x,split = "[/]")[[1]][2]
})

combi$MinorColor[is.na(combi$MinorColor)] = NA
combi$MinorColor = as.factor(combi$MinorColor)


#Type of color in the animal
# combi$Tabby = 0
# combi$Tabby[grepl("Tabby",combi$Color)] = 1
# 
# combi$Brindle = 0
# combi$Brindle[grepl("Brindle",combi$Color)] = 1
# 
# combi$Tick = 0
# combi$Tick[grepl("Tick",combi$Color)] = 1
# 
# combi$Point = 0
# combi$Point[grepl("Point",combi$Color)] = 1
# 
# combi$Merle = 0
# combi$Merle[grepl("Merle",combi$Color)] = 1
# 
# combi$Smoke = 0
# combi$Smoke[grepl("Smoke",combi$Color)] = 1
# 
# combi$Tiger = 0
# combi$Tiger[grepl("Tiger",combi$Color)] = 1
# 
# #Breed Type - Mix/No
# combi$BreedType = sapply(as.character(combi$Breed), FUN=function(x) { 
#   if(grepl("Mix",x,ignore.case = T)){"Mix"}
#   else {"No"}
# })
# combi$BreedType = as.factor(combi$BreedType)

#PetType
combi$PetType = sapply(as.character(combi$Breed), FUN=function(x) { 
  strsplit(x,split="[/ ]")[[1]][1]
})
combi$PetType = as.integer(as.factor(combi$PetType))

#Hair
combi$Hair = sapply(as.character(combi$Breed), FUN=function(x) { 
  if(grepl("Long",x,ignore.case = T)){3}
  else if(grepl("Short",x,ignore.case = T)){1}
  else if(grepl("Medium",x,ignore.case = T)){2}
  else {0}
})


#TODO: AGE binning
quantile(combi$Age,probs = seq(0,1,0.05),na.rm = T)

combi$Age[combi$Age<=21] = 21
combi$Age[(combi$Age>21) & (combi$Age<=30)] = 30
combi$Age[(combi$Age>30) & (combi$Age<=60)] = 60
combi$Age[(combi$Age>60) & (combi$Age<=90)] = 90
combi$Age[(combi$Age>90) & (combi$Age<=150)] = 150
combi$Age[(combi$Age>150) & (combi$Age<=240)] = 240
combi$Age[(combi$Age>240) & (combi$Age<=365)] = 365
combi$Age[(combi$Age>365) & (combi$Age<=730)] = 730
combi$Age[(combi$Age>730) & (combi$Age<=1095)] = 1095
combi$Age[(combi$Age>1095) & (combi$Age<=1825)] = 1825
combi$Age[(combi$Age>1825) & (combi$Age<=2190)] = 2190
combi$Age[(combi$Age>2190) & (combi$Age<=3285)] = 3285
combi$Age[(combi$Age>3285)] = 3286


#impute
impute = mice(combi[,-c(1,2,3,4,5,7,8,9,10)],maxit=0,m=5)
summary(impute)
impcombi = complete(impute)
impcombi$AnimalID = combi$AnimalID
impcombi$OutcomeType = combi$OutcomeType

#Feature Month and reproduce
impcombi$RepMonth = as.integer(impcombi$Month) * as.integer(impcombi$Reproduce)
impcombi$RepMonth = as.factor(impcombi$RepMonth)

#Feature Sex and reproduce
impcombi$SexRep = as.integer(impcombi$Sex) * as.integer(impcombi$Reproduce)
impcombi$SexRep = as.factor(impcombi$SexRep)

#
impcombi$FemaleRep = 0
impcombi$FemaleRep[(impcombi$Sex == "Female") & (impcombi$Reproduce == 1)] = 1

#hour n day
impcombi$HourDay = 0
impcombi$HourDay = as.integer(impcombi$Hour) * as.integer(impcombi$Day)

#Min n Hour
impcombi$HourMin = 0
impcombi$HourMin = as.integer(impcombi$Hour) * as.integer(impcombi$Mins)

#seperate train and data
train = impcombi[1:ltrain,]
test = impcombi[ltrain+1:ltest,]


#releasing some memory

combi = 0
impcombi = 0


#Random forrest

# rfFit = randomForest(OutcomeType ~ NumChar + MajorColor+newColor +MinorColor+ Month+ Year + Day + Reproduce + AnimalType + Hair+
#                      AgeRep+RepMonth+SexRep+FemaleRep+Hour+HourDay+Mins+HourMin+Sex+Mono+
#                      BreedType,
#                      data = train, mtry=5, ntree = 150)


rfFit = randomForest(OutcomeType ~ . -AnimalType -AnimalID -Sex -OutcomeType -MixColor -newColor -MinorColor,
                     data = train, mtry=5, ntree = 200)

rfFit #29.63%
varImpPlot(rfFit)

#####trying to figureout whether underfitting or not
pred5 = predict(rfFit, newdata = test,type = 'vote')
#it looks like overfitts
# OOB estimate of  error rate:

#submit = data.frame(ID = test$AnimalID, pred5)
#write.csv(submit, file = "C:/Users/prrathin.ORADEV/Desktop/hacks/kaggle/US_animal_shelter/submit_new_plain_rf_31.99%.csv", row.names = FALSE)

###XGBOOST


xgTrain =  data.matrix(train[,-c(1,26,16,27,21,19,23)])
xgTest = data.matrix(test[,-c(1,26,16,27,21,19,23)])

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              #"eta" = "0.1", #eta is like learning rate
              "num_class" = 5)


set.seed(2123)
xgbcv =  xgb.cv(data=xgTrain,label = as.integer(train[,27])-1,nrounds = 1000,nfold=7,early.stop.round = 10, maximize = FALSE,params = param)

###trying iterative version of xgboost to figureout best tuning params.

best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0

for (iter in 1:100) {
  param <- list(objective = "multi:softprob",
                eval_metric = "mlogloss",
                num_class = 5,
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 1000
  cv.nfold = 7
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=xgTrain, label = as.integer(train[,27])-1,params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early.stop.round=10, maximize=FALSE)
  
  min_logloss = min(mdcv[, test.mlogloss.mean])
  min_logloss_index = which.min(mdcv[, test.mlogloss.mean])
  
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}


set.seed(best_seednumber)
#by tuning for test-mlogloss:0.739732
xg = xgboost(data=xgTrain,label = as.integer(train[,27])-1,nrounds = best_logloss_index,nfold=7,params = best_param)

# #136
# $max_depth
# [1] 10
# 
# $eta
# [1] 0.07882545
# 
# $gamma
# [1] 0.1835613
# 
# $subsample
# [1] 0.7508865
# 
# $colsample_bytree
# [1] 0.6374721
# 
# $min_child_weight
# [1] 5
# 
# $max_delta_step
# [1] 5


#train-mlogloss:0.536820+0.004358	test-mlogloss:0.755052+0.007092
#xg = xgboost(data=xgTrain,label = as.integer(train[,27])-1,nrounds = 136,nfold=7,params = param)
preddf <- data.frame(matrix(predict(xg,xgTest), ncol = 5, byrow=TRUE))


#tuninig XG boost improves score :)
#colnames(preddf) = colnames(pred5)
#submit1 = data.frame(ID = test$AnimalID, preddf)
#write.csv(submit1, file = "C:/Users/prrathin.ORADEV/Desktop/hacks/kaggle/US_animal_shelter/submit_xg_boost_best.csv", row.names = FALSE)





# eval_metric [ default according to objective ]
# The metric to be used for validation data.
# The default values are rmse for regression and error for classification.
# Typical values are:
#   rmse - root mean square error
# mae - mean absolute error
# logloss - negative log-likelihood
# error - Binary classification error rate (0.5 threshold)
# merror - Multiclass classification error rate
# mlogloss - Multiclass logloss
# auc: Area under the curve

avg = preddf
avg = avg[,1:5]
avg5 = 0.5*(avg+pred5)

ens7 = read.csv('C:/Users/prrathin.ORADEV/Desktop/hacks/kaggle/US_animal_shelter/submit_em_20june.csv')
ens7 = ens7[,2:6]
ens9 = 0.5*(avg5+ens7)

colnames(ens9) = colnames(pred5)
submit = data.frame(ID = test$AnimalID, ens9)

# feature_important = xg.importance(model = xg, feature_names = colnames(train_data))
# head(feature_important, n = 20) %>%
#   formattable(align = 'l')

write.csv(submit, file = "C:/Users/prrathin.ORADEV/Desktop/hacks/kaggle/US_animal_shelter/submit_em_21june.csv", row.names = FALSE)
