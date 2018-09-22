
##### 1. Loading packages #####

if (!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}

if(!require("caret")){
  install.packages("caret")
  library("caret")
}
if (!require("caTools")){
  install.packages("caTools")
  library("caTools")
}
if (!require("RANN")){
  install.packages("RANN")
  library("RANN")
}

if (!require("VIM")){
  install.packages("VIM")
  library("VIM")
}
if (!require("rpart")){
  install.packages("rpart")
  library("rpart")
}
if (!require("DMwR")){
  install.packages("DMwR")
  library("DMwR")
}

##### 2. Loading Dataset. #####

data_train = read.csv("train.csv", header=TRUE, sep = ",")
data_test = read.csv("test.csv", header=TRUE, sep = ",")


##### 3. Responce variable & responce variable vs independent variables. #####

##### 3.1. Responce variable. ##### 

ggplot(data_train, aes(Survived)) +
geom_bar(fill = "#0073C2FF") 

df <- data_train %>%
  group_by(as.factor(Survived)) %>%
  summarise(Survived = n())
df # 549 died and 342 survived

# Propotion:
# This is an important step because if the proportion was smaller than 15%, it would be considered 
#a rare event and would be more challenging to model.

prop.table(table(data_train$Survived))

# 61% died vs 38% survived.

# Response variable vs Sex

ggplot(data_train, aes(as.factor(Survived), fill = Sex)) +
  geom_bar()

table(data_train$Survived, data_train$Sex)

table(data_train$Sex, data_train$Survived)

# Among those who survived were 233 female and 109 male and those who didn´t 81 female and 468 male

##### 3.2 Response variable vs Pclass. #####

ggplot(data_train, aes(as.factor(Survived), fill = as.factor(Pclass))) +
  geom_bar()

ggplot(data_train, aes(as.factor(Pclass), fill = as.factor(Survived))) +
  geom_bar()


table(data_train$Survived, as.factor(data_train$Pclass))

# Among the first class 136 (almost two thirds - 62%) people survived and 80 (38%) died, the second class - 97(52%) died and 87(48% - less than a half) survived
# and whithin the third class 119 (25%) survived and 372(75%) died

##### 4. Combining two data sets & creating new variables. #####

data_test$Survived <- NA
data_full <- rbind(data_train, data_test)

##### 4.1  Variable "Titles" #####

# Function to extract the title from a string.

get_title <- function(x){
  x<-strsplit(x, " ")
  tit<-c()
  for (word in x[[1]]){
   if (grepl("\\.",word) & length(strsplit(word, "")[[1]])>2){
     tit<-append(tit,word,after = length(tit))
     
   }
     
  }
return(tit)
}

# Function to extract the surname from a string.

get_surname <- function(x){
  x<-strsplit(x, " ")
  surn<-x[[1]][1]
  return(surn)
}


# A loop to create a list of titles only:

k<-c()

for (i in data_full$Name) {
  i<-get_title(i)
  k<-append(k,i,after = length(k))
  
}

# Creating a new column "Titles".

data_full$Titles<-k
data_full$Titles<-as.factor(data_full$Titles)

# Function to extract the surname from a string.

get_surname <- function(x){
  x<-strsplit(x, " ")
  surn<-x[[1]][1]
  return(surn)
}


# A loop to create a list of surnames only:

s<-c()

for (i in data_full$Name) {
  i<-get_surname(i)
  s<-append(s,i,after = length(s))
  
}


# Creating a new column "Surname".

data_full$Surnames<-s
data_full$Surnames<-as.factor(data_full$Surnames)

table(data_full$Surnames)

# Reassigning low occurance levels to "Others"

levels(data_full$Titles)<-list(Mr.=c("Mr.", "Rev.","Jonkheer."), Miss.="Miss.", Mrs.="Mrs.", Master.="Master.",
                Others = c("Capt.", "Col.",  "Don.", "Major.", "Sir.", "Dr.","Dona.","Countess.","Ms.", "Lady.", "Mlle.", "Mme."))


table(data_full$Titles)

##### 4.2. Variable Deck #####

#creating a column of NA values first

data_full$Deck <- NA  

#NULL splits at each position

data_full$Deck <- sapply(as.character(data_full$Cabin), function(x){ strsplit(x, NULL)[[1]][1]}) 
data_full$Deck <- as.factor(data_full$Deck)

# Deck vs. tickets numer:

data_deck<-data_full[!is.na(data_full$Deck),] #leave only those rows of the dataset where Deck is not NA.

# Create a temporary data frame storing Ticket values & filter the ticket numbers which repeat:

repeat_table<-data.frame(table(data_deck$Ticket))
repeat_table<-repeat_table[repeat_table$Freq>1,]

dim(repeat_table)

# Creating a Deck variable/column and setting its level the same as data_deck$Deck variable:

length(intersect(repeat_table$Var1,data_deck$Ticket))
repeat_table$Deck<-0
repeat_table$Deck<-as.factor(repeat_table$Deck)

levels <- levels(data_deck$Deck)


repeat_table$Deck <- factor(repeat_table$Deck, levels = levels)

table(repeat_table$Deck)

# A loop to get the Decks of those passengers whose tickets repeat 2 and more times.

for(i in 1:nrow(data_deck)) {
  for (j in 1:nrow(repeat_table)){
    if( data_deck$Ticket[i]== repeat_table$Var1[j]) {
      repeat_table$Deck[j] = data_deck$Deck[i]  }
      
       }   } 

# A loop to add decks to those passengers whose ticket number are the same.

for(j in 1:nrow(data_full)) {
  for(i in 1:nrow(repeat_table)) {
    if( data_full$Ticket[j]== repeat_table$Var1[i]) {
      data_full$Deck[j] = repeat_table$Deck[i]} }   } 


##### 4.5 Ticket & Fare #####

# create a temporary data frame storing Ticket values

FOO <- data.frame(table(data_full$Ticket)) # there is a column "frequency"

# Store only those Ticket values that belong to more than one passenger:

FOO <- FOO[FOO$Freq > 1, ]
dim(FOO)
FOO$Fare <- 0

# Run a nested loop to extract the Fare values for the Ticket values in FOO

for(i in 1:nrow(data_full)) {
  for(j in 1:nrow(FOO)) {
    if(data_full$Ticket[i] == FOO$Var1[j]) {
      FOO$Fare[j] = data_full$Fare[i] } }   } 

# Calculate the Fare per passenger

FOO$Fare <- FOO$Fare / FOO$Freq

# Recalculate the fare in the data_train

for(i in 1:nrow(data_full)) {
  for(j in 1:nrow(FOO)) {
    if(data_full$Ticket[i] == FOO$Var1[j]) {
      data_full$Fare[i] = FOO$Fare[j] } }   } 

##### 4.6. Family Id.

# Family Size:

# 1 is for the passenger himself/herself

data_full$FamilySize <- data_full$SibSp + data_full$Parch + 1 

# Family ID

data_full$FamilyID <- paste(as.character(data_full$FamilySize), data_full$Surnames, sep="")


data_full$FamilyID[data_full$FamilySize <= 3] <- 'Small'

famIDs <- data.frame(table(data_full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 3,]
data_full$FamilyID[data_full$FamilyID %in% famIDs$Var1] <- 'Small'


data_full$FamilyID<-as.factor(data_full$FamilyID)

##### 4.6. Data set check. #####

str(data_full)

# Zero fare:

subset(data_full, Fare==0) # There are some fares which are equal to zero, let´s replace them with NAs.

data_full$Fare[data_full$Fare==0] <- NA

# Pclass to facator:

data_full$Pclass <- as.factor(data_full$Pclass)

# New data_frame just in case

data<-data_full 

# Drop columns/variables I am not going to use:

colnames(data_full)

data_full$PassengerId<-NULL
data_full$Name<-NULL
data_full$Ticket<-NULL
data_full$Cabin<-NULL
data_full$Surnames<-NULL
data_full$FamilySize<-NULL

###### 5. NAs treatment. Random forest NA replacement. #####

# Visualizing missing values:

data_full[data_full=="" | data_full==" "] <- NA


NA_data_full <- data_full %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))

NA_data_full<-sort(NA_data_full, decreasing = TRUE)

ggplot(,aes(x=as.integer(NA_data_full[1,]), y=variable.names(NA_data_full))) + 
  geom_point(size=3, show.legend = TRUE, colour="orange") + 
  ggtitle(label="MISSING VALUES")+ labs(x="the number of NAs", y="variables") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="brown", linetype="dashed"))



# Duplicating dataframe:

data_full_NA<-data_full


##### 5.1 Age Nas random forest replacement. #####

set.seed(123)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Titles , # + FamilySize
                data=data_full_NA[!is.na(data_full_NA$Age),], 
                method="anova", na.action=na.omit)


data_full_NA$Age[is.na(data_full_NA$Age)] <- predict(Agefit, data_full_NA[is.na(data_full_NA$Age),])
sum(is.na(data_full_NA$Age))

##### 5.2 Fare Nas random forest replacement. #####

set.seed(123)
Farefit <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Embarked + Titles+Age , # + FamilySize
                data=data_full_NA[!is.na(data_full_NA$Fare),], 
                method="anova", na.action=na.omit)


data_full_NA$Fare[is.na(data_full_NA$Fare)] <- predict(Farefit, data_full_NA[is.na(data_full_NA$Fare),])


sum(is.na(data_full_NA$Fare))

##### 5.3 Embarked NAs replacement & Dropping the levels that do not occur: #####

table(data_full_NA$Embarked) # "S" is the most commun one 
data_full_NA$Embarked[is.na(data_full_NA$Embarked)] <- "S"

unused_levels<- function(dataset, number){
  factor_var <- sapply(dataset, is.factor)
  factor_df <-dataset[, factor_var]
  factors<-c()
  for (column in names(factor_df)){
    
    levels_ = table(factor_df[[column]])
    levels_df = as.data.frame(levels_)
    
    if (min(levels_df$Freq)<=number){
      factors<-append(factors,column,after = length(factors)) 
      
    }
    
  }
  return(factors)
}


unused_levels(data_full_NA, number=0)


data_full_NA$Embarked<-droplevels(data_full_NA$Embarked)


##### 5.4. Deck NAs replacement. #####

table(data_full_NA$Deck) # double check.

subset(data_full_NA, Deck=="T") # only one and the price is high and the class is 1
levels(data_full_NA$Deck)[levels(data_full_NA$Deck)=="T"] <- "B"

subset(data_full_NA, Deck=="B") # There is one fare equal to $5 which is way too low for a deck "B" cabin.

data_full_NA$Fare[( data_full$Pclass=="1" & data_full_NA$Fare==5)]<-23

set.seed(123)

class_mod <- rpart(Deck ~ Fare + Pclass + Sex + SibSp + Parch + Embarked + Titles , # + FamilySize
                 data=data_full_NA[!is.na(data_full_NA$Deck),], 
                 method="class", na.action=na.omit)
rad_pred <- predict(class_mod, data_full_NA[is.na(data_full_NA$Deck), ])
predicteds <- as.factor(colnames(rad_pred)[apply(rad_pred, 1, which.max)])

table(predicteds)

data_full_NA$Deck[is.na(data_full_NA$Deck)]<-predicteds
table(data_full_NA$Deck, data_full_NA$Pclass)

# Splitting data_full back into train and test:

data_train <- data_full_NA[1:891,]
data_test <- data_full_NA[892:1309,]


##### 6. Data Engeneering part. #####

#####  6.1. Deck rearrangement. #####

data_train$Deck<-
  ifelse((data_train$Deck=="G" | data_train$Deck=="F"), 'GF', 
         ifelse((data_train$Deck=="E" | data_train$Deck=="D"), "ED","ABC"))
data_train$Deck<-as.factor(data_train$Deck)

prop.table(table(data_train$Deck, data_train$Survived),1)

# Same for the test:

data_test$Deck<-
  ifelse((data_test$Deck=="G" | data_test$Deck=="F"), 'GF', 
         ifelse((data_test$Deck=="E" | data_test$Deck=="D"), "ED","ABC"))
data_test$Deck<-as.factor(data_test$Deck)

##### 6.2. Kid variable. #####

data_train$Age<-round(data_train$Age)

table( data_train$Age, data_train$Survived)
data_train$Kid<-
  ifelse(data_train$Age < 16,"yes", "no") 

data_train$Kid<-as.factor(data_train$Kid)

prop.table(table(data_train$Kid, data_train$Survived),1)

# Test:

data_test$Age<-round(data_test$Age)

data_test$Kid <-
  ifelse(data_test$Age<16,"yes", "no") 
data_test$Kid<-as.factor(data_test$Kid)


##### 6.3 Low_Fare variable. #####

table(data_train$Fare, data_train$Survived)


data_train$Low_Fare <-ifelse(data_train$Fare <11, "yes", "no") 

data_train$Low_Fare<- as.factor(data_train$Low_Fare)
prop.table(table(data_train$Low_Fare, data_train$Survived),1)

# test:

data_test$Low_Fare <-ifelse(data_test$Fare <11, "yes", "no") 

data_test$Low_Fare<- as.factor(data_test$Low_Fare)


##### 6.4 Family ID rearrangement. #####
 
table(data_train$FamilyID) # make sure both data_sets share the same levels:
table(data_test$FamilyID) # 4Baclini, 4Carter, 6Panula, 6Skoog, can only befound in train set.


levels(data_train$FamilyID)[levels(data_train$FamilyID)=="4Baclini,"] <- "Small"
levels(data_train$FamilyID)[levels(data_train$FamilyID)=="4Carter,"] <- "Small"
levels(data_train$FamilyID)[levels(data_train$FamilyID)=="6Panula,"] <- "Small"
levels(data_train$FamilyID)[levels(data_train$FamilyID)=="6Skoog,"] <- "Small"

data_test$FamilyID<-droplevels(data_test$FamilyID)


##### 7 Modelling. #####

str(data_train)
 
data_train1<-data_train


##### 7.1 Dumify the dataset. #####

str(data_train1)
titanicDummy <- dummyVars("~.",data=data_train1, fullRank=F)
titanicDF <- as.data.frame(predict(titanicDummy,data_train1))
str(titanicDF)

titanicDummy_t <- dummyVars("~.",data=data_test, fullRank=F)
titanicDF_t <- as.data.frame(predict(titanicDummy_t,data_test))
str(titanicDF_t)


# data_train$Survived to factor:

titanicDF$Survived <-
  ifelse(titanicDF$Survived == 1,
         "yes",
         "no"
  )
titanicDF$Survived<-as.factor(titanicDF$Survived)
str(titanicDF)


##### 7.2 Create trainControl object: myControl for crossfold validation.#####

set.seed(123)

myControl <- trainControl(
  method = "repeatedcv", number = 10,
  repeats = 5,
  
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)


##### 7.3 Training glm #####

# metric="Accuracy"
set.seed(123)
model_m <- train(Survived~.,  data = titanicDF, method = "glm", 
                 trControl = myControl, 
                 metric="Accuracy",
                 preProcess=c("zv", "nzv", "center", "scale", "pca")) # 



print(model_m) # Accuracy = 0.820455 

varImp(model_m)


##### 7.4 Training glmnet #####

set.seed(123)
net_exp <-train(Survived~.,  data = titanicDF,
            tuneGrid = expand.grid(alpha = seq(0, 1, length=10),
            lambda = seq(0.001, .23, length = 10)),
            trControl=myControl,
            method = "glmnet",
            metric="Accuracy",
            preProcess=c("zv","center", "scale"))

print(max(net_exp[["results"]][["Accuracy"]])) # Accuracy = 0.8321485
plot(net_exp)
print(net_exp) # alpha = 0.7777778 and lambda = 0.001.

# Model with defined alpha and lambda:

set.seed(123)

net_exp <-train(Survived~.,  data = titanicDF,
                tuneGrid = expand.grid(alpha =0.7777778,
                lambda = 0.001),
                trControl=myControl,
                method = "glmnet",
                metric="Accuracy",
                preProcess=c("center", "scale"))

print(max(net_exp[["results"]][["Accuracy"]])) # 0.8321485

##### 7.5 Simple random forest #####

set.seed(123)
model_rf_1 = train(Survived~.,  data = titanicDF,
                 metric="Accuracy",
                 method = "ranger",
                 tuneGrid=expand.grid(mtry =c(10,11,12,13,14 ),splitrule = c("gini"),
                 min.node.size = c(1:10)), 
                 trControl = myControl)


print(max(model_rf_1[["results"]][["Accuracy"]])) # 0.8449257
plot(model_rf_1) # mtry = 14, splitrule = gini and min.node.size = 10                           
print(model_rf_1)                            


set.seed(123)
model_rf_2 = train(Survived~.,  data = titanicDF,
                   metric="Accuracy",
                   method = "ranger",
                   tuneGrid=expand.grid(mtry =c(14 ),splitrule = c("gini"),
                                        min.node.size = c(10)) , 
                   trControl = myControl)

print(max(model_rf_2[["results"]][["Accuracy"]])) # 0.8415518                                   

set.seed(123)
model_rf_11 = train(Survived~.,  data = titanicDF,
                   metric="Accuracy",
                   method = "ranger",
                   tuneGrid=expand.grid(mtry =c(10,11,12,13,14, 15, 16, 17, 18 ),splitrule = c("gini"),
                                        min.node.size = c(1:10)), 
                   trControl = myControl)


print(max(model_rf_11[["results"]][["Accuracy"]])) # 0.8449257
plot(model_rf_11)
print(model_rf_11) # mtry = 10, splitrule = gini and min.node.size = 4.

set.seed(123)
model_rf_11 = train(Survived~.,  data = titanicDF,
                    metric="Accuracy",
                    method = "ranger",
                    tuneGrid=expand.grid(mtry =c(10),splitrule = c("gini"),
                                         min.node.size = c(4)), 
                    trControl = myControl)
print(max(model_rf_11[["results"]][["Accuracy"]])) # 0.8422437

##### 7.6 stochastic gradient boosting machine #####

gbmTuningGrid = expand.grid(interaction.depth = c(10), 
                            n.trees = c(2399:2500), 
                            shrinkage = c( 0.001),
                            n.minobsinnode = c(1,2,3,4,5,10))

set.seed(123)

model_gbm = train(Survived~.,  data = titanicDF,
                  metric = "Accuracy",
                  method = "gbm",
                  trControl = myControl,
                  tuneGrid= gbmTuningGrid,
                  preProcess=c("zv"))

print(max(model_gbm[["results"]][["Accuracy"]])) # 0.8422314
#n.trees =650, interaction.depth = 10, shrinkage = 0.01 and n.minobsinnode = 5.
print(model_gbm)
plot(model_gbm)

set.seed(123)

model_gbm_1 = train(Survived~.,  data = titanicDF,
                  metric = "Accuracy",
                  method = "gbm",
                  trControl = myControl,
                  tuneGrid= expand.grid(interaction.depth = c(10), 
                                        n.trees = 2423, 
                                        shrinkage = c( 0.001),
                                        n.minobsinnode = 10),
                  preProcess=c("zv"))

print(max(model_gbm_1[["results"]][["Accuracy"]])) # 0.8420041


##### 7.7 Extreme gradient boosting machine #####

gbmTuningGrid_x<-expand.grid(eta =  c( 0.01,0.05,0.1), 
                             colsample_bytree = c(0.755,  0.8),
                             max_depth=c(2,4,6,8,10,14), 
                             nrounds=1000, 
                             gamma = c( 0.01), 
                             subsample= seq(0.8285714, 1),
                             min_child_weight=1)


set.seed(123)
model_gbm_x = train(Survived~.,  data = titanicDF,
                    metric = "Accuracy",
                    method = "xgbTree",
                    trControl = myControl,
                    
                    tuneGrid=gbmTuningGrid_x,
                    preProcess=c("zv", "nzv"))
print(max(model_gbm_x[["results"]][["Accuracy"]])) # 0.8402087
print(model_gbm_x)
plot(model_gbm_x)
varImp(model_gbm_x)

# The final values used for the model were nrounds = 1000, max_depth = 4, eta = 0.01, gamma =
# 0.01, colsample_bytree = 0.755, min_child_weight = 1 and subsample = 0.8285714.

gbmTuningGrid_xx<-expand.grid(eta =  c( 0.1), 
                             colsample_bytree = seq(0.755,  0.8, length = 15), 
                             max_depth=c( 6), 
                             nrounds=c( 50), 
                             gamma = c( 0.01),
                             subsample= seq(0.8285714),
                             min_child_weight=1)
set.seed(123)
model_gbm_xx = train(Survived~.,  data = titanicDF,
                    metric = "Accuracy",
                    method = "xgbTree",
                    trControl = myControl,
                    
                    tuneGrid=gbmTuningGrid_xx,
                    preProcess=c("zv", "nzv"))

print(max(model_gbm_xx[["results"]][["Accuracy"]])) # 0.8388552


#### 7.8 Passing model_list to resamples() ##### 

model_list <- list( GLM =model_m, GLMNET = net_exp,RandomF = model_rf_2,RandomF_2 = model_rf_11, GBM = model_gbm_1, XXboost=model_gbm_xx)

resamples <- resamples(model_list)

# Summarize the results

print(summary(resamples))

# Visualize the results:

bwplot(resamples, metric = "Accuracy")


##### 8 Predicting on the test set #####

# Random forest model:

p <- predict(model_rf_2, titanicDF_t)

# Xboost model:

p_boost<-predict(model_gbm_xx, titanicDF_t)

# Random forest_2 model:

p_11 <- predict(model_rf_11, titanicDF_t)

##### 9 Saving and sending the result to Kaggle #####

PassengerId<-data$PassengerId[892:1309]

# Random forest:
Prediction<-
  ifelse(p == "yes",
         "1",
         "0"
  )
table(Prediction)

Prediction<-as.factor(Prediction)

# Xboost:

Prediction_x<-
  ifelse(p_boost == "yes",
         "1",
         "0"
  )
table(Prediction_x)

Prediction_x<-as.factor(Prediction_x)

# Random forest_11:

p_11<-
  ifelse(p == "yes",
         "1",
         "0"
  )

table(p_11)

Prediction<-as.factor(Prediction)

##### 10 Submition ####

# Random forest:
submit <- data.frame(PassengerId = PassengerId, Survived = Prediction)
write.csv(submit, file = "test_predictions.csv", row.names = FALSE)

# Xboost:

submit <- data.frame(PassengerId = PassengerId, Survived = Prediction_x)
write.csv(submit, file = "test_predictions_xboost.csv", row.names = FALSE)

# Random forest 11

submit <- data.frame(PassengerId = PassengerId, Survived = p_11)
write.csv(submit, file = "test_predictions_rf_11.csv", row.names = FALSE)
