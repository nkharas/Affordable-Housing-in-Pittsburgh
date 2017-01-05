# Author: Nick Kharas
# Description : Classification Models to understand investor behavior

# Import the cleaned property assessments data
data.set <- read.csv('property_assessments_clean.csv', sep="," ,header=TRUE)

# Convert numbers into factors where applicable
data.set$PROPERTYZIP <- as.factor(data.set$PROPERTYZIP)
data.set$isVacant <- as.factor(data.set$isVacant)
data.set$ClassLabel <- as.factor(data.set$ClassLabel)
# data.set$YEARBLT <- as.Date(as.character(data.set$YEARBLT), "%Y")
data.set$YEARBLT <- as.factor(data.set$YEARBLT)

####################### PREPARE DATA FOR MODELING #######################################

# Below code guarantees that data from both classes are evenly split in train and test data
# Data has some bias, as one class dominates the other; we cannot expect the market to be overrun by flippers
# only about 10% parcels are classified as potentially bad investmemts
# For both classes, 80% data is for training and the rest is for testing

train.test.split <- function(df){
  n.row <- nrow(df)        # Number of rows in the data set
  n.col <- ncol(df)        # Index of dependent variable, always last variable in the data set
  df <- df[sample(n.row),] # Shuffle the data set, change the order of the rows, remove possible selection bias
  train.rows <- I( round(n.row * (4/5) ) ) # 80% data for training
  # Split the train and test data
  train <- df[0:train.rows , ]
  test <- df[I(train.rows + 1) : n.row , ]
  return(list("train" = train, "test" = test)) # Return a list containing both data frames
}

data.set.1 <- data.set[which(data.set$ClassLabel == 1) , ] # Potentially bad investments
data.set.0 <- data.set[which(data.set$ClassLabel == 0) , ] # Other parcels

# Split train and test for Class label = 1
data.sets.1 <- train.test.split(data.set.1)
train.1 <- data.sets.1$train
test.1 <- data.sets.1$test

# Split train and test for Class label = 0
data.sets.0 <- train.test.split(data.set.0)
train.0 <- data.sets.0$train
test.0 <- data.sets.0$test

# Combine both results and re-shuffle rows to remove selection bias
train <- rbind(train.0, train.1)
test<- rbind(test.0, test.1)

train <- train[sample(nrow(train)),]
test <- test[sample(nrow(test)),]

# nrow(train) = 143765
# nrow(test) = 35941
# nrow(data.set) = 179706
# nrow(train) + nrow(test) = nrow(data.set)

# Clear memory
rm(data.sets.0, data.sets.1, data.set.0, data.set.1, 
   train.0, train.1, test.0, test.1, data.set)

####################### CLASSIFICATION MODELS ######################################

library(ROCR)

####################### LOGISTIC REGRESSION ########################################

# Computationally expensive
# lgm.fit <- glm(formula = ClassLabel  ~ ., data = train, family=binomial(link = "logit"))

####################### DECISION TREES #############################################

############# ctree #############
library(party)

ctr.fit <- ctree(ClassLabel  ~ . , data = train) # Will take long to run
# plot(ctr.fit)
# Evaluate model performance
ctr.pred <- predict(ctr.fit, test, type = "prob", simply = FALSE)  # Fetch predicted values from the model

# Convert predictions into data frame - step 1
ctr.pred.df <- rep(NA, length(ctr.pred))
# Convert predictions into data frame - step 2
for(i in 1:length(ctr.pred)){
  ctr.pred.df[i] <- ctr.pred[[i]][2]
}

ctr.roc <- prediction(ctr.pred.df , test[,ncol(test)])               # Prediction object for ROCR
ctr.auc <- performance(ctr.roc, "auc")                               # Calculate the AUC
ctr.perf <- performance(ctr.roc, measure = "tpr", x.measure = "fpr") # Calculate performance for ROC plot
ctr.auc@y.name                                                       # Print the AUC name
ctr.auc@y.values                                                     # Print the AUC value

rm(ctr.fit, ctr.pred, ctr.pred.df, ctr.roc, ctr.auc)                 # Clear memory for further computations

#### Convert year into date for other models, ctree does not accept date inputs
train$YEARBLT <- as.Date(train$YEARBLT, "%Y")
test$YEARBLT <- as.Date(test$YEARBLT, "%Y")

############# rpart #############
# Decision trees with rpart perform poorly on our data
# Only about 10% parcels are classified as potentially bad investmemts
# Thus, the spliting criteria do not work on any input feature

# library(rpart)
# rpr.fit <- rpart(ClassLabel  ~ . , data = train)
# printcp(rpr.fit)
# plot(rpr.fit)                                     # Print the classification tree
# text(rpr.fit, pretty = TRUE)                      # print the labels of the tree
# Evaluate performance of the model
# rpr.pred <- predict(rpr.fit, test)                            # Fetch predicted values from the model
# rpr.roc <- prediction(rpr.pred[,2] , test[,ncol(test)])       # Prediction object for ROCR
# rpr.auc <- performance(rpr.roc, "auc")                        # Calculate the AUC
# rpr.perf <- performance(rpr.roc, measure = "tpr", x.measure = "fpr") # Calculate performance for ROC plot
# rpr.auc@y.name                                                # Print the AUC name
# rpr.auc@y.values                                              # Print the AUC value
# plot(rpr.perf, colorize = TRUE)                             # Plot the ROC curve

##################### RANDOM FOREST ############################

library(randomForest)
rdf.fit <- randomForest(ClassLabel  ~ . - PROPERTYZIP - MUNIDESC - NEIGHDESC,
                        data = train, na.action = na.omit) #, ntree = 100) # Fit the model
rdf.pred <- predict(rdf.fit, 
                    test[, !(colnames(test) %in% c(test$PROPERTYZIP, test$MUNIDESC, test$NEIGHDESC))], 
                    type = "prob", simply = FALSE )         # Fetch predicted values from the model
rdf.roc <- prediction(rdf.pred[,2] , test[,ncol(test)])                # Prediction object for ROCR
rdf.auc <- performance(rdf.roc, "auc")                                 # Calculate the AUC
rdf.perf.1 <- performance(rdf.roc, measure = "tpr", x.measure = "fpr") # Calculate performance for ROC plot
rdf.auc@y.name                                                         # Print the AUC name
rdf.auc@y.values                                                       # Print the AUC value
plot(rdf.fit)                                        # Show the error depending on the number of trees
varImpPlot(rdf.fit, main = NA)                       # Plot showing most important features
mtext("Relevant Features For Random Forest", 
      side=3, outer=TRUE, line = -2, cex = 1.5)      # Plot title

############# Re-train random forest with feature selection ######################

# We train the model again with only important features highlighted in the above plot
train.new <- data.frame(train$SALEPRICE, train$SCHOOLDESC, 
                        train$FINISHEDLIVINGAREA, train$LOTAREA, 
                        train$YEARBLT, 
                        train$COUNTYTOTAL, 
                        train$COUNTYBUILDING, train$FAIRMARKETTOTAL, 
                        train$LOCALTOTAL, train$COUNTYLAND, 
                        train$STYLEDESC, train$TOTALROOMS, 
                        train$GRADE, train$OWNERDESC,
                        train$BEDROOMS, train$EXTFINISH_DESC, 
                        train$CONDITIONDESC, train$CDU, train$HALFBATHS, 
                        train$BSMTGARAGE, train$FIREPLACES, 
                        train$STORIES, train$FULLBATHS,
                        train$HOMESTEADFLAG, train$HEATINGCOOLINGDESC, 
                        train$ROOFDESC, train$ClassLabel)

test.new <- data.frame(test$SALEPRICE, test$SCHOOLDESC, 
                       test$FINISHEDLIVINGAREA, test$LOTAREA, 
                       test$YEARBLT, test$COUNTYTOTAL, 
                       test$COUNTYBUILDING, test$FAIRMARKETTOTAL, 
                       test$LOCALTOTAL, test$COUNTYLAND, 
                       test$STYLEDESC, test$TOTALROOMS, 
                       test$GRADE, test$OWNERDESC,
                       test$BEDROOMS, test$EXTFINISH_DESC, 
                       test$CONDITIONDESC, test$CDU, test$HALFBATHS, 
                       test$BSMTGARAGE, test$FIREPLACES, 
                       test$STORIES, test$FULLBATHS,
                       test$HOMESTEADFLAG, test$HEATINGCOOLINGDESC, 
                       test$ROOFDESC, test$ClassLabel)

colnames(train.new) <- c('SALEPRICE', 'SCHOOLDESC', 
                        'FINISHEDLIVINGAREA', 'LOTAREA', 
                        'YEARBLT', 'COUNTYTOTAL', 
                        'COUNTYBUILDING', 'FAIRMARKETTOTAL', 
                        'LOCALTOTAL', 'COUNTYLAND', 
                        'STYLEDESC', 'TOTALROOMS', 
                        'GRADE', 'OWNERDESC',
                        'BEDROOMS', 'EXTFINISH_DESC', 
                        'CONDITIONDESC', 'CDU', 'HALFBATHS', 
                        'BSMTGARAGE', 'FIREPLACES', 
                        'STORIES', 'FULLBATHS',
                        'HOMESTEADFLAG', 'HEATINGCOOLINGDESC', 
                        'ROOFDESC', 'ClassLabel')

colnames(test.new) <- c('SALEPRICE', 'SCHOOLDESC', 
                        'FINISHEDLIVINGAREA', 'LOTAREA', 
                        'YEARBLT', 'COUNTYTOTAL', 
                        'COUNTYBUILDING', 'FAIRMARKETTOTAL', 
                        'LOCALTOTAL', 'COUNTYLAND', 
                        'STYLEDESC', 'TOTALROOMS', 
                        'GRADE', 'OWNERDESC',
                        'BEDROOMS', 'EXTFINISH_DESC', 
                        'CONDITIONDESC', 'CDU', 'HALFBATHS', 
                        'BSMTGARAGE', 'FIREPLACES', 
                        'STORIES', 'FULLBATHS',
                        'HOMESTEADFLAG', 'HEATINGCOOLINGDESC', 
                        'ROOFDESC', 'ClassLabel')

rm(train, test) # Clear memory

rdf.fit <- randomForest(ClassLabel  ~ . , data = train.new, na.action = na.omit) #,ntree = 99)
rdf.pred <- predict(rdf.fit, test.new, type = "prob", simply = FALSE)
rdf.roc <- prediction(rdf.pred[,2] , test.new[,ncol(test.new)])
rdf.auc <- performance(rdf.roc, "auc")
rdf.perf.2 <- performance(rdf.roc, measure = "tpr", x.measure = "fpr")
rdf.auc@y.name
rdf.auc@y.values

############################ PLOT THE ROC CURVES ################################
plot(ctr.perf, col = "blue", lwd=1.5)                # Conditional Inference Trees
abline(a = 0, b = 1)                                 # Print line with AUC = 0.5 to evaluate classifier performance
plot(rdf.perf.1, col = "red", lwd = 1.5, add = TRUE) # Random Forest without feature selection
plot(rdf.perf.2, col = "gray", lwd = 2, add = TRUE)  # Random Forest with feature selection

legend(x = 0.25, y = 0.2, 
       c("Conditional Inference Trees", 
         "Random Forest (No Feature Selection)",
         "Random Forest (With Feature Selection)"),  
       lty=c(1, 1, 1), 
       lwd=c(1.5, 1.5, 2),
       col=c("blue", "red", "gray"))
