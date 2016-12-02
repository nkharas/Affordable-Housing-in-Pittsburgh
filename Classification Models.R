# Author: Nick Kharas
# Description : Classification Models to understand investor behavior

# read.csv("./filename.csv", colClasses = c("factor", "factor", "factor", "character"...... )

import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

fillna <- function(x){
  if(is.na(x)){
    if(is.numeric(x)){
      x <- 0
    }
    else{
      x <- ''
    }
    
  }
  else {
    x <- ''
  }
  return(x)
}

# total <- import.csv('BestFile.csv')
data.set <- import.csv('property_assessments_clean.csv')


# data.set <- data.frame(total$PROPERTYZIP_x, total$MUNICODE_x, total$SCHOOLCODE_x, total$NEIGHCODE, total$TAXCODE, total$OWNERCODE, total$CLASS, total$USECODE, total$LOTAREA, total$SALECODE_x, total$ClassLabel)

data.set$PROPERTYZIP <- as.factor(data.set$PROPERTYZIP)
data.set$isVacant <- as.factor(data.set$isVacant)
data.set$ClassLabel <- as.factor(data.set$ClassLabel)
data.set$YEARBLT <- as.Date(as.character(data.set$YEARBLT), "%Y")

####################### PREPARE DATA FOR MODELING #######################################
# Begin modeling from here

library(ROCR)

n.row <- nrow(data.set)                  # Number of rows in the data set
n.col <- ncol(data.set)            # Index of dependent variable 
                                         # Always last variable in the data set
data.set <- data.set[sample(n.row),]     # Shuffle the data set, change the order of the rows, 
                                         # Remove possible bias in data selection
train.rows <- I( round(n.row * (4/5) ) ) # 80% data for training

# Divide the data into training and testing
train <- data.set[0:train.rows , ]
test <- data.set[I(train.rows + 1) : n.row , ]

formula <- paste(colnames(data.set[n.col])," ~ .")

####################### LOGISTIC REGRESSION ########################################

lgm.fit <- glm(formula = total.ClassLabel  ~ ., data = train, family=binomial(link = "logit"))

####################### DECISION TREES #############################################

############# rpart #############
library(rpart)

rpr.fit <- rpart(ClassLabel  ~ . , data = train)

printcp(rpr.fit)
plot(rpr.fit)                                       # Print the classification tree
text(rpr.fit, pretty = TRUE)                        # print he labels of the tree

# Evaluate performance of the model
rpr.pred <- predict(rpr.fit, test)                  # Fetch predicted values from the model
rpr.roc <- prediction(rpr.pred[,2] , test[,n.col]) # Prediction object for ROCR
rpr.auc <- performance(rpr.roc, "auc")             # Actual AUC value
rpr.perf <- performance(rpr.roc, measure = "tpr", x.measure = "fpr") # For the plot
rpr.auc@y.name                                      # Print the AUC
rpr.auc@y.values                                    # Print the value of the AUC
plot(rpr.perf)                                      # Plot the AUC curve

############# ctree #############
library(party)

ctr.fit <- ctree(total.ClassLabel  ~ . , data = train)
plot(ctr.fit)

##################### RANDOM FOREST ############################

library(randomForest)
rdf.fit <- randomForest(ClassLabel  ~ . - PROPERTYZIP - MUNIDESC - NEIGHDESC,
                        data = train, na.action = na.omit,
                        ntree = 225)
rdf.pred <- predict(rdf.fit, 
                    test[, !(colnames(test) %in% c(test$PROPERTYZIP, test$MUNIDESC, test$NEIGHDESC))], 
                    type = "prob" )
rdf.roc <- prediction(rdf.pred[,2] , test[,n.col])
rdf.auc <- performance(rdf.roc, "auc")
rdf.perf <- performance(rdf.roc, measure = "tpr", x.measure = "fpr")
rdf.auc@y.name
rdf.auc@y.values
plot(rdf.perf)
imp <- data.frame(importance(rdf.fit) )
varImpPlot(rdf.fit)

############### BORUTA FEATURE SELECTION #######################
library(Boruta)

data.copy <- data.set[rowSums(is.na(data.set)) == 0,]

# data.copy[is.na(data.copy)] <- 0
# data.copy[data.copy == ""] <- 0

X <- data.copy[,-n.col]
Y <- data.copy[,n.col]

set.seed(123)
boruta.train<-Boruta(X, Y)
