library(randomForest)

data <- read.csv(file= "C://HEC/Advanced StatLearning/Project/RandomForestPaper/data/CC_data.csv", header = TRUE)

summary(data)

data$target_0 = as.factor(data$target_0)

#Split trainset into train and test set
set.seed(123)
index_train <- sample(1:nrow(data), 4/5 * nrow(data))
train_set <- data[index_train, ]
valid_set <- data[-index_train, ]


#Tuning mtry parameter
tuneRF(train_set[,2:29],train_set[,30], stepFactor = 1.5,
       plot=TRUE, trace=TRUE, doBest=TRUE)

# random forest 
RF <- randomForest(target_0~. -ID_TRAIN, data = train_set , 
             type = "classification", mtry = 13 ,
             importance = TRUE)

#Output variable importance based on MSE increase %
importance <- importance(RF, type=1)
#Plot of variable importance for random forest
varImpPlot <-  varImpPlot(RF,type=1, main="VARIABLE IMPORTANCE PLOT")



