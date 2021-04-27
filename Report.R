#Loading the already installed libray containing the dataset.
library("mlbench")

# Loading the dataset from mlbench package.
data("BreastCancer")

# removing instances with missing values
BreastCancer <- na.omit(BreastCancer)

# Summary of breast cancer type in the dataset
summary(BreastCancer$Class)

data("BostonHousing2")

#The dataset description indicates that medv is incorret and cmedv should be used.
BostonHousing2$medv <- NULL

#Some variables represent the exact information. Tract , town , lon and lat. 
#They all refer to the geographical location of the how in question.
#Only lon and lat will be kept as they are numerical values. The others are 
#character values.
BostonHousing2$town <- NULL
BostonHousing2$tract <- NULL

#classification data
set.seed(12345)
index = sort(sample(nrow(BreastCancer), nrow(BreastCancer)*.8))
train_class = BreastCancer[index , ]
train_class$Id <- NULL
test_class = BreastCancer[-index , ]
test_class$Id <- NULL

#Regression data
set.seed(12345)
index = sort(sample(nrow(BostonHousing2), nrow(BostonHousing2)*.8))
train_reg = BostonHousing2[index , ]
test_reg = BostonHousing2[-index , ]

num_noise_var = 5 
set.seed(1234)
for ( i in 1: num_noise_var) {
  train_class[,10+i] <- rnorm(nrow(train_class) , mean = i , i)
  test_class[,10+i] <- rnorm(nrow(test_class) , mean = i , i)
  train_reg[,16+i] <- rnorm(nrow(train_reg) , mean = i , i)
  test_reg[,16+i] <- rnorm(nrow(test_reg) , mean = i , i)
}

#First load the package
library("randomForest")

#Fitting the randomForest, note that importance is set as true for future reference.
rf_class <- randomForest(Class ~ . , data = train_class , importance = TRUE )

#Predicting on the test set
pred_bench_class <- predict(rf_class , newdata = test_class , type = "response")

#good classification rate
Bench_class <- length(which(pred_bench_class == test_class$Class )) / nrow(test_class)

#Confusion matrix below present the results of the classification task
c_mat_1 <- table(pred_bench_class , test_class$Class)

#Fitting the random forest 
rf_reg <- randomForest(cmedv~. , data=train_reg , importance = TRUE) 

pred_bench_reg <- predict(rf_reg , newdata = test_reg , type="response" )

MSE_bench <- mean((pred_bench_reg - test_reg$cmedv)^2)
MAE_bench <- mean(abs(pred_bench_reg - test_reg$cmedv))

# Computing using mean decrease in accuracy 
imp_class_1 <- importance(rf_class , type = 1 , scale = TRUE )

# Computing using mean decrease in node impurity
imp_class_2 <- importance(rf_class , type = 2 , scale = TRUE)

# Visualing the results using the plot function most important.
par(cex.main = 0.8 , cex.axis = 0.8 , cex.lab = 0.8 , cex.main = 0.8)
varImpPlot(rf_class , sort = TRUE , n.var = 5 , 
           main = "Variable importance : classification task" , 
           scale = TRUE)
# Computing variable importance for regression task : increase in MSE
imp_reg_1 <- importance(rf_reg , type = 1 , scale = TRUE)

# Computing variable importance for regression task : increase in Node purity
imp_reg_2 <- importance(rf_reg , type = 2 , scale = TRUE)

# Plotting the results
par(cex.main = 0.8 , cex.axis = 0.8 , cex.lab = 0.8 , cex.main = 0.8)
varImpPlot( rf_reg , n.var = 5 , main = "Variable importance : regression task" , 
            scale = TRUE)

# First , loading the package
library("randomForestSRC")

#Classification
# Fitting a tree using the default parameters
set.seed(1234)
rfsrc_class <- rfsrc( Class ~ . , data = train_class , ntree = 500 )

#Predictions 
pred_bench_src_class <- predict(rfsrc_class , newdata =  test_class)

#The output when using randomForest src is a list of objects. It only outputs probs.
#The following code transforms the output to the same format as the test data.
pred_bench_src_class <- ifelse(pred_bench_src_class$predicted[,1] > 0.5 ,
                               "benign" , "malignant")
pred_bench_src_class <- as.factor(pred_bench_class)

Bench_rfsrc_class <- length(which(pred_bench_src_class == test_class$Class )) /+
  nrow(test_class)

# Fitting the regression model
set.seed(1234)
rfsrc_reg <- rfsrc( cmedv ~ . , data = train_reg , ntree = 500 )

#Making predictions
pred_bench_src_reg <- predict(rfsrc_reg , newdata = test_reg  )

#Evaluating performance
MSE_bench_src <- mean((pred_bench_src_reg$predicted - test_reg$cmedv)^2)
MAE_bench_src <- mean(abs(pred_bench_src_reg$predicted - test_reg$cmedv))

#The vimp() function allows to select three different types of error computation.
#Classification task first

#Permutation
imp_src_class_1 <- vimp(rfsrc_class , importance = "permute" , block.size = 1 )
#The following code gets the overall importance of each variable instead 
#of class specific
imp_src_class_1 <- sort(imp_src_class_1$importance[,1])

#Random 
imp_src_class_2 <- vimp(rfsrc_class , importance = "random" , block.size = 1 )
imp_src_class_2 <- sort(imp_src_class_2$importance[,1])

#Opposite split
imp_src_class_3 <- vimp(rfsrc_class , importance = "anti" , block.size = 1 )
imp_src_class_3 <- sort(imp_src_class_3$importance[,1])

#Plot to compare the importance per method, NEEDS A LITTLE ATTENTION
par(mfrow = c(1,3) , cex.axis = 0.8 , cex.lab = 0.8 , cex.main = 0.8)
dotchart(imp_src_class_1[10:14] , main = "Permutation" , xlim = c(0 , 0.2))
dotchart(imp_src_class_2[10:14] , main = "Random" , xlim = c(0 , 0.2))
dotchart(imp_src_class_3[10:14] , main = "Opposite split" , xlim = c(0 , 0.2))

# Permutation on the regression data
imp_src_reg_1 <- vimp(rfsrc_reg , importance = "permute" , block.size = 1  )
imp_src_reg_1 <- sort(imp_src_reg_1$importance)
# Random 
imp_src_reg_2 <- vimp(rfsrc_reg , importance = "random" , block.size = 1  )
imp_src_reg_2 <- sort(imp_src_reg_2$importance)
#opposite
imp_src_reg_3 <- vimp(rfsrc_reg , importance = "anti" , block.size = 1  )
imp_src_reg_3 <- sort(imp_src_reg_3$importance)

#Ploting
par(mfrow = c(1,3) , cex.axis = 0.8 , cex.lab = 0.8 , cex.main = 0.95)
dotchart(imp_src_reg_1[16:20] , main = "Permutation" , xlim = c(0 , 120))
dotchart(imp_src_reg_2[16:20] , main = "Random" , xlim = c(0 , 120))
dotchart(imp_src_reg_3[16:20] , main = "Opposite split" , xlim = c(0 , 120))

#Block size varying [1 , 10 , 50 , 100 , 500]
block_size = c(1 , 10 , 50 , 100 , 200 , 500)
imp_block_size = list()
for (i in block_size){
  imp_block_size[[i]] = vimp(rfsrc_class , importance = "permute" , block.size = i)
  imp_block_size[[i]] = sort(imp_block_size[[i]]$importance[,1])
}

par(mfrow = c(2 , 3) , cex.axis = 1.1 , cex.lab = 0.8 , cex.main = 1.1)
# We need to find a way to plot them together
for( i in block_size) {
  dotchart(imp_block_size[[i]][10:14] , 
           main = paste("block size" , as.character(i)) , xlim = c(0 , 0.15))
}

# Computing variable importance for the classification task
imp_hold_class <- holdout.vimp( Class ~. , data= train_class , ntree = 500 ,
                                block.size = 1 , ntime = 100)

imp_hold_class <- sort(imp_hold_class$importance[,1])

# Computing cariable importance for the regression task
imp_hold_reg <- holdout.vimp(cmedv ~. , data= train_reg , ntree = 500 ,
                             block.size = 1 , ntime = 100)

imp_hold_reg <- sort(imp_hold_reg$importance)

# Plot of the results
par(mfrow = c(1,2) , cex.axis = 0.8 , cex.lab = 0.3 , cex.main = 0.75)
dotchart(imp_hold_class[10:14] , labels = rownames(imp_hold_class) , 
         main = "Classifcation task" , xlim = c(min(imp_hold_class[10:14]) - 0.001 , 
                                                max(imp_hold_class[10:14] + 0.001) ) )
dotchart(imp_hold_reg[16:20] , labels=rownames(imp_hold_reg) ,
         main = "Regression task" , xlim = c(min(imp_hold_reg[16:20])-0.5 , 
                                             max(imp_hold_reg[16:20])+ 0.5))
#Create the ranking
# 9 variables in the classification task
rank <- seq(1 , length(imp_class_1) , 1)

# sort the vectors that are not
covariates <- rownames(imp_class_1)
data_1 <- as.data.frame(cbind(covariates , imp_class_1))
rownames(data_1) <- seq(1 , nrow(data_1) , 1)
data_1$MeanDecreaseAccuracy <- as.numeric(as.character(data_1$MeanDecreaseAccuracy ))
data_1 <- data_1[order(data_1$MeanDecreaseAccuracy , decreasing = TRUE) , ]
data_1$classic_rank <- rank  
data_1$MeanDecreaseAccuracy <- NULL

# Vimp function
imp_src_class_1 <- sort(imp_src_class_1 , decreasing = TRUE)
data_2 <-cbind(imp_src_class_1 , rank)
covariates_2 <- rownames(data_2)
data_2 <- as.data.frame(data_2 )
data_2$covariates <- covariates_2
data_2$imp_src_class_1 <- NULL
colnames(data_2) <- c("Vimp_rank" , "covariates")

# holdout function
imp_hold_class <- sort(imp_hold_class , decreasing = TRUE)
data_3 <-cbind(imp_hold_class , rank)
covariates_3 <- rownames(data_3)
data_3 <- as.data.frame(data_3)
data_3$covariates <- covariates_3
data_3$imp_hold_class <- NULL
colnames(data_3) <- c("Hold_rank" , "covariates")

#Merging
data_m <- merge(data_1 , data_2 , by="covariates")
data_m <- merge(data_m , data_3 , by="covariates")

# plot hold-classic , hold-vimp , vimp-classic
par(mfrow = c(1 , 3) , cex.main=0.8  , cex.axis = 1 , cex.lab = 1.1)
plot(data_m$Hold_rank , data_m$classic_rank , 
     xlab = "Holdout ranking" , ylab = "classic ranking")
axis(2, at = seq(1, 9, by = 1), las=0) ; axis(1, at = seq(1, 9, by = 1), las=0)
plot(data_m$Hold_rank , data_m$Vimp_rank , 
     xlab = "Holdout ranking" , ylab="Vimp ranking")
axis(2, at = seq(1, 9, by = 1), las=0) ; axis(1, at = seq(1, 9, by = 1), las=0)
plot(data_m$Vimp_rank , data_m$classic_rank, 
     xlab ="Vimp ranking" , ylab="classic ranking")
axis(2, at = seq(1, 9, by = 1), las=0) ; axis(1, at = seq(1, 9, by = 1), las=0)
mtext("Ranking comparison of variable importance" , outer = TRUE , line = -1.5)

library("Boruta")
library("VSURF")
library("varSelRF")
library("vita")

# Using Boruta to make variable selection
Boruta_class <- Boruta(Class ~., data = train_class)

Boruta_reg <- Boruta(cmedv ~.  , data = train_reg)

# The following variables are the onese Boruta's method deemed important
print("Boruta's selection for classification task")
Boruta_class$finalDecision

print("Boruta's selection for regression task")
Boruta_reg$finalDecision

#Fitting the model without rejected variables
# Classification and regression
boruta_rf_class <- randomForest(Class~. - V11 - V12 -V13 -V14 -V15 ,
                                data=train_class)

boruta_rf_reg <- randomForest(cmedv~.  -chas -V17 -V18 -V19 -V20 -V21 , 
                              data=train_reg)

#Prediction and performance measure
pred_boruta_class <- predict(boruta_rf_class , newdata = test_class , type = "response")

pred_boruta_reg <- predict(boruta_rf_reg , newdata = test_reg , type="response")

#good classification rate
boruta_rate_class <- length(which(pred_bench_class == test_class$Class )) /+
  nrow(test_class)

#Confusion matrix below present the results of the classification task
c_mat_2 <- table(pred_boruta_class , test_class$Class)

#MSE and MAE
MSE_Boruta <- mean(( pred_boruta_reg - test_reg$cmedv)^2)
MAE_Boruta <- mean(abs(pred_boruta_reg - test_reg$cmedv))

# using the default parameters to perform variable selection
vsurf_class <- VSURF(Class ~. , data=train_class , parallel=TRUE ,
                     ncores=4 , verbose=FALSE)

# On the first step the following variables were removed
colnames(train_class[,vsurf_class$varselect.thres])

# On the first step the following variables were removed
colnames(train_class[,vsurf_class$varselect.interp])

# On the first step the following variables were removed
colnames(train_class[,vsurf_class$varselect.pred])

# using the default parameters to perform variable selection
vsurf_reg <- VSURF(cmedv ~. , data=train_reg , parallel=TRUE , ncores=4 , VERBOSE = FALSE)

colnames(train_reg[,vsurf_reg$varselect.pred])

#Fiiting on data
rf_vsurf_class <- randomForest(Class~ Cell.size + Bare.nuclei + Cell.shape +
                                 Normal.nucleoli , data=train_class)

rf_vsurf_reg <- randomForest(cmedv ~ b + nox + chas + zn , data=train_reg )

#Pre on the classification task
#Prediction and performance measure
pred_vsurf_class <- predict(rf_vsurf_class , newdata = test_class , type ="response")

pred_vsurf_reg <- predict(rf_vsurf_reg , newdata = test_reg , type="response")

#good classification rate
vsurf_rate_class <- length(which(pred_vsurf_class == test_class$Class )) /+
  nrow(test_class)

#Confusion matrix below present the results of the classification task
c_mat_3 <- table(pred_vsurf_class , test_class$Class)

#MSE and MAE
MSE_vsurf <- mean(( pred_vsurf_reg - test_reg$cmedv)^2)
MAE_vsurf <- mean(abs(pred_vsurf_reg  - test_reg$cmedv))

#Loading the library
library("varSelRF")

# This function takes x as a matrix
x_train_class <- train_class; x_test_class <- test_class
x_train_class$Class <- NULL ; x_test_class$Class <- NULL
y_train_class <- train_class$Class ; y_test_class <- test_class$Class

#Classification only. 
varselrf_class <- varSelRF(x_train_class , y_train_class , ntree = 500 ,
                           recompute.var.imp = TRUE ,
                           keep.forest = TRUE , verbose = FALSE)

varselrf_class$selected.vars

# Fitting the model using the selected variables
varsel_rf <- randomForest(Class~ Bare.nuclei + Cell.shape + Cell.size + Normal.nucleoli ,
                          data=train_class )

#Prediction and performance measure
pred_varsel_class <- predict(varsel_rf , newdata = test_class , type ="response")


#good classification rate
varsel_rate_class <- length(which(pred_varsel_class == test_class$Class )) /+
  nrow(test_class)

#Confusion matrix below present the results of the classification task
c_mat_4 <- table(pred_varsel_class , test_class$Class)