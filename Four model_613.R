trainData <- read.csv("C:\\Users\\shrad\\Downloads\\data613.csv")
#PCR-----------------------------------------------------------------------

set.seed(100)
pcr_model <- pcr(Hospitalizations~., data = data1, scale = TRUE, validation = "CV")
summary(pcr_model)
# Plot the root mean squared error
validationplot(pcr_model)
# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")
# Plot the R2
validationplot(pcr_model, val.type = "R2")
set.seed(100)
index<-sample(nrow(data),nrow(data1)*0.80)
training<-data1[index,]
y_test<-data1$Hospitalizations[-index]

model <- pcr(Hospitalizations~., data=training, scale=TRUE, validation="CV")
pcr_pred <- predict(model, testing, ncomp=15)
sqrt(mean((pcr_pred - y_test)^2))
#--------------------------------------------------------------------------FWD SELECTION

library(leaps)
regfit.fwd<-regsubsets(Hospitalizations~., data=trainData,method="forward",nvmax = 258)
summary.fwd<- summary(regfit.fwd)

set.seed(1)
train <-  sample(1:dim(trainData)[1], dim(trainData)[1]*0.7)
test <- -train
train_613 <- trainData[train, ]
test_613 <- trainData[-train, ] 
test.mat <- model.matrix(Hospitalizations ~., data = test_613)
val.errors <- rep (NA , 258)
for (i in 1:258) {
  coefi <- coef(regfit.fwd,i)
  pred <- test.mat[, names ( coefi )] %*% coefi
  val.errors [ i] <- mean (( trainData$Hospitalizations[test] - pred ) ^2)
}
which.min(val.errors)
min(val.errors)


which.max(summary.fwd$rsq)
which.min(summary.fwd$cp)
which.min(summary.fwd$bic)
which.max(summary.fwd$adjr2)

coef(regfit.fwd,40)

#--------------------------SVM---------------

library(e1071)
library(ISLR)
library(dplyr)
SVM_polynomial <- svm(Hospitalizations ~., data = train_613, kernel = "polynomial", cost=0.01,degree=2)
summary(SVM_polynomial)
SVM_pred3 <- predict(SVM_polynomial, train_613)
svm.test.pred3 <- predict(SVM_polynomial, test_613)
E_train <- mean(SVM_pred3 != train_613$Hospitalizations)
E_test <- mean(svm.test.pred3 != test_613$Hospitalizations)
E_train
E_test 

#------------------------------------------Random Forest
library(randomForest)
Random_model <- randomForest(Hospitalizations ~ ., data = train_613, mtry = 15, importance = TRUE, na.action = na.omit)
summary(Random_model)
Random_model
png(file = "randomForestRegression.png")
plot(Random_model)
dev.off()
plot(Random_model)
# Variable Importance Plot
varImpPlot(Random_model,sort = T,main="Variable Importance",n.var=5)
varImpPlot(Random_model)
pred_rf <- predict(Random_model, test_613)
test_pred_rf <- as_tibble(cbind(test_613, pred_rf))


glimpse(test_pred_rf)

RMSE_rf_TEST <- yardstick::rmse(test_pred_rf, truth=Hospitalizations, estimate=pred_rf)

RMSE_rf_TEST
