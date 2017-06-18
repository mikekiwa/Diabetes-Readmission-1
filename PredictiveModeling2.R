
set.seed(60)

library(readr)
library(nnet)
library("randomForest")
library(caret)
library(e1071)
library(rpart)

# Read the table as factors so categorical variales are confirmed treated as such
# diabetes <- read.table("C:/Users/Moyo/Downloads/hcip2.csv", header=TRUE, sep =",")
hcip2 <- read.table("C:/Users/Moyo/Downloads/hcip2.csv", header=TRUE, sep =",")
hcip1 <- read.table("C:/Users/Moyo/Downloads/hcip1.csv", header=TRUE, sep =",")
d <- cbind(hcip1, hcip2)


# Splitting the data
dt1 = sort(sample(nrow(d), nrow(d)*.60))
dt_train<- d[dt1,]
dt_test<- d[-dt1,]

# Using NNET package
ann <- nnet(REP_readmitted ~ ., data=hcip.train, size=5, maxit=120) 
ann_predict <- predict(ann, hcip.test, type = "class")
confusionMatrix(ann_predict, hcip.test$REP_readmitted)


# NNet package F-score
ann_nnet <- as.factor(ann_predict)
precision_nnet <- posPredValue(ann_caret, ann_y)
recall_nnet <- sensitivity(ann_nnet, ann_y)
F1_nnet <- (2 * precision_nnet * recall_nnet) / (precision_nnet + recall_nnet)
F1_nnet


### Using the caret package

ann_caret <- train(REP_readmitted ~ ., data=hcip.train, method="nnet")
ann_caret_pred <- predict(ann_caret, hcip.test)
confusionMatrix(ann_caret_pred, hcip.test$REP_readmitted)

#F-measure
#Caret Package
ann_y <- as.factor(hcip.test$REP_readmitted)
ann_caret <- as.factor(ann_caret_pred)

precision_caret <- posPredValue(ann_caret, ann_y)
recall_caret <- sensitivity(ann_caret, ann_y)

F1_caret <- (2 * precision_caret * recall_caret) / (precision_caret + recall_caret)
F1_caret


# Variable Importance from ANN
require(devtools)

#import 'gar.fun' from beckmw's Github - this is Garson's algorithm
source_gist('6206737')

#use the function on the model created above
gar.fun('y',mod1)
varImp(ann_predict)

devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")

nnPredictions <- predict(nnModel, modelTest)

# SVM method
svmstart <- Sys.time()
svm.model <- svm(REP_readmitted ~ ., data = hcip.train)   #, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, hcip.testmod[,-42])
svmend <- Sys.time()

hcip.testmod <- read.table("C:/Users/Moyo/Downloads/hciptest_mod.csv", header=TRUE, sep =",")
hcip.trainmod <- read.table("C:/Users/Moyo/Downloads/hciptrain_mod.csv", header=TRUE, sep =",")


dt1 = sort(sample(nrow(d), nrow(d)*.70))
svm_train<- d[dt1,]
svm_test<- d[-dt1,]

# Ran SVM again
svmstart <- Sys.time()
svm.model_mod2 <- svm(REP_readmitted ~ ., data = svm_train[,-9,-10])   #, cost = 100, gamma = 1)
svm.pred2 <- predict(svm.model_mod2, svm_test[,-9,-10])
svmend <- Sys.time()

confusionMatrix(svm.pred2, svm_test$REP_readmitted)

summary(svm.model_mod2)

# SVM F-score
svm_vals <- as.factor(svm.pred2)
precision_svm <- posPredValue(svm_vals, ann_y)
recall_svm <- sensitivity(svm_vals, ann_y)
F1_svm <- (2 * precision_svm * recall_svm) / (precision_svm + recall_svm)
F1_svm



# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM

#Important variables for ANN
source_url('https://gist.githubusercontent.com/fawda123/6206737/raw/d6f365c283a8cae23fb20892dc223bc5764d50c7/gar_fun.r')

#create a pretty color vector for the bar plot
cols<-colorRampPalette(c('lightgreen','lightblue'))#(num.vars)

#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')
ann_plot <- gar.fun(dt_train$REP_readmitted, ann, abs(rel.imp) >= 0.5)

ann_plot <- gar.fun(dt_train$REP_readmitted, ann)

# closeAllConnections()
# rm(list=ls())




library(readr)
library(glmnet)
library(pscl)
library(caret)

# The datasets were split into numerical and categorical before to scale the numeric
# variables but decided not to anymore

hcip2 <- read.table("C:/Users/Moyo/Downloads/hcip2.csv", header=TRUE, sep =",")
hcip1 <- read.table("C:/Users/Moyo/Downloads/hcip1.csv", header=TRUE, sep =",")
d <- cbind(hcip1, hcip2)

dt1 = sort(sample(nrow(d), nrow(d)*.75))
glm.train<- d[dt1,]
glm.test<- d[-dt1,]

# model <- glm (REP_readmitted~., data = glm.train, family = binomial)
# summary(model)
# 
# 1-pchisq(99565, 77123)
# 
# 1-pchisq(107033, 77207)
# 
# 1-pchisq(107033-99565, 77207-77123)


model2 <- glm (REP_readmitted~gender+age+REP_diag_1+REP_diag_3+rosiglitazone+insulin+num_procedures
               +num_medications+number_outpatient+number_emergency+number_inpatient+number_diagnoses
               +diabetesMed +IMP_race+REP_discharge_disposition_id+
                 num_lab_procedures+glipizide, data = glm.train, family = binomial)
summary(model2)


# time_in_hospital
# num_lab_procedures
# glipizideNo
#REP_admission_source_id

#Evaluating the table of deviance
anova(model2, test="Chisq")

#McFadden R-squared
pR2(model2)


pred_model2 <- predict(model2, glm.test)

pred_model2 <- ifelse(pred_model2 > 0.5,"Yes","No")
misClasificError <- mean(pred_model2 != glm.test$REP_readmitted)
print(paste('Accuracy',1-misClasificError))
table(pred_model2, glm.test$REP_readmitted)

###########Model 2 Evaluation##############
pred2 <- as.factor(pred_model2)
precision2 <- posPredValue(pred2, y)
recall2 <- sensitivity(pred2, y)

F1_2 <- (2 * precision2 * recall2) / (precision2 + recall2)

F1_2


#######################################################################################


model3 <- glm (REP_readmitted~insulin+num_medications+number_outpatient+number_emergency+number_inpatient+number_diagnoses
               +diabetesMed, data = glm.train, family = binomial)
summary(model3)


# time_in_hospital
# num_lab_procedures
# glipizideNo
#REP_admission_source_id

#Evaluating the table of deviance
anova(model3, test="Chisq")

#McFadden R-squared
pR2(model3)


pred_model3 <- predict(model3, glm.test)

pred_model3 <- ifelse(pred_model3 > 0.5,"Yes","No")
misClasificError3 <- mean(pred_model3 != glm.test$REP_readmitted)
Acc3 <- 1-misClasificError3
table(pred_model3, glm.test$REP_readmitted)


y <- as.factor(glm.test$REP_readmitted)
pred3 <- as.factor(pred_model3)
precision <- posPredValue(pred3, y)
recall <- sensitivity(pred3, y)

F1 <- (2 * precision * recall) / (precision + recall)


