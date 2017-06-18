library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

#setwd("C:/Users/pooja/Desktop/Spring 17/Healthcare Analytics/Project")
setwd("C:/Users/pooja/Desktop/Spring 17/Visual Analytics/Project")
#db <- read.csv("db_data_cleaned_v0.6.csv", header = TRUE)
db <- read.csv("db_data_cleaned_temp.csv", header = TRUE)
View(db)
str(db)
attach(db)

cols <- c("REP_diag_1", "REP_diag_2", "REP_diag_3", "REP_admission_source_id", "REP_admission_type_id", "REP_discharge_disposition_id")
db[,cols] <- data.frame(apply(db[cols], 2, as.factor))

set.seed(123)
dtype<-sample(2,nrow(db),replace=TRUE,prob=c(0.6, 0.4))
sum(dtype==1) # size of training set
sum(dtype==2) # size of test set
db_train <- db[dtype == 1,]
db_valid <- db[dtype == 2,]

rpart.fit <- rpart(REP_readmitted ~ . - patient_nbr - encounter_id, data = db_train, control = rpart.control(minsplit = 5, minbucket = 10, cp = 0.0013), method = "class" )
plot(rpart.fit)
text(rpart.fit, pretty = 1)
fancyRpartPlot(rpart.fit)

names(rpart.fit)
rpart.fit$variable.importance

prediction <- predict(rpart.fit, db_valid, type = "class")
table(prediction, db_valid$REP_readmitted)

set.seed(555)
rf.fit <- randomForest(REP_readmitted ~ . - patient_nbr - encounter_id, data = db_train, importance = TRUE, ntree = 500)
varImpPlot(rf.fit)
partialPlot(rf.fit, db_train, REP_diag_1)
partialPlot(rf.fit, db_train, REP_discharge_disposition_id)
partialPlot(rf.fit, db_train, REP_admission_source_id)

pred <- predict(rf.fit, db_valid)
table(pred, db_valid$REP_readmitted)

library(nnet)
ann.fit <- nnet(REP_readmitted ~ . - patient_nbr - encounter_id, data = db_train, size = 5, maxit = 120)
ann_pred <- predict(ann.fit, db_valid, type = "class")
table(ann_pred, db_valid$REP_readmitted)
install.packages("NeuralNetTools")
library(NeuralNetTools)
garson(ann.fit)
olden(ann.fit)
garson(ann.fit, bar_plot = FALSE)
