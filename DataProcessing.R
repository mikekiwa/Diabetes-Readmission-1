setwd("C:/Users/pooja/Desktop/Spring 17/Healthcare Analytics/Project")
db_data <- read.csv("Original_data.csv", header = TRUE)
View(db_data)
attach(db_data)
db_data1 <- subset(db_data, ,-c(6,12,40,41))
View(db_data1)
rm(db_data)

attach(db_data1)
sum(diag_2 == "?")
sum(diag_3 == "?")

str(diag_2)
table(diag_2)
diag_2 <- as.character(diag_2)
diag_2[diag_2 == "?"]<-0
sum(diag_2 == 0)
diag_2 <- as.factor(diag_2)
levels(diag_2)
str(diag_2)
sum(is.na(diag_2))
sum(diag_2 == "?")

str(diag_3)
diag_3 <- as.character(diag_3)
diag_3[diag_3 == "?"]<-0
diag_3 <- as.factor(diag_3)
str(diag_3)
sum(diag_3 == "?")
sum(is.na(diag_3))
sum(diag_3 == 0)

sum(is.na(num_procedures))
median(num_procedures, na.rm = TRUE)
num_procedures[is.na(num_procedures)]<-1
sum(is.na(num_procedures))

setwd("C:/Users/pooja/Desktop/Spring 17/Healthcare Analytics/Project")
db_data <- read.csv("db_data_v0.2.csv", header = TRUE)
db_pres <- db_data[(db_data$race != ""),]
db_abs <- db_data[(db_data$race == ""),]
db_samp <- db_data[sample(1:nrow(db_pres), 10000, replace = FALSE),]

race.fit_pres <- rpart(race ~ gender + age + admission_type_id + admission_source_id + discharge_disposition_id, data = db_pres, method = "anova")
race.fit_pres2 <- rpart(race ~ gender + age + admission_type_id + admission_source_id + discharge_disposition_id + time_in_hospital + num_lab_procedures + num_medications + num_procedures + number_outpatient + number_emergency + number_inpatient + number_diagnoses, data = db_pres, method = "anova")

db$admission_source_id <- as.factor(db$admission_source_id)
db$admission_type_id <- as.factor(db$admission_type_id)
db$discharge_disposition_id <- as.factor(db$discharge_disposition_id)
library(corrplot)
a <- cor(db[9:16])
a
corrplot(a)