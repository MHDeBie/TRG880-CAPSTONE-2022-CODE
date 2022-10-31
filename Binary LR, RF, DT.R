library(dplyr)
library(pROC)
library(rpart.plot)
library(rpart)
library(randomForest)
library(caTools)
library(ROCR)
library(caret)

# Data prep
data_no_missing <- read.csv("/content/data_no_missing.csv")


data_no_missing$STATUS_DATE<-as.numeric(as.Date(data_no_missing$STATUS_DATE))

data_no_missing$COMMENCEMENT_DATE<-as.numeric(as.Date(data_no_missing$COMMENCEMENT_DATE))


data_no_missing$PRODUCT_CODE<-factor(data_no_missing$PRODUCT_CODE)
data_no_missing$PAYMENT_MODE<-factor(data_no_missing$PAYMENT_MODE)

data_no_missing$STATUS<-factor(data_no_missing$STATUS)
data_no_missing$ID_DOCUMENT_CODE <-factor(data_no_missing$ID_DOCUMENT_CODE)

data_no_missing$MARITAL_STATUS<-factor(data_no_missing$MARITAL_STATUS)  
data_no_missing$GENDER1<-factor(data_no_missing$GENDER1)

data_no_missing$INSURED_ID_DESCRIPTION <-factor(data_no_missing$INSURED_ID_DESCRIPTION)
data_no_missing$GENDER <-factor(data_no_missing$GENDER)
data_no_missing$INSURED_MARITAL_STATUS  <-factor(data_no_missing$INSURED_MARITAL_STATUS )

data_no_missing$ID_DOCUMENT    <-factor(data_no_missing$ID_DOCUMENT) 
data_no_missing$INSURED_GENDER<-factor(data_no_missing$INSURED_GENDER)
summary(data_no_missing)
head(data_no_missing)


after_feature=data_no_missing %>% select(STATUS_DATE, PAYPOINT_NAME, COMMENCEMENT_DATE,
                                         PAYMENT_MODE,PRODUCT_CODE,INCOME,TERM,PREMIUM,
                                         PAYER_DATE_OF_BIRTH,INSURED_DATE_OF_BIRTH,STATUS)

after_feature=after_feature %>%
  mutate_at(c(1,2,3,6,7,8,9,10), scale)

# Test and Train
set.seed(1)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(after_feature), replace=TRUE, prob=c(0.7,0.3))
train  <- after_feature[sample, ]
test   <- after_feature[!sample, ]

head(test)

options(scipen=999)
######################################Logistic Reg
LR2<-glm(STATUS~., train, family="binomial")
summary(LR2)

predict_reg <- predict(LR2, test[,-c(11)], type = "response")
#predict_reg  
ROCR_perf_test <- performance(prediction(predict_reg, test$STATUS),'tpr','fpr')


# Changing probabilities
predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix

confusionMatrix(factor(predict_reg), factor(test$STATUS))
#missing_classerr <- mean(predict_reg != test2$STATUS, na.rm = T)
#print(paste('Accuracy FOR LOG REG=', 1 - missing_classerr))


# Random Forest and Decision Tree

auc(test$STATUS,as.vector(as.numeric(predict_reg)))

data_no_missing_dates <- read.csv("/content/data_no_missing_dates.csv")

data_no_missing_dates$STATUS_DATE<-as.Date(data_no_missing_dates$STATUS_DATE)

data_no_missing_dates$COMMENCEMENT_DATE<-as.Date(data_no_missing_dates$COMMENCEMENT_DATE)

data_no_missing_dates$INSURED_DATE_OF_BIRTH<-as.Date(data_no_missing_dates$INSURED_DATE_OF_BIRTH)

data_no_missing_dates$DATE_OF_BIRTH<-as.Date(data_no_missing_dates$DATE_OF_BIRTH)

data_no_missing_dates$PAYER_DATE_OF_BIRTH<-as.Date(data_no_missing_dates$DATE_OF_BIRTH)

data_no_missing_dates$PRODUCT_CODE<-factor(data_no_missing_dates$PRODUCT_CODE)
data_no_missing_dates$PAYMENT_MODE<-factor(data_no_missing_dates$PAYMENT_MODE)

data_no_missing_dates$STATUS<-factor(data_no_missing_dates$STATUS)
data_no_missing_dates$ID_DOCUMENT_CODE <-factor(data_no_missing_dates$ID_DOCUMENT_CODE)

data_no_missing_dates$MARITAL_STATUS<-factor(data_no_missing_dates$MARITAL_STATUS)  
data_no_missing_dates$GENDER1<-factor(data_no_missing_dates$GENDER1)

data_no_missing_dates$INSURED_ID_DESCRIPTION <-factor(data_no_missing_dates$INSURED_ID_DESCRIPTION)
data_no_missing_dates$GENDER <-factor(data_no_missing_dates$GENDER)
data_no_missing_dates$INSURED_MARITAL_STATUS  <-factor(data_no_missing_dates$INSURED_MARITAL_STATUS )

data_no_missing_dates$ID_DOCUMENT    <-factor(data_no_missing_dates$ID_DOCUMENT) 
data_no_missing_dates$INSURED_GENDER<-factor(data_no_missing_dates$INSURED_GENDER)

after_feature=data_no_missing_dates %>% select(STATUS_DATE, PAYPOINT_NAME, COMMENCEMENT_DATE,
                                               PAYMENT_MODE,PRODUCT_CODE,INCOME,TERM,PREMIUM,
                                               PAYER_DATE_OF_BIRTH,INSURED_DATE_OF_BIRTH,STATUS)

after_feature=after_feature %>%
  mutate_at(c(2,6,7,8), scale)


set.seed(1)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(after_feature), replace=TRUE, prob=c(0.7,0.3))
train  <- after_feature[sample, ]
test   <- after_feature[!sample, ]
head(train)



model2 <- rpart(STATUS ~ ., data=train, method="class")

head(test)
predict_reg <- predict(model2,test[,-c(11)], type = "class")
confusionMatrix(factor(predict_reg), factor(test$STATUS))

auc(test$STATUS,as.vector(as.numeric(predict_reg)))

rpart.plot(model2, box.palette="RdBu", shadow.col="gray", nn=TRUE)

train2<-train[1:5000,]
test2<-test[1:5000,]
model3 <- randomForest(STATUS ~ ., data = train2)
predict_reg <- predict(model3,test2[-c(11)], type = "class")

confusionMatrix(factor(predict_reg), factor(test2$STATUS))
auc(test2$STATUS,as.vector(as.numeric(predict_reg)))