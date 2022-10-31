#### REMOVE MISSING VALUES

library(caTools)
library(readr)
library(dplyr)
##IMPORT
TRG_DAT_CLN_RMV_new <- read_csv("2022/ST2/TRG/CApstone/TRG_DAT_CLN_RMV new.csv")

data=TRG_DAT_CLN_RMV_new

# BINARY CLASSES
data[data$STATUS=="ACT",5]<-"RET"

data[data$STATUS=="LAP" | data$STATUS== "CAN" |data$STATUS=="SUR",5]<-"NOTRET"

data<-subset(data, data$STATUS != "NFS" & data$STATUS != "PUP" & data$STATUS != "MAT" & data$STATUS !="NUL")

data <- data %>% 
  mutate(STATUS = recode(STATUS, 
                         "RET" = "1", 
                         "NOTRET" = "0"))
data$STATUS<-factor(data$STATUS)

# THIS CALCS. AGE
data$DATE_OF_BIRTH<-lubridate::time_length(difftime(as.Date("2018-06-30"), data$DATE_OF_BIRTH), "years")

#THIS CALCS. AGE
data$PAYER_DATE_OF_BIRTH<-lubridate::time_length(difftime(as.Date("2018-06-30"), data$PAYER_DATE_OF_BIRTH), "years")

data$INSURED_DATE_OF_BIRTH<-lubridate::time_length(difftime(as.Date("2018-06-30"), data$INSURED_DATE_OF_BIRTH), "years")


data$PRODUCT_CODE<-factor(data$PRODUCT_CODE)
data$PAYMENT_MODE<-factor(data$PAYMENT_MODE)

data$GENDER<-factor(data$GENDER)
data$MARITAL_STATUS<-factor(data$MARITAL_STATUS)
data$INSURED_GENDER<-factor(data$INSURED_GENDER)   
data$INSURED_MARITAL_STATUS<-factor(data$INSURED_MARITAL_STATUS)  
data$GENDER1<-factor(data$GENDER1)


set.seed(1)
#SUBSET COMPLETE DATA
missingdata=data[!complete.cases(data),]
#SUBSET INCOMPLETE DATA
nonmissingdata=data[complete.cases(data),]

#SAMPLE COMPLETE DATA OF SIZE 71572 WITH ALL INCOMPLETE DATA FOR FAST COMPUTATION TIME. 20% NA DATA
sampledata=rbind(nonmissingdata[sample(1:nrow(nonmissingdata),replace=FALSE,size=71572),],missingdata)

summary(sampledata)

#INDEX ALL MISSING VALUE ROWS
idx<-unique (unlist (lapply (data, function (x) which (is.na (x)))))
sort(idx)

#IMPORT LIBRARY FOR MULTIPLE IMPUTATION TECHNIQUE
library(mice)


mrMeanMatch <- mice(sampledata, maxit=0)   # TO GET INITIAL SET UP OF FUNCTION
preds<-mrMeanMatch$predictorMatrix  # THIS IS A PREDICTOR MATRIX
meths<-mrMeanMatch$method   # THIS IS THE METHOD DEPENDENT ON CATEGORICAL/CONTINOUS


#meths[c("OCCUPATION_CODE")] <- "polyreg": too many categories>50 # LEFT OCCU CODE AS NUMERIC AS TOO MANY CATS


preds[, c("STATUS_DATE")] <- 0 # I DO NOT WANT TO INCLUDE THESE DATES IN CALCULTAING MISSING VALUES

preds[,c("COMMENCEMENT_DATE")]<-0 # SAME AS ABOVE

dataagh <- mice(sampledata, maxit = 5,    # IMPUTATION MODEL
                predictorMatrix = preds, 
                method = meths, print =  FALSE)

new <- mice::complete(dataagh, 1) # GET NEW DATASET

data[idx,]<-new[71573:nrow(new),] # LAST 71573->END IS THE IMPUTED MISSING DATA,REPLACE ORGINAL WITH THIS
 


summary(data)
write.csv(data,"~/2022/ST2/TRG/CApstone/data_no_missing.csv", row.names = FALSE)