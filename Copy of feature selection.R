library(readr)

data_no_missing <- read_csv("2022/ST2/TRG/CApstone/data_no_missing.csv")

data_no_missing$STATUS<-factor(data_no_missing$STATUS)
data_no_missing$PRODUCT_CODE<-factor(data_no_missing$PRODUCT_CODE)
data_no_missing$PAYMENT_MODE<-factor(data_no_missing$PAYMENT_MODE)
data_no_missing$GENDER<-factor(data_no_missing$GENDER)
data_no_missing$MARITAL_STATUS<-factor(data_no_missing$MARITAL_STATUS)
data_no_missing$INSURED_GENDER<-factor(data_no_missing$INSURED_GENDER)   
data_no_missing$INSURED_MARITAL_STATUS<-factor(data_no_missing$INSURED_MARITAL_STATUS)  
data_no_missing$GENDER1<-factor(data_no_missing$GENDER1)
data_no_missing$ID_DOCUMENT<-factor(data_no_missing$ID_DOCUMENT)
data_no_missing$PAYPOINT_NAME<-factor(data_no_missing$PAYPOINT_NAME)
data_no_missing$ID_DOCUMENT_CODE<-factor(data_no_missing$ID_DOCUMENT_CODE)
data_no_missing$INSURED_ID_DESCRIPTION<-factor(data_no_missing$INSURED_ID_DESCRIPTION)

summary(data_no_missing)


library(Boruta)
set.seed(1)
boruta.bank_train <- Boruta(STATUS~., data = data_no_missing, doTrace = 2)
print(boruta.bank_train)

boruta.bank <- TentativeRoughFix(boruta.bank_train)
print(boruta.bank)

par(mar=c(9,4,1,2))
plot(boruta.bank, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.bank$ImpHistory),function(i)
  boruta.bank$ImpHistory[is.finite(boruta.bank$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.bank$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.bank$ImpHistory), cex.axis = 0.7)


# CHISQUARED

all_num<-dplyr::select_if(data_no_missing, is.numeric)
all_cate<-dplyr::select_if(data_no_missing, is.factor)

CHIS <- lapply(all_cate, function(x) chisq.test(data_no_missing$STATUS, x))

do.call(rbind, CHIS)[,c(1,3)]


#FSELECTOR
weights<- chi.squared(STATUS~., data_no_missing)