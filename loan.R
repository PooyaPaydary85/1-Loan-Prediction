setwd("C:/Users/Env.lab/Desktop/Data Science/Random Projects/Loan Prediction")
#Install the random forest package

library("randomForest")
#Reading the train and test data sets
train <- read.csv("train.csv",header=T)
test <- read.csv("test.csv",header=T)

#Making sure that both train and test have the same number of columns and column names so that we can mix them
test$Loan_Status<- as.factor(NA)
train$CoapplicantIncome <-as.integer(train$CoapplicantIncome)

#This column is created so that we can later seperate train and test
train$isTraining <-T
test$isTraining <-F

#We are binding the two datasets
total <- rbind(train,test)

#Cleaning the data
total[total$Gender=="","Gender"] <-'Male'
total[total$Married=="","Married"] <-'Yes'
total[total$Dependents=="","Dependents"] <-'0'

total[total$Self_Employed=="","Self_Employed"] <-'No'

median.loan<-median(total$LoanAmount,na.rm = TRUE)
total[is.na(total$LoanAmount),"LoanAmount"] <-median.loan

median.loan.term<-median(total$Loan_Amount_Term,na.rm = TRUE)
total[is.na(total$Loan_Amount_Term),"Loan_Amount_Term"] <-median.loan.term


total[is.na(total$Credit_History),"Credit_History"] <-as.factor(1)


#Data is cleaned now its time to seperate them again
train <-total[total$isTraining==T,]
test <-total[total$isTraining==F,]

#Set up the Forest and do the prediction
loan.status.equation <-"Loan_Status ~ Married + Education + ApplicantIncome + LoanAmount + Credit_History + Gender + Dependents + Self_Employed + CoapplicantIncome + Loan_Amount_Term + Property_Area "
loan.status.formula<-as.formula (loan.status.equation)
model <- randomForest(formula=loan.status.formula, data=train, ntree=500, mtry=3, nodsize=0.01*nrow(train), na.action=na.roughfix)
loan.prediction <- predict(model, newdata = test)

output <-as.data.frame(test$Loan_ID)
output$loan.prediction <-as.data.frame(loan.prediction)
tail(output)

write.csv(output,"Prediction2.csv",row.names=F)
