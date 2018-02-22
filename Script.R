#To see the dependency on the categorical variables
CrossTable(train_data$Education)
CrossTable(train_data$Gender, train_data$Loan_Status, prop.r = TRUE, prop.c = FALSE, 
           prop.t = FALSE,  prop.chisq = FALSE)

CrossTable(train_data$Dependents, train_data$Loan_Status, prop.r = TRUE, prop.c = FALSE, 
           prop.t = FALSE,  prop.chisq = FALSE)

hist(train_data$ApplicantIncome, xlab = "Applicant Income", breaks = 50)

parts = sqrt(nrow(train_data))
hist(train_data$LoanAmount, xlab = "Loan Amount", breaks = parts)
plot(train_data_1$ApplicantIncome, xlab = "Applicant Income") #Outlier
plot(train_data$CoapplicantIncome, xlab = "Coapplicant Income") #outlier
plot(train_data$LoanAmount, xlab = "Loan Amount")

outlier <- which(train_data$ApplicantIncome > 60000) 
train_data_1 <- train_data[-outlier, ]

thumb_Applicant <- quantile(train_data$ApplicantIncome, 0.75) + 1.5 * IQR(train_data$ApplicantIncome)
outlier_2 <- which(train_data$ApplicantIncome > thumb)

train_data_3 <- train_data[ -outlier_2, ] 
plot(train_data_3$ApplicantIncome, xlab = "Applicant Income") #Outlier


thumb_Coapplicant <- quantile(train_data$CoapplicantIncome, 0.75) + 1.5 * IQR(train_data$CoapplicantIncome)
outlier_3 <- which(train_data$CoapplicantIncome > thumb_Coapplicant)

train_data_4 <- train_data_3[- outlier_3, ]
plot(train_data_4$CoapplicantIncome, xlab = "CoApplicant Income") #Outlier

index_loan_amount <- which(is.na(train_data_4$LoanAmount))
LoanAmount <- train_data_4$LoanAmount
LoanAmount[index_loan_amount] <- median(train_data_4$LoanAmount, na.rm = TRUE)
train_data_4$LoanAmount <- LoanAmount
summary(train_data_4$LoanAmount)

index_loan_term <- which(is.na(train_data_4$Loan_Amount_Term))
LoanAmountTerm <- train_data_4$Loan_Amount_Term
LoanAmountTerm[index_loan_term] <- median(train_data_4$Loan_Amount_Term, na.rm = TRUE)
train_data_4$Loan_Amount_Term <- LoanAmountTerm
summary(train_data_4$Loan_Amount_Term)

train_data_4$Credit_History <- as.factor(train_data_4$Credit_History)

index_credit_hist <- which(is.na(train_data_4$Credit_History))
train_data_5 <- train_data_4[-index_credit_hist, ]

set.seed(88)
split <- sample.split(train_data_6$Loan_Status, SplitRatio = 0.8)
train <- subset(train_data_6, split == TRUE)
test <- subset(train_data_6, split == FALSE)


convto0<-which(train$Loan_Status=="N")
train$Loan_Status[convto0]<-0
View(train)
convto1<-which(train$Loan_Status=="Y")
train$Loan_Status[convto1]<-1

convto0_t<-which(test$Loan_Status=="N")
test$Loan_Status[convto0_t]<-0
convto1_t<-which(test$Loan_Status=="Y")
test$Loan_Status[convto1_t]<-1

train$Loan_Status<-as.numeric(as.character(train$Loan_Status))
test$Loan_Status<-as.numeric(as.character(test$Loan_Status))

#Model Creation
model <- glm(formula = Loan_Status ~ . -Loan_ID, family = "binomial", data = train)
summary(model)


#Predicting the Train Set
predict <- predict(model, newdata = test, type = "response")
predict
#Confusion Matrix
table(ActualValue = train$Loan_Status, PredictedValue = predict > 0.6)

cut_off <- ifelse(predict > 0.6, 1, 0)


