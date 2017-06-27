setwd("D:/R_Prog/Bank Loan")
dim(train)
dim(test)

summary(train)

library(Amelia)
missmap(train)

colSums(is.na(train))

which(is.na(train$Married))
c <- rbind(train[105,], train[229,], train[436,])
View(c)

prop.table (table(train$Married))

library(ggplot2)

qplot(Married, ApplicantIncome, data = train, geom = "density")

mean(train$ApplicantIncome[train$Married == 'Yes'], na.rm = TRUE)
mean(train$ApplicantIncome[train$Married == 'No'], na.rm = TRUE)

mean(train$CoapplicantIncome [train$Married == 'Yes'], na.rm = TRUE)
mean(train$CoapplicantIncome [train$Married == 'No'], na.rm = TRUE)

train$Married[106] = 'No'
train$Married[229] = 'No'
train$Married[436] = 'Yes'

which(is.na(train$Gender))

c <- train[is.na(train$Gender),]
View(c)
prop.table(table(train$Gender, train$Married),1)
prop.table(table(train$Gender, train$Dependents),1)
prop.table(table(train$Gender, train$Education),1)
prop.table(table(train$Gender, train$Self_Employed),1)

mean(train$ApplicantIncome[train$Gender == 'Female'], na.rm = TRUE)
mean(train$ApplicantIncome[train$Gender == 'Male'], na.rm = TRUE)

mean(train$CoapplicantIncome [train$Gender == 'Female'], na.rm = TRUE)
mean(train$CoapplicantIncome [train$Gender == 'Male'], na.rm = TRUE)

#replace NA in Gender
train$Gender[is.na(train$Gender) & train$Married == 'Yes'] <- 'Male'
train$Gender[is.na(train$Gender) & train$Married == 'No'] <- 'Male'

train$Gender = NULL

train1 <- read_csv("D:/R_Prog/Bank Loan/train.csv")



train$Gender <- train1$Gender

sum(is.na(train$Gender))

rm(train1)

colSums(is.na(train))

prop.table(table(train$Married, train$Dependents))

unique(train$Dependents)

#Anova for Dependets and Applicant Income

c <- cbind('Dependents' = train$Dependents, 'ApplicantIncome' = train$ApplicantIncome)
class(c)
c <- as.data.frame(c)

unique(c$Dependents)
dim(c)
c <- c[complete.cases(c),]
str(c)
c$ApplicantIncome <- as.integer(c$ApplicantIncome)
anovaresult <- aov(c$ApplicantIncome ~ c$Dependents, data = c)


summary(anovaresult)

qplot(c$Dependents,c$ApplicantIncome, Data = c, geom = "boxplot" )
boxplot(c$ApplicantIncome, c$Dependents)

anovaresult$coefficients

TukeyHSD(anovaresult)

colSums(is.na(train))

prop.table(table(train$Dependents, train$Married),2)

#Missing Dependants

View(train[is.na(train$Dependents),])

train$Dependents[is.na(train$Dependents) & train$Married == 'No'] <- 0

boxplot(train$Dependents, train$ApplicantIncome )

str(train)

train$Married <- as.factor(train$Married)
train$Dependents <- as.factor(train$Dependents)
train$Education <- as.factor(train$Education)
train$Self_Employed <- as.factor(train$Self_Employed)
train$Credit_History <- as.factor(train$Credit_History)
train$Property_Area <- as.factor(train$Property_Area)
train$Loan_Status <- as.factor(train$Loan_Status)
train$Gender <- as.factor(train$Gender)
train$ApplicantIncome <- as.integer(train$ApplicantIncome)

mean(train$ApplicantIncome[train$Dependents == '0'], na.rm = TRUE)
mean(train$ApplicantIncome[train$Dependents == '1'], na.rm = TRUE)
mean(train$ApplicantIncome[train$Dependents == '2'], na.rm = TRUE)
mean(train$ApplicantIncome[train$Dependents == '3+'], na.rm = TRUE)

train$Dependents[train$ApplicantIncome <= 4900] <- '0'
train$Dependents[train$ApplicantIncome >= 4900 & train$ApplicantIncome < 5100] <- '2'
train$Dependents[train$ApplicantIncome >= 5100 & train$ApplicantIncome < 5900] <- '1'
train$Dependents[train$ApplicantIncome >= 5900] <- '3+'

colSums(is.na(train))

#Self Employed vs Income

boxplot(train$Self_Employed, train$ApplicantIncome)

plot(train$ApplicantIncome)

View(train[train$ApplicantIncome == max(train$ApplicantIncome),])

quantile(train$ApplicantIncome, 0.98)

a <- which(train$ApplicantIncome > 19585)
c <- cbind(train$ApplicantIncome, train$Self_Employed)
dim(c)
c <- c[-127,]
c <- c[-131,]
c <- c[-156,]
c <- c[-172,]
c <- c[-184,]
c <- c[-186,]
c <- c[-285,]
c <- c[-309,]
c <- c[-334,]
c <- c[-370,]
c <- c[-410,]
c <- c[-444,]
c <- c[-507,]

c <- as.data.frame(c)
missmap(c)

boxplot(c$V1, c$V2)

c <- train[-(is.na(train$LoanAmount)),]
c <- na.omit(train[,8])

colSums(is.na(c))

View(train)
colSums(is.na(train))

c <- train[!is.na(train$LoanAmount),]
View(c)

colSums(is.na(c))

dim(train)
dim(c)

train <- train[!is.na(train$LoanAmount),]

colSums(is.na(train))

#t test for self employed vs applicant income

ttest <- t.test(train$ApplicantIncome ~ train$Self_Employed)
summary(ttest)

train <- (train[!is.na(train$Self_Employed),])

train$Self_Employed[train$ApplicantIncome < 5800] <- 'No'
train$Self_Employed[train$ApplicantIncome > 6800] <- 'Yes'

prop.table(table(train$Credit_History, train$Loan_Status),2)

train$Credit_History[train$Loan_Status == 'Y'] <- '1'
train$Credit_History[train$Loan_Status == 'N'] <- '0'

train <- train[!is.na(train$Loan_Amount_Term),]

missmap(train)

library(MASS)
fit <- glm(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed
           + ApplicantIncome +CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area,
           data = train, family = binomial)
summary(fit)

prediction <- predict(fit, test, type = "response")

str(train$Credit_History)

unique(train$Credit_History)
unclass(train$Credit_History)

train$Credit_History <- as.factor(train$Credit_History)



View(train)

test$Loan_Status <- 'Y'

test <- cbind(test,prediction )
View(test)

summary(train)

summary(train$Credit_History)
class(train$Credit_History)

str(train)

summary(prediction)

View(test[is.na(test$prediction),])

table(is.na(test$prediction))
test$prediction[test$Credit_History == '0'] <- 0

rnorm(4, mean = .5, sd = 3)
set.seed(2)
runif(5,0,1)

set.seed(111)
test$prediction[is.na(test$prediction)] <- runif(5,0,1)

View(test)
test$Loan_Status[test$prediction < 0.5] <- 'N'

submit <- cbind(Loan_ID = test$Loan_ID, Loan_Status = test$Loan_Status)

getwd()
write.csv(submit, file = "first.csv", row.names = FALSE)


#######################################Randomforest

View(train)
View(test)
rm(test1)
train1$Credit_History <- NULL

prop.table(table(train$Credit_History, train$Loan_Status),1)

library(randomForest)
fit1 <- randomForest(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed
                     + ApplicantIncome +CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area,
                     data = train1, ntree = 500)
plot(fit1)
varImpPlot(fit1)

test1$Loan_Status <- 'Y'
dim(train1)
dim(test1)
test1$Credit_History <- NULL
prediction1 <- predict(fit1, test1)


names(train1)
names(test1)

str(test1)

test1$Gender <- as.factor(test1$Gender)
test1$Married <- as.factor(test1$Married)
test1$Dependents <- as.factor(test1$Dependents)
test1$Education <- as.factor(test1$Education)
test1$Self_Employed <- as.factor(test1$Self_Employed)
test1$Property_Area <- as.factor(test1$Property_Area)
test1$Loan_Status <- as.factor(test1$Loan_Status)

test1 <- cbind(test1,prediction1 )
View(test1)

test1$Loan_Status <- NULL

sum(is.na(test1$prediction1))
test1$credithistory <- test$Credit_History

test1$credithistory <- as.factor(test1$credithistory)

test1$prediction1[test1$credithistory == '0'] <- 'N'

test1$prediction <- NULL

View(test1[is.na(test1$prediction1),])

test1$credithistory[test1$Loan_ID == 'LP001232'] <- 0
test1$credithistory[test1$Loan_ID == 'LP002111'] <- 1
test1$credithistory[test1$Loan_ID == 'LP002415'] <- 0
test1$credithistory[test1$Loan_ID == 'LP002441'] <- 1

str(test1)


test1$pred <- 0
str(test1$pred)

set.seed(112)
test1$pred <- runif(367,0,1)

test1$pred1 <- ifelse(test1$pred < 0.5, 'N', 'Y')

View(test1)

test1$pred1 <- as.factor(test1$pred1)
test1$prediction1[is.na(test1$prediction1)] <- test1$pred1

submit <- cbind(Loan_ID = test1$Loan_ID, Loan_Status = test1$prediction1)

getwd()
write.csv(submit, file = "first.csv", row.names = FALSE)

##################Random Forest################################

View(train)
dim(train)

train1 <- train
View(test)
test1 <- test

test1$prediction <- NULL

View(test1)

test1$Loan_Status <- NULL

names(test1)

library(randomForest)
fit1 <- randomForest(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed
                     + ApplicantIncome +CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area,
                     data = train1, ntree = 500)

str(train1)
str(test1)

test1$Gender <- as.factor(test1$Gender)
test1$Married <- as.factor(test1$Married)
test1$Dependents <- as.factor(test1$Dependents)
test1$Education <- as.factor(test1$Education)
test1$Self_Employed <- as.factor(test1$Self_Employed)
test1$Property_Area <- as.factor(test1$Property_Area)

prediction1 <- predict(fit1, test1)

str(prediction1)

test1$loan_status <- prediction1

View(test1)

test1$loan_status <- as.factor(test1$loan_status)

str(test1$loan_status)

View(test1[is.na(test1$loan_status),])
test1$loan_status1 <- 'N'
test1$loan_status1 <- as.factor(test1$loan_status1)

test1$loan_status1[test1$loan_status == 'N'] <- 'N'
test1$loan_status1[!is.na(test1$loan_status) & test1$loan_status == 'Y'] <- 'Y'

View(test1)


submit <- cbind(Loan_ID = test1$Loan_ID, Loan_Status = test1$loan_status)
write.csv(submit, file = "second.csv", row.names = FALSE)