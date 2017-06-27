setwd("D:/R_Prog/Big mart")

combicopy <- combi

View(combi)

train_new <- combi[1:nrow(Train),]
test_new <- combi[-(1:nrow(Train)),]

fit <- lm(Item_Outlet_Sales ~., data = train_new)
summary(fit)
cor(train_new)

install.packages("dummies")
library(dummies)

combi <- dummy.data.frame(combi, names = c('Item_Fat_Content', 'Item_Type',
                                           'Outlet_Location_Type', 'Outlet_Type',
                                           'newitems'), sep = '_')

combi$Item_Identifier <- NULL
combi$Outlet_Identifier <- NULL
combi$Outlet_Establishment_Year <- NULL

fit <- lm(Item_Outlet_Sales ~., data = train_new)
summary(fit)

str(combi)

p <- cor(train_new)

sapply(train_new, is.numeric)

library(corrplot)
corrplot(train_new)

View(train_new)

colSums(is.na(train_new))

train_new$Item_Count <- NULL
train_new$Outlet_Count <- NULL

View(combicopy)

combi1 <- combicopy

train_new <- combi1[1:nrow(Train),]
test_new <- combi1[-(1:nrow(Train)),]

train_new$Item_Identifier <- NULL
train_new$Outlet_Identifier <- NULL
train_new$newitems <- NULL
train_new$Outlet_Establishment_Year <- NULL
train_new$year <- NULL

View(train_new)

fit <- lm(log(Item_Outlet_Sales) ~., data = train_new)
summary(fit)

mod <- lm(train_new$Item_Outlet_Sales ~ ., data = train_new)
summary(mod)

par(mfrow = c(2,2))
plot(fit)

mod <- lm((train_new$Item_Outlet_Sales) ~ ., data = train_new)
summary(mod)

install.packages("Metrics")
library(Metrics)

rmse(train_new$Item_Outlet_Sales, (mod$fitted.values))

dim(Train)
dim(train_new)

train_new$year <- 2013 - Train$Outlet_Establishment_Year

library(rpart)
library(rpart.plot)

tree_fit <- rpart(Item_Outlet_Sales ~ ., train_new,method = "anova",
                  control=rpart.control(minsplit=10, cp=0.005))

rpart.plot(tree_fit)

predictions <- predict(tree_fit, type = "vector")

rmse(train_new$Item_Outlet_Sales, predictions)

library(randomForest)
rf_fit <- randomForest(Item_Outlet_Sales ~ ., train_new,method = "parRF")

str(train_new)

train_new$Item_Fat_Content <- as.factor(train_new$Item_Fat_Content)
train_new$Item_Type <- as.factor(train_new$Item_Type)
train_new$Outlet_Location_Type <- as.factor(train_new$Outlet_Location_Type)
train_new$Outlet_Type <- as.factor(train_new$Outlet_Type)

plot(rf_fit)
print(rf_fit)

install.packages("tree")
library(tree)

plot.tree.sequence(rf_fit)

rf_fit$importance

library(party)

plot(rf_fit)

varImpPlot(rf_fit)

submit <- predict(rf_fit, test_new)

