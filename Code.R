rm(Data)
names(Data) <- c("id", "clumpThickness", "uniformityOfCellSize","uniformityOfCellShape", "marginalAdhesion", "singleEpithelialCellSize",
                  "bareNuclei", "blandChromatin", "normalNucleoli", "mitoses", "class")
View(Data)

Data$id <- NULL
str(Data)

Data$clumpThickness <- as.numeric(Data$clumpThickness)
Data$uniformityOfCellSize <- as.numeric(Data$uniformityOfCellSize)
Data$uniformityOfCellShape <- as.numeric(Data$uniformityOfCellShape)
Data$marginalAdhesion <- as.numeric(Data$marginalAdhesion)
Data$singleEpithelialCellSize <- as.numeric(Data$singleEpithelialCellSize)
Data$bareNuclei <- as.numeric(Data$bareNuclei)
Data$blandChromatin <- as.numeric(Data$blandChromatin)
Data$normalNucleoli <- as.numeric(Data$normalNucleoli)
Data$mitoses <- as.numeric(Data$mitoses)
Data$class <- as.numeric(Data$class)

library(Amelia)
missmap(Data)

sum(is.na(Data$bareNuclei))

which(is.na(Data$bareNuclei))

cancerData <- Data[complete.cases(Data),]

str(cancerData)
683/3
683-227

train$target <- NULL
test$target <- NULL

traindata <- cancerData[1:457,1:9]
testdata <- cancerData[458:683,1:9]
traintargetdata <- cancerData[1:457,10]
testtargetdata <- cancerData[458:683,10]

library(class)
prediction <- knn(traindata, testdata, traintargetdata$class, k = 21, l = 0, prob = FALSE,
                  use.all = TRUE)

str(traindata)
dim(traintarget)
dim(testdata)
dim(testtargetdata)

traindata <- as.data.frame(traindata)
traintarget <- as.data.frame(traintarget)
testdata <- as.data.frame(testdata)
testtargetdata <- as.data.frame(testtargetdata)

a <- table(testtargetdata$class, prediction)
mean(testtargetdata$class== prediction)

library(ggplot2)
?ggplot2

plot(a)

install.packages("ElemStatLearn")

library(ElemStatLearn)
