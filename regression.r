install.packages("Amelia")
install.packages("")


#na.strings=c(""), each missing value is coded as NA 
training.data.raw <- read.csv("train.csv",header=T,na.strings=c(""))
test.data.raw <- read.csv("test.csv",header=T,na.strings=c(""))

#Look for missing values and how many unique values there are
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
sapply(test.data.raw,function(x) sum(is.na(x)))
sapply(test.data.raw, function(x) length(unique(x)))

#Any missing? Graph form
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")
missmap(test.data.raw, main = "Missing values vs observed")



#Select #########################################################################################
data <- subset(training.data.raw,select=c(81,44,45,39))
################################################################################################
datatest <- subset(test.data.raw,select=c(44,45,39))

#Missing values, 
data$----[is.na(data$----)] <- mean(data$-----,na.rm=T)

#A factor is how R deals categorical variables.
#We can check the encoding using the following lines of code
is.factor(data$-----)
is.factor(data$-----)

#This function will show us how the variables have been dummyfied by R and how to interpret them in a model
contrasts(data$----)
contrasts(data$----)

#missing values in -------, since there are only two, we will discard those two rows 
data <- data[!is.na(data$-------),]
rownames(data) <- NULL

#Model fitting
#We split the data into two chunks: training and testing set. 
#The training set will be used to fit our model which we will be testing over the testing set.
train <- data[1:1460,]
test <- datatest[1:1459,]


#Add a new sale price column & new SF column
SalePriceHigh <- max(train$SalePrice)
yaxis <- train$SalePrice/SalePriceHigh
summary(yaxis)
train["yaxis"] <- yaxis

TotalSquareFoot <- train$X1stFlrSF + train$X2ndFlrSF + train$TotalBsmtSF
train["TotalSquareFoot"] <- TotalSquareFoot

#Newdata set
datatrain <- subset(train,select=c(5,6))

#Train the data
model <- glm(yaxis ~.,family=quasibinomial(link='logit'),data=datatrain)
summary(model)



anova(model, test="Chisq")


library(pscl)
pR2(model)



fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))


library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
