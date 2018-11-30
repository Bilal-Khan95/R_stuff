install.packages("class")
library(class)

raw_data <- read.csv("C:/Users/Admin/Documents/Bilal - R/tables/titanic.csv")

titanic_main = subset(titanic, select = -c(1,4,9,10,11,12) )

titanic_main$Sex <- as.integer(titanic_main$Sex)
titanic_main$Age <- as.integer(titanic_main$Age)
titanic_main <- na.omit(titanic_main)

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x)))}

raw_data_normalised <- as.data.frame(lapply(titanic_main, FeatureScaling))

raw_data_training <- raw_data_normalised[1:667,]

raw_data_test <- raw_data_normalised[668:891,]

k_value <- floor(sqrt(length(raw_data_training[,1])))

raw_data_predictions <- knn(raw_data_training,raw_data_test, titanic_main[1:667,1], k=k_value)

raw_data_reference <- titanic_main[668:891,1]

raw_data_predictions

table(raw_data_predictions,raw_data_reference)

