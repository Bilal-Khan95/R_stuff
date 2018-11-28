#This is a template for the Introduction to Data Science Session.
#You are free to use any suitable dataset with a categorical dependent variable and
#and numeric independent variables.

#'install.packages' uses the R Package Manager to find and install packages
#'library' brings needed packages into scope (homologous with 'import' in Java)

#Classifier Library

install.packages("class")
library(class)

#Graphing Library

install.packages("ggplot2")
library(ggplot2)

#setwd = 'Set Working Directory'. This is where R will look for files that you reference.

setwd("C:/Users/tknow/Desktop/datasets")

#Read in CSV Data into a DataFrame object. A DataFrame is like a table.

RawData <- read.csv("C:/Users/Admin/Documents/Bilal - R/BreastCancerData.csv")

#The 'c' function puts individual Strings into a String vector. This String vector can
#be added to the DataFrame as a header row with the 'names' function.

names(RawData) <- c("ID","Diagnosis","Mean Radius","Mean Texture", "Mean Perimeter","Mean Area","Mean Smoothness","Mean Compactness","Mean Concavity","Mean Concave Points","Mean Symmetry","Mean Fractal Dimension",
                 "Radius SE","Texture SE","Perimeter SE","Area SE", "Smoothness SE","Compactness SE","Concavity SE","Concave Points SE","Symmetry SE","Fractal Dimension SE",
                 "Worst Radius","Worst Texture","Worst Perimeter","Worst Area","Worst Smoothness","Worst Compactness","Worst Concavity","Worst Concave Points", "Worst Symmetry","Worst Fractal Dimension")

#Let's remove the ID Variable - R has an index built-in, so a second one is extraneous
#Besides, we don't want to feed ID in as a parameter to our KNN

RawData_NoID <- RawData[,-1]

#We also need a subset of the BCD data set without the Result column
#This allows it to be used for normalisation and validation purposes

RawData_NoResults <- RawData_NoID[,-1]

#There are many Feature Scaling functions defined in CRAN, but to learn the maths, it is
#good to built one yourself, like so:

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
#This corresponds to the equation given in the slides

RawData_Normalised <- as.data.frame(lapply(RawData_NoResults, FeatureScaling))
#Now our data has been normalised according to our function

#This is a good time to split our data. Let's go for a 75:25 (ish) split.
RawData_Training <- RawData_Normalised[1:426,] #Leave the value after the comma empty to
                                                    #capture all columns

RawData_Test <- RawData_Normalised[426:568,]

#Finally, let's compute a k-Value to use with the classifier
#The rule of thumb is the square root of the number of observations
#trained with, so let's use that for now

K_Value <- floor(sqrt(length(RawData_Training[,1])))  #Result is floored, as k must be
                                                  #a whole number

#Now we can use the KNN Algorithm that comes with the 'class' package to classify our data
#This is done all at once, in a single function

RawData_Predictions <- knn(RawData_Training,RawData_Test, RawData_NoID[1:426,1], k=K_Value)

#Now, let's view the performance of our KNN
#This is expressed in terms of {True Positives   False Positives}
#                              {False Negatives  True Negatives}

#First, subset the reference data into its own data frame

RawData_Reference <- RawData_NoID[426:568,1]

#Then, we can tabulate the two

table(RawData_Predictions,RawData_Reference)

#Further analysis of the results can give further insights into the suitability
#of the model, and of its parameters

#Type '?knn' into the console, and observe the various parameters
#You can adjust the parameters to empirically determine which k-Value and which
#geometry works best for this data set

#Once you're done testing results yourself, see below for a comprehensive exploration of all
#K-values up to 100.

###############################

big_errors <- c()

kValues <- c()

#Below is a for loop that creates a DataFrame containing the error values when KNN is applied
#to the dataset, for 1 <= k <= 100.
#When graphed, it allows the Data Scientist to find the lowest point on the trendline and
#therefore empircally determine the best K-Value to model this dataset.

for(i in c(1:100)) {
  
  next_error <-  table(knn(RawData_Training,RawData_Test,RawData_NoID[1:426,1],k=i),RawData_Reference)[1,2] +
                table(knn(RawData_Training,RawData_Test,RawData_NoID[1:426,1],k=i),RawData_Reference)[2,1]
  
  big_errors <- c(big_errors, next_error)
  
  kValues <- c(kValues, i)
}

big_errors_df <- data.frame(kValues,big_errors)

names(big_errors_df) <- c("k-Value","Error Value")

#A collection of ggplot2 functions used for defining a graph.
#ggplot2 is based off the 'Grammar of Graphics' model, and each component is able to
#be added and altered seperately from the rest.
#You will be provided with a PDF that will help you edit the graphic to your liking.

ggplot(big_errors_df, aes(x = kValues, y = big_errors)) +
  geom_point() +
  geom_smooth(method = "loess",colour = "blue", size = 1) + 
  ggtitle("Error vs k-Value for Breast Cancer Data") +
  xlab("k-Values") +
  ylab("Error") +
  theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
  scale_colour_manual(values = c("red","blue"))
