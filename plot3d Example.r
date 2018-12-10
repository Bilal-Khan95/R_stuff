install.packages('rgl')
install.packages("Amelia")
install.packages("pscl")
install.packages("ggplot2")
library(rgl)
library(Amelia)
library(pscl)
library(ggplot2)



training_data_raw <- read.csv("train.csv",header=T)

#New Column
SalePriceTrain <- max(training_data_raw$SalePrice)
SalePriceRaw <- training_data_raw$SalePrice
yaxis <- (SalePriceRaw / SalePriceTrain)
summary(yaxis)
training_data_raw$yaxis <- yaxis

#3D Graph
open3d()
plot3d(training_data_raw$LotArea,training_data_raw$BedroomAbvGr,training_data_raw$yaxis)



#2D graph
ggplot(training_data_raw, aes(x = training_data_raw$GarageType, y = training_data_raw$yaxis)) +
  geom_point() +
  geom_smooth(method = "lm")

#training_data_raw$GarageType <- as.numeric(training_data_raw$GarageType)
#training_data_raw$CentralAir <- as.numeric(training_data_raw$CentralAir)
#training_data_raw$CentralAir[training_data_raw$CentralAir == "Y"] <- 1

#boxplot(training_data_raw$Electrical, training_data_raw$yaxis)

#training_data_raw$total_bathrooms <- rowSums(training_data_raw[,48:51])

lin_reg <- lm(training_data_raw$SalePrice~ training_data_raw$square_feet + training_data_raw$total_bathrooms + training_data_raw$LotArea + training_data_raw$MasVnrArea + training_data_raw$TotalBsmtSF, data = training_data_raw )
