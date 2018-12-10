install.packages('rgl')
install.packages("Amelia")
install.packages("pscl")
install.packages("ggplot2")
library(rgl)
library(Amelia)
library(pscl)
library(ggplot2)



training.data.raw <- read.csv("train.csv",header=T)
test.data.raw <- read.csv("test.csv",header=T)

#New Column
SalePriceHigh <- max(training.data.raw$SalePrice)
sale_price_raw <- training.data.raw$SalePrice
yaxis <- sale_price_raw/SalePriceHigh
summary(yaxis)
training.data.raw$yaxis <- yaxis

#3D Graph
open3d()
plot3d(training.data.raw$LotArea,training.data.raw$BedroomAbvGr,training.data.raw$yaxis)



#2D graph
ggplot(training.data.raw, aes(x = training.data.raw[,8], y = training.data.raw$yaxis)) +
  geom_point() + 
  geom_smooth(method = "lm")

lin_reg <- lm(training.data.raw$SalePrice ~ training.data.raw$OverallQual + training.data.raw$Neighborhood + training.data.raw$GrLivArea + training.data.raw$TotalBsmtSF + training.data.raw$X2ndFlrSF,  data = training.data.raw)
summary(lin_reg)

coefficients(lin_reg)
confint(lin_reg, level = 0.95)
fitted(lin_reg)
residuals(lin_reg)
anova(lin_reg)
influence(lin_reg)
plot(lin_reg)
training.data.raw[56]<- NULL

test_data <- read.csv("test.csv")

