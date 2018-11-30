install.packages("class")
install.packages("ggplot2")

library(class)
library(ggplot2)

car_data <- read.csv("C:/Users/Admin/Documents/Bilal - R/car.data")

names(car_data) <- c("Buying","Mainteance","No. of Doors","Seating Capacity","Boot Size","Safety","Class Values")
