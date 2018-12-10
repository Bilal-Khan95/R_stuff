training.data.raw <- read.csv("train.csv",header=T)
test.data.raw <- read.csv("test.csv",header=T)

train_clean <- read.csv("training_data_cleaned.csv")
test_clean <- read.csv("test_data_clean.csv")

train_clean[57] <- NULL
#test_clean[c(7,8,9)] <- NULL

lin_reg <- lm(SalePrice ~ OverallQual + Neighborhood + GrLivArea + X2ndFlrSF + TotalBsmtSF  + LotArea,  data = training.data.raw)
#summary(lin_reg)

#coefficients(lin_reg)
#confint(lin_reg, level = 0.95)
#fitted(lin_reg)
#residuals(lin_reg)
#anova(lin_reg)
#influence(lin_reg)
#plot(lin_reg)
#test_data_2 <- test.data.raw

prediction_1 <- predict(lin_reg, test.data.raw)


write.csv(prediction_1, "prediction_1.csv")


