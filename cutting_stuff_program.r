train_data_2 <- read.csv("train.csv")

train_data$PoolArea[train_data$PoolArea > 0]<- 1

train_data$GarageArea[train_data$GarageArea > 0] <- 1

train_data$Fence <- as.numeric(train_data$Fence)

train_data$Fence[is.na(train_data$Fence)] <- 0
train_data$Fence[train_data$Fence != 0] <- 1

train_data$CentralAir <- as.numeric(train_data$CentralAir)
train_data$CentralAir[train_data$CentralAir == 1] <- 0
train_data$CentralAir[train_data$CentralAir == 2] <- 1

train_data$MasVnrArea[train_data$MasVnrArea > 0] <- 1

train_data$LotFrontage[is.na(train_data$LotFrontage)] <- 0
train_data$LotFrontage[train_data$LotFrontage > 0] <- 1

train_data$FireplaceQu <- as.numeric(train_data$FireplaceQu)

train_data$Bathroom <- rowSums(train_data[,48:51])

train_data$WoodDeckSF[train_data$WoodDeckSF > 0] <- 1

train_data$PavedDrive <- as.numeric(train_data$PavedDrive)

train_data$PavedDrive[train_data$PavedDrive < 3] <- 0

train_data$PavedDrive[train_data$PavedDrive == 3] <- 1


train_data$Porches <- rowSums(train_data[,68:71])
train_data$Porches[train_data$Porches >0 ] <- 1

train_data_2 = subset(train_data_2, select = -c(1,2,3,4,6,7,8,9,11,12, )