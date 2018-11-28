v_2 <- read.csv("evens.csv", header=FALSE)

odds <- v_2 - 1

write.csv(odds, "odds.csv")