plot(iris)

boxplot(Sepal.Length ~ Petal.Length, data=iris)

#lines
plot(iris$Petal.Width, iris$Petal.Length)
abline(v=2, col="blue2")
abline(h=5, col="red", lty=3, lwd=4)
abline(lm(iris$Petal.Width, iris$Petal.Length))


boxplot(iris$Sepal.Width ~ iris$Species)

#different shapes
plot(iris$Petal.Width, iris$Petal.Length, pch=c(2,3,4)[iris$Species])

#diferent shapes and colours
plot(iris$Petal.Width, iris$Petal.Length, pch=c(2,3,5)[iris$Species], col=c("red", "lime green", "pink")[iris$Species])

#Zoomed in
plot(iris$Petal.Width, iris$Petal.Length, main="Zoomed In Graph", sub = "Subtitle", xlab = "Petal Width", 
     ylab = "Petal Length", xlim = c(1, 4), ylim = c(1, 4))

#Legend
legend("bottomright", title="Species", legend=levels(iris$Species), col=c("red", "blue", "green"), pch=c(2:4))

#Multiple graphs
par(mfrow = c(2,2))
plot(Petal.Length ~ Sepal.Length, data = iris)
plot(Sepal.Length ~ Petal.Length, data = iris)
hist(iris$Sepal.Length)
boxplot(iris$Petal.Length)

#Histograms
hist(iris$Petal.Width)
hist(iris$Petal.Width, main  = "Petal Width Distirbution", col = "blue", breaks = 5)

#BarPlots
par(mfrow = (c(2,2)))
counts <- table(mtcars$gear)
barplot(counts, xlab = "number of gears")
barplot(counts, horiz = TRUE, names.arg = c("3 Gears", "4 Gears", "5 Gears"), col = "red")
morecounts <- table(mtcars$vs, mtcars$gear)
barplot(morecounts, main = "Car distr by gears and VS", xlab = "Number of Gears", col = c("darkblue", "red"))
barplot(morecounts, col = c("darkblue", "red"), beside = TRUE)

#Pie charts
myinfo <- table(iris$Species)
pie(myinfo, labels = c("setosa", "Versicolor", "Virginica"), main = "Pie chart of Iris Species")


##############
#Plotting
data(orange)
Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)

#set up plot
plot(xrange, yrange, typr = "n", xlab = "Age (days)", ylab = "Circumference (mm)")
colours <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18, 18 + ntrees, 1)

#addlines
for(i in 1:ntrees) {
  tree <- subset(Orange, Tree == i)
  lines(tree$age, tree$circumference, type = "b", lwd = 1.5, lty = linetype[i], col = colours, pch = plotchar[i])
}

#add title subtitle and legend
title("Tree Growth", "Example of line plot")
legend(xrange[1], yrange[2], 1:ntrees, cex = 0.8, col = colours, pch = plotchar, lty = linetype, title = "tree")
