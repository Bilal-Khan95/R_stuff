install.packages("class","ggplot2")
library(class)
library(ggplot2)

track_data <- read.csv("C:/Users/Admin/Documents/Bilal - R/go_track_trackspoints.csv")[-1]

spatialpos(track_data[1], track_data[2])
flatearth("atlas") 
flatpoints(loc = track_data[,1:2], pch = 16, col = "Blue")