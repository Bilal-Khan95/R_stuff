install.packages("ggplot2")
library(ggplot2)

ggplot(training.data.raw, aes(x = training.data.raw$LotArea, y = training.data.raw$yaxis)) +
  geom_point()
  
  
  
  
  
  
  
  
  
  
  





#+ geom_smooth(method = "loess",colour = "blue", size = 1) + 
  #ggtitle("Error vs k-Value for Breast Cancer Data") +
  #xlab("k-Values") +
  #ylab("Error") +
  #theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  #theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
 # scale_colour_manual(values = c("red","blue"))