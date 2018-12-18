install.packages('jpeg')
library(jpeg)

#Inside train/NORMAL folder of Chest Xray Dataset cos thats the only way it will work, won't open files otherwise.
#IE have to setwd() to the exact folder wher the files exist.
setwd("~/Bilal - Hadoop/train/resized_normal/")
normal_train_images <- list.files(pattern = '*.jpg')

normal_train_data <- lapply(normal_train_images, readJPEG)
#For pneumonia train data
setwd("~/Bilal - Hadoop/train/resized_penumonia/")
pneumonia_train_images <- list.files(pattern = '*.jpg')

pneumonia_train_data <- lapply(pneumonia_train_images, readJPEG)
