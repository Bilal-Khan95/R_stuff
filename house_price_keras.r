install.packages("devtools")
library(devtools)
install.packages("keras")
library(keras)
library(ggplot2)




training_data<- read.csv("train.csv")
test_data <- read.csv("test.csv")
sale_prices <- read.csv("sample_submission.csv")

train <- list()

train_data_trim <- training_data[c(5,18,27,39,47,45)]
test_data_trim <- test_data[c(5,27,18,39,47,45)]

train_data <- as.vector(training_data[c(18,39,47,45)])
train_labels <- as.vector(training_data[81])
test_data <- as.vector(test_data[c(18,39,47,45)])
test_labels <- as.vector(sample_submission$SalePrice)
#c(train_data, train_labels) %<-% boston_housing$train
#c(test_data, test_labels) %<-% boston_housing$test

#Normalize data
train_data <- scale(train_data)

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")

test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 1460, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 730, activation = "relu") %>%
    layer_dense(units = 365, activation = "relu") %>%
    layer_dense(units = 1, activation = "softmax")
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model

}

model <- build_model()
model %>% summary()


print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 25 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 50

# Fit the model and store training stats
history <- model %>% fit(
  as.matrix(train_data),
  as.matrix(train_labels),
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 5))

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

model <- build_model()
history <- model %>% fit(
  as.matrix(train_data),
  as.matrix(train_labels),
  epochs = 30,
  validation_split = 0.2,
  verbose = 0,
  callbacks = early_stop
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 5))

# Evaluate performance
c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))

# Generate predictions
test_predictions <- model %>% predict(test_data)
test_predictions[ , 1]
