## Installation

# install latest miniconda
# install latest RTools.exe
# install latest R version

install.packages("devtools")
library(devtools)
install.packages("keras")
library(keras)

# pip install tensorflow
# pip install keras

## Data Wrangling

# Load the data from the website
data <- dataset_mnist()

# Split the raw data into suitable groupings
# x = Input (pixel values), y = Output (number categories)
x_train <- data$train$x
y_train <- data$train$y
x_test <- data$test$x
y_test <- data$test$y

# Reshape into matrices
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# Scale to turn them into range 0-1
x_train <- x_train / 255
x_test <- x_test / 255

# Turns the integers into classes suitable for Keras' processing
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

## Build the Neural Network

# Base network creation - used as scaffolding to attach other layers
neural_network <- keras_model_sequential()

# Add layers with the pipe (%>%) operator
neural_network %>% 
  layer_dense(units = 784, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 392, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 196, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 98, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 10, activation = 'softmax')

# Compile the model

neural_network %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model, recording performance in a variable

history <- neural_network %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

# View the history
# Only needed if Keras interactive graph doesn't load

plot(history)

# Evaluate performance

neural_network %>% evaluate(x_test, y_test)

# Generate predictions

neural_network %>% predict_classes(x_test)
