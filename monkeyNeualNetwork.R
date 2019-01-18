install.packages("magick")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
devtools::install_github('thomasp85/lime')
install.packages("abind")
install.packages("jpeg")
install.packages("future.apply")
install.packages("ggplot2")
library(magick)
library(lime)
library(abind)
library(jpeg)
library(future.apply)
library(keras)
library(ggplot2)
## Global variables for easy configuration

# Preprocessing control

seed <- 6747386
image_size <- c(60,60)
mode <- "categorical"
data_location <- "C:/Users/Admin/Documents/Bilals Stuff/Bilal - R/monkeyData"

# Categories in the dataset

categories <- c("antled_howler","patas_monkey","bald_uakari","japanese_macaque","pygmy_marmoset","white_headed_capuchin","silver_marmoset","common_squirrel_monkey","black_headed_night_monkey","nilgiri_langur")

# Data file paths

training_filepath <- paste(data_location, "/training", sep = "")
test_filepath <- paste(data_location, "/validation", sep = "")

# Setting up the image import device. This will loop over the data folders and pull them into
# R in a suitable format for further processing. This also skips the horrors of the read_JPEG
# function. Extra parameters such as shearing and rotation can be added as desired.
# Type '?image_data_generator' to see the parameters.
training_data_generator = image_data_generator(
  rescale = 1/255)

# Test data shouldn't be altered, since this is merely checking the correctness of the network,
# not teaching it to generalise. It is too late to get any benefits from augmenting at
# this stage.
test_data_generator <- image_data_generator(
  rescale = 1/255
)

# Load in the training images
train_image_array_gen <- flow_images_from_directory(training_filepath, 
                                                    training_data_generator,
                                                    target_size = image_size,
                                                    class_mode = mode,
                                                    classes = categories,
                                                    seed = seed)

# Load in the test images
test_image_array_gen <- flow_images_from_directory(test_filepath, 
                                                    test_data_generator,
                                                    target_size = image_size,
                                                    class_mode = mode,
                                                    classes = categories,
                                                    seed = seed)

# Create category indicies for use later

category_indices <- train_image_array_gen$class_indices

## Neural Network setup and training

batch_size <- 32
epochs <- 20
input_shape <- c(image_size, 3)
global_dropout_rate <- 0.15
global_filter_size <- 64
global_kernel_size <- c(2,2)
global_pool_size <- c(2,2)

# Initialise the network
model <- keras_model_sequential()

# Adding layers. This is being built as a Convolutional Network, with Convolutional, Dropout
# and Max Pooling layers featuring heavily. Feel free to add and edit layers as desired, just
# bear in mind that it should be as responsive as possible for the user, so needs to balance
# accuracy and robustness with speed.
model %>%
  layer_activation_leaky_relu(0.5, input_shape = input_shape) %>%
  layer_conv_2d(filter = global_filter_size, kernel_size = global_kernel_size, padding = "same") %>%
  layer_activation("relu") %>%
  
  layer_conv_2d(filter = global_filter_size, kernel_size = global_kernel_size, padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
  layer_max_pooling_2d(pool_size = global_pool_size) %>%
  layer_dropout(global_dropout_rate) %>%
  
  layer_conv_2d(filter = global_filter_size, kernel_size = global_kernel_size, padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
  layer_max_pooling_2d(pool_size = global_pool_size) %>%
  layer_dropout(global_dropout_rate) %>%
  
  layer_conv_2d(filter = global_filter_size, kernel_size = global_kernel_size, padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
  layer_max_pooling_2d(pool_size = global_pool_size) %>%
  layer_dropout(global_dropout_rate) %>%
  
  layer_conv_2d(filter = global_filter_size, kernel_size = global_kernel_size, padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%
  
  layer_max_pooling_2d(pool_size = global_pool_size) %>%
  layer_dropout(global_dropout_rate) %>%
  
  # Flatten max filtered output into a feature vector and feed into full connected layer
  layer_flatten() %>%
  #layer_dense(100) %>%
  #layer_activation("relu") %>%
  #layer_dropout(global_dropout_rate) %>%
  
  # Softmax is ideal for end classification
  layer_dense(length(categories)) %>% 
  layer_activation("softmax")

# Train the model
model2 <- model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.01, decay = 0.01),
  metrics = "accuracy"
)

# Test the model
hist <- model2 %>% fit_generator(
  train_image_array_gen,
  steps_per_epoch = as.integer(train_image_array_gen$n / batch_size), 
  epochs = epochs, 
  validation_data = test_image_array_gen,
  validation_steps = as.integer(test_image_array_gen$n / batch_size),
  
  # Track progress, both on screen, and in checkpoint and log files
  verbose = 2,
  callbacks = list(
    callback_model_checkpoint(paste(data_location, "\\flower_checkpoints.h5", sep = ""), save_best_only = TRUE),
    callback_tensorboard(log_dir = paste(data_location, "\\logs", sep = ""))
  )
)

# Explaining the model

image_prep <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(60,60))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- x /255
  })
  do.call(abind::abind, c(arrays, list(along = 1)))
}

#example_image_path <- paste(data_location,"/training/nilgiri_langur/n9043.jpg", sep = "")
#res <- predict(model, image_prep(example_image_path))

#as_classifier(res,categories)

#plot_superpixels(example_image_path)

train_data_location <- paste(data_location, "", sep = "")

plan(multiprocess)

#files_to_use <- as.vector(choose.files())


imageFunction <- function(files_to_use){
  all_train_images <- future_lapply(files_to_use,readJPEG)
  processed_train_images <- lapply(all_train_images, as.raster)
  magicked_images <- future_lapply(processed_train_images,image_read)
  explainer <- lime(files_to_use, model, image_prep)
  explanation <- explain(files_to_use, explainer, n_labels = 1, n_features = 20)
  
  explanation$label[explanation$label == 1] <- "Mantled Howler"
  explanation$label[explanation$label == 2] <- "Patas Monkey"
  explanation$label[explanation$label == 3] <- "Bald Uakari"
  explanation$label[explanation$label == 4] <- "Japanese Macaque"
  explanation$label[explanation$label == 5] <- "Pygmy Marmoset"
  explanation$label[explanation$label == 6] <- "White Headed Capuchin"
  explanation$label[explanation$label == 7] <- "Silvery Marmoset"
  explanation$label[explanation$label == 8] <- "Common Squirrel Monkey"
  explanation$label[explanation$label == 9] <- "Black Headed Night Monkey"
  explanation$label[explanation$label == 10] <- "Nilgiri Langur"
}


plot10 <- plot_image_explanation(explanation)


library(shiny)

ui <- fluidPage(
  
  titlePanel('Monkeys Neural Network'),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "plot1",
        label = "Monkeys",
        accept = c('image/png','image/jpeg','image/jpg')
      ),
      tags$hr()
    ),
    mainPanel(
      textOutput("plot"),
      imageOutput(outputId = "image")
    )
  )
)

server <- function(input, output) {
  
  re1 <- reactive({gsub("\\\\", "/", input$imageFunction(plot1$datapath))})
  
  output$image <- renderImage({list(src = plot10)})
  
}

shinyApp(ui, server)
