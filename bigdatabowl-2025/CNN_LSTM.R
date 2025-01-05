library(keras)

CoverageClassificationNN <- function(input_shape, mask_shape) {
  inputs <- layer_input(shape = input_shape)
  input_mask <- layer_input(shape = mask_shape)
  
  # Apply 2D convolution to each frame independently
  X <- inputs %>%
    time_distributed(layer_conv_2d(filters = 128, kernel_size = c(1, 1), strides = c(1, 1), activation = "relu")) %>%
    time_distributed(layer_conv_2d(filters = 160, kernel_size = c(1, 1), strides = c(1, 1), activation = "relu")) %>%
    time_distributed(layer_conv_2d(filters = 128, kernel_size = c(1, 1), strides = c(1, 1), activation = "relu"))
  
  # Pooling layers (per frame)
  Xmax <- X %>%
    time_distributed(layer_max_pooling_2d(pool_size = c(1, 5))) %>%
    time_distributed(layer_lambda(f = function(x) x * 0.3))
  
  Xavg <- X %>%
    time_distributed(layer_average_pooling_2d(pool_size = c(1, 5))) %>%
    time_distributed(layer_lambda(f = function(x) x * 0.7))
  
  # Combine Max and Average Pooling
  X <- list(Xmax, Xavg) %>%
    layer_add() %>%
    time_distributed(layer_lambda(f = function(y) k_squeeze(y, axis = 3))) %>%
    time_distributed(layer_batch_normalization())
  
  # 1D Convolutional Layers (per frame)
  X <- X %>%
    time_distributed(layer_conv_1d(filters = 160, kernel_size = 1, strides = 1, activation = "relu")) %>%
    time_distributed(layer_batch_normalization()) %>%
    time_distributed(layer_conv_1d(filters = 96, kernel_size = 1, strides = 1, activation = "relu")) %>%
    time_distributed(layer_batch_normalization()) %>%
    time_distributed(layer_conv_1d(filters = 96, kernel_size = 1, strides = 1, activation = "relu")) %>%
    time_distributed(layer_batch_normalization())
  
  # Pooling for 1D features (per frame)
  Xmax <- X %>%
    time_distributed(layer_max_pooling_1d(pool_size = 11)) %>%
    time_distributed(layer_lambda(f = function(x) x * 0.3))
  
  Xavg <- X %>%
    time_distributed(layer_average_pooling_1d(pool_size = 11)) %>%
    time_distributed(layer_lambda(f = function(x) x * 0.7))
  
  X <- list(Xmax, Xavg) %>%
    layer_add() %>%
    time_distributed(layer_lambda(f = function(y) k_squeeze(y, axis = 2)))
  
  # Multiply by the mask to ignore invalid frames (zero out invalid frames)
  X <- layer_multiply(list(X, input_mask))
  
  X <- layer_masking(X, mask_value = 0.0)
  
  # Add LSTM layer to process temporal patterns
  X <- X %>%
    layer_lstm(units = 128, return_sequence = FALSE) %>%
    layer_batch_normalization()
  
  # Fully connected layers
  X <- X %>%
    layer_dense(units = 96, activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_layer_normalization() %>%
    layer_dropout(rate = 0.3)
  
  # Output layer for classification
  output <- X %>%
    layer_dense(units = 8, activation = "softmax")
  
  # Define and return the model
  model <- keras_model(inputs = list(inputs, input_mask), outputs = output)
  
  return(model)
}

model <- CoverageClassificationNN(input_shape = c(250, 11, 5, 5), mask_shape = c(250, 1))

# Setting seed for reproducibility
set.seed(1995)
set_random_seed(1995)

# Cross-validation folds
folds <- splitTools::create_folds(
  y = as.integer(train_y),
  k = 5,
  type = "stratified",
  invert = TRUE
)

accuracies <- numeric(length(folds))
best_epochs <- numeric(length(folds))

for (fold in seq_along(folds)) {
  cat(sprintf("\n------------- FOLD %d ---------\n", fold))
  
  # Split data into training and validation
  val_indices <- folds[[fold]]
  train_indices <- setdiff(seq_len(nrow(train_x)), val_indices)
  
  train_x_fold <- train_x[train_indices, , , , ]
  train_mask_fold <- train_mask[train_indices, , ]
  train_y_fold <- train_y[train_indices]
  val_x_fold <- train_x[val_indices, , , , ]
  val_mask_fold <- train_mask[val_indices, , ]
  val_y_fold <- train_y[val_indices]
  
  # Define model
  model <- CoverageClassificationNN(input_shape = c(250, 11, 5, 5), mask_shape = c(250, 1))
  
  # Compile the model
  model %>% compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = 0.001, clipnorm = 1.0),
    metrics = c("accuracy")
  ) 
  
  #callback for checkpoints to save the model with the best val accuracy
  checkpoint <- callback_model_checkpoint(
    filepath = sprintf("C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/best_model_fold_%d.hdf5", fold),
    monitor = "val_loss",
    save_best_only = TRUE,
    save_weights_only = TRUE,
    mode = "min",
    verbose = 1
  )
  
  #callback to print training and validation accuracy of each epoch
  print_metrics <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      cat(sprintf(
        "Epoch: %d - Loss: %.4f - Accuracy: %.4f - Val_Loss: %.4f - Val_Accuracy: %.4f\n",
        epoch + 1, logs[["loss"]], logs[["accuracy"]],
        logs[["val_loss"]], logs[["val_accuracy"]]
      ))
      flush.console()
    }
  ) 
  
  #callback to stop training on fold if validation accuracy hasn't improved after 10 epoch to avoid overfitting
  early_stopping <- callback_early_stopping(
    monitor = "val_loss",
    patience = 10,
    restore_best_weights = TRUE
  )
  
  #schedule to reduce learning rate by gamma of 0.983 each epoch
  step_lr <- function(epoch, lr) {
    gamma <- 0.983
    step_size <- 1
    new_lr <- lr * gamma^(floor(epoch / step_size))
    return(new_lr)
  }
  
  #learning rate scheduler to reduce learning rate
  lr_scheduler <- callback_learning_rate_scheduler(schedule = step_lr)
  
  # Train the model
  history <- model %>% fit(
    list(train_x_fold, train_mask_fold), train_y_fold,
    validation_data = list(list(val_x_fold, val_mask_fold), val_y_fold),
    epochs = 40,
    batch_size = 32,
    callbacks = list(print_metrics, early_stopping, lr_scheduler, checkpoint),
    verbose = 0
  )
  
  # Extract the best validation accuracy and epoch
  best_val_idx <- which.min(history$metrics$val_loss)
  accuracies[fold] <- history$metrics$val_accuracy[best_val_idx]
  best_epochs[fold] <- best_val_idx
  
  cat(glue::glue("Fold {fold}: Best Val Accuracy = {round(accuracies[fold], 3)}, Epoch = {best_epochs[fold]}\n"))
}

# Summarize results
cat(sprintf("Average Validation Accuracy: %.4f\n", mean(accuracies)))