library(magick)

#Theme for the CNN-LSTM plot animation
custom_theme <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


#Generating dummy input data
input_data <- data.frame(
  timestep = rep(1:10, each = 50),
  channel = rep(1:5, times = 100),
  activation = runif(500))

#Creating a visualization for the input layer of the CNN-LSTM model 
input_plot <- ggplot(input_data, aes(x = timestep, y = channel, fill = activation)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Input Layer", x = "Timesteps", y = "Channels") +
  theme_minimal() +
  custom_theme

animated_input <- input_plot +
  transition_manual(timestep) +
  labs(title = "Input Layer: Timestep {current_frame}")

animate(animated_input, nframes = 10, fps = 2)
anim_save("input_layer.gif", animation = last_animation())

#Generating dummy 2d convolution data
conv2d_data <- expand.grid(x = 1:10, y = 1:10, filter = 1:5, timestep = 1:10)
conv2d_data$activation <- runif(nrow(conv2d_data))

#Creating a visualization for the 2d convolution layer of the CNN-LSTM model
conv2d_plot <- ggplot(conv2d_data, aes(x = x, y = y, fill = activation)) +
  geom_tile() +
  facet_wrap(~filter) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "2D Convolution", x = "Timesteps", y = "Filters") +
  theme_minimal() +
  custom_theme

animated_conv2d <- conv2d_plot +
  transition_manual(timestep) +
  labs(title = "2D Convolution: Timestep {current_frame}")

animate(animated_conv2d, nframes = 10, fps = 2)
anim_save("conv2d_layer.gif", animation = last_animation())

#Generating dummy 2d pooling data
pool2d_data <- expand.grid(x = 1:5, y = 1:5, filter = 1:5, timestep = 1:10)
pool2d_data$activation <- runif(nrow(pool2d_data))

#Creating a visualization for the 2d pooling layer of the CNN-LSTM model
pool2d_plot <- ggplot(pool2d_data, aes(x = x, y = y, fill = activation)) +
  geom_tile() +
  facet_wrap(~filter) +
  scale_fill_gradient(low = "white", high = "yellow") +
  labs(title = "2D Pooling", x = "Timesteps", y = "Filters") +
  theme_minimal() +
  custom_theme

animated_pool2d <- pool2d_plot +
  transition_manual(timestep) +
  labs(title = "2D Pooling: Timestep {current_frame}")

animate(animated_pool2d, nframes = 10, fps = 2)
anim_save("pool2d_layer.gif", animation = last_animation())

#Generating dummy 1d convolution data
conv1d_data <- expand.grid(timestep = 1:10, filter = 1:5)
conv1d_data$activation <- runif(nrow(conv1d_data))

#Creating a visualization for the 1d convolution layer of the CNN-LSTM model
conv1d_plot <- ggplot(conv1d_data, aes(x = timestep, y = filter, fill = activation)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "1D Convolution", x = "Timesteps", y = "Filters") +
  theme_minimal() +
  custom_theme

animated_conv1d <- conv1d_plot +
  transition_manual(timestep) +
  labs(title = "1D Convolution: Timestep {current_frame}")

animate(animated_conv1d, nframes = 10, fps = 2)
anim_save("conv1d_layer.gif", animation = last_animation())

#Generating dummy 1d pooling data
pool1d_data <- expand.grid(timestep = 1:10, filter = 1:5)
pool1d_data$activation <- runif(nrow(pool1d_data))

#Creating a visualization for the 1d pooling layer of the CNN-LSTM model
pool1d_plot <- ggplot(pool1d_data, aes(x = timestep, y = filter, fill = activation)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(title = "1D Pooling", x = "Timesteps", y = "Filters") +
  theme_minimal() +
  custom_theme

animated_pool1d <- pool1d_plot +
  transition_manual(timestep) +
  labs(title = "1D Pooling: Timestep {current_frame}")

animate(animated_pool1d, nframes = 10, fps = 2)
anim_save("pool1d_layer.gif", animation = last_animation())

#Generating dummy masking data
masking_data <- expand.grid(timestep = 1:10, channel = 1:5)
masking_data$mask <- sample(c(0, 1), nrow(masking_data), replace = TRUE)

#Creating a visualization for the masking layer of the CNN-LSTM model
masking_plot <- ggplot(masking_data, aes(x = timestep, y = channel, fill = factor(mask))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "white", "1" = "gray")) +
  labs(title = "Masking", x = "Timesteps", y = "Channels") +
  theme_minimal() +
  custom_theme

animated_masking <- masking_plot +
  transition_manual(timestep) +
  labs(title = "Masking: Timestep {current_frame}")

animate(animated_masking, nframes = 10, fps = 2)
anim_save("masking_layer.gif", animation = last_animation())

#Generating dummy lstm data
lstm_data <- expand.grid(timestep = 1:10, state = 1:10)
lstm_data$activation <- runif(nrow(lstm_data))

#Creating a visualization for the lstm layer of the CNN-LSTM model
lstm_plot <- ggplot(lstm_data, aes(x = timestep, y = state, fill = activation)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +
  labs(title = "LSTM", x = "Timesteps", y = "States") +
  theme_minimal() +
  custom_theme

animated_lstm <- lstm_plot +
  transition_manual(timestep) +
  labs(title = "LSTM: Timestep {current_frame}")

animate(animated_lstm, nframes = 10, fps = 2)
anim_save("lstm_layer.gif", animation = last_animation())

#Generating dummy dense data
dense_data <- expand.grid(timestep = 1:10, neuron = 1:10)
dense_data$activation <- runif(nrow(dense_data))

#Creating a visualization for the dense layer of the CNN-LSTM model
dense_plot <- ggplot(dense_data, aes(x = timestep, y = neuron, fill = activation)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "brown") +
  labs(title = "Dense Layer", x = "Timesteps", y = "Neurons") +
  theme_minimal() +
  custom_theme

animated_dense <- dense_plot +
  transition_manual(timestep) +
  labs(title = "Dense Layer: Timestep {current_frame}")

animate(animated_dense, nframes = 10, fps = 2)
anim_save("dense_layer.gif", animation = last_animation())

# Reading in individual GIFs
input_gif <- image_read("input_layer.gif")
conv2d_gif <- image_read("conv2d_layer.gif")
pool2d_gif <- image_read("pool2d_layer.gif")
conv1d_gif <- image_read("conv1d_layer.gif")
pool1d_gif <- image_read("pool1d_layer.gif")
masking_gif <- image_read("masking_layer.gif")
lstm_gif <- image_read("lstm_layer.gif")
dense_gif <- image_read("dense_layer.gif")

# Combining GIFs horizontally
combined_gif <- image_append(c(input_gif[1], conv2d_gif[1], pool2d_gif[1], conv1d_gif[1], 
                               pool1d_gif[1], masking_gif[1], lstm_gif[1], dense_gif[1]))
for (i in 2:length(input_gif)) {
  frame <- image_append(c(input_gif[i], conv2d_gif[i], pool2d_gif[i], conv1d_gif[i], 
                          pool1d_gif[i], masking_gif[i], lstm_gif[i], dense_gif[i]))
  combined_gif <- c(combined_gif, frame)
}

# Saving combined GIF
image_write(combined_gif, "combined_layers.gif")