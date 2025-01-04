library(dplyr)
library(tibble)
library(purrr)
library(abind)

make_preds <- function(model, folds, data, mask, classes){
  
  # Load model weights for each fold
  models_with_weights <- map(1:folds, ~ {
    # Initialize the model with the predefined architecture
    model <- model(input_shape = c(250, 11, 5, 5), mask_shape = c(250, 1))
    model %>% compile(
      loss = "sparse_categorical_crossentropy",
      optimizer = optimizer_adam(learning_rate = 0.001, clipnorm = 1.0),
      metrics = c("accuracy")
    ) 
    
    # Load weights for this fold
    model %>% load_model_weights_hdf5(glue::glue("C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/best_model_fold_{.x}.hdf5"))
    
    return(model)
  })
  
  # Initialize empty output (predictions for each fold and each test example)
  num_classes <- classes # Update based on the actual number of output classes in your problem
  output <- array(0, dim = c(folds, nrow(data), num_classes)) # Get predictions for each model fold
  
  # Get predictions for each fold
  for (i in 1:folds) {
    predictions <- models_with_weights[[i]] %>% predict(list(data, mask))
    output[i, , ] <- predictions  # Assign predictions to the output array
  }
  
  # After the predictions are collected, calculate the average predictions across folds
  avg_predictions <- apply(output, c(2, 3), mean)
  
  # If you want the final predicted class for each example, use max.col on the averaged predictions
  final_predictions <- apply(avg_predictions, 1, which.max) - 1
  
  # Get probabilities of the predicted classifications
  predicted_probs <- sapply(1:length(final_predictions), function(i) {
    avg_predictions[i, final_predictions[i] + 1]
  })
  
  return(list(avg_predictions = avg_predictions,
              final_predictions = final_predictions,
              predicted_probs = predicted_probs
  ))
}

# Convert test_y to a vector (if it's a matrix) and create labels tibble
labels <- test_y %>%
  as.vector() %>%  # Ensure labels are a vector
  as_tibble() %>%
  set_names("label")

week_9_predictions_plus_probs <- make_preds(CoverageClassificationNN, length(folds), test_x, test_mask, 8)

week_9_final_predictions <- week_9_predictions_plus_probs$final_predictions

# Combine with true labels and calculate accuracy
predictions <- tibble(prediction = week_9_final_predictions) %>%
  bind_cols(labels) %>%
  mutate(correct = ifelse(prediction == label, 1, 0)) %>%
  mutate(label = as.factor(label), prediction = as.factor(prediction))

# Calculate accuracy for the averaged predictions
accuracy <- mean(predictions$correct)
print(paste("Average accuracy over folds:", accuracy))

# confusion matrix
tab <- predictions %>%
  mutate(
    label = as.factor(as.integer(label)),
    prediction = as.factor(as.integer(prediction))
  )

levels(tab$label) <-
  c("C0m", "C1m", "C2z", "C2m", "C3z", "C4z", "C6z", "PRE")
levels(tab$prediction) <-
  c("C0m", "C1m", "C2z", "C2m", "C3z", "C4z", "C6z", "PRE")

conf_mat <- caret::confusionMatrix(tab$prediction, tab$label)
conf_matrix_plot <- conf_mat$table %>%
  broom::tidy() %>%
  dplyr::rename(
    Target = Reference,
    N = n
  ) %>%
  cvms::plot_confusion_matrix(
    add_sums = TRUE, place_x_axis_above = FALSE,
    add_normalized = FALSE
  ) + 
  ggplot2::ggtitle("Confusion Matrix for Coverage Prediction") +  
  ggplot2::xlab("xCoverage") + 
  ggplot2::ylab("Actual Coverage") + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

conf_matrix_plot

mid_season_2022_x <- abind(train_x, test_x, along = 1)

mid_season_2022_mask <- abind(train_mask, test_mask, along = 1)

mid_season_2022_label <- abind(train_y, test_y, along = 1) %>%
  as.vector() %>%  # Ensure labels are a vector
  as_tibble() %>%
  set_names("label")

mid_season_2022_label <- mid_season_2022_label + 1

mid_season_2022_playIds <- abind(train_playId %>% set_names('playId'), 
                                 test_playId %>%
                                   set_names('playId'), 
                                 along = 1)

rm(train_x, test_x, train_mask, test_mask, train_y, test_y, train_playId, test_playId)
gc()

final_preds_probs_mid_2022 <- make_preds(CoverageClassificationNN, 5, mid_season_2022_x, mid_season_2022_mask, 8)

average_probs_mid_2022 <- final_preds_probs_mid_2022$avg_predictions

predicted_probs_mid_2022 <- final_preds_probs_mid_2022$predicted_probs

final_predictions_mid_2022 <- final_preds_probs_mid_2022$final_predictions

# Mapping from numeric labels to column names in preds_2022
coverage_mapping <- c("C0m", "C1m", "C2z", "C2m", "C3z", "C4z", "C6z", "PRE")

colnames(average_probs_mid_2022) <- coverage_mapping

presnap_coverage_stats_mid_2022_df <- mid_season_2022_label %>%
  mutate(label = factor(label, levels = 1:8, labels = coverage_mapping)) %>%
  cbind(average_probs_mid_2022) %>%
  rowwise() %>%
  mutate(acp = get(paste0(label))) %>%
  ungroup() %>% 
  cbind(tibble(predicted_prob = predicted_probs_mid_2022)) %>%
  cbind(tibble(predictions = final_predictions_mid_2022)) %>%
  mutate(predictions = factor(predictions + 1, levels = 1:8, labels = coverage_mapping),
         correct = ifelse(predictions == label, 1, 0),
         label = as.factor(label), 
         predictions = as.factor(predictions),
         scoe = correct - predicted_prob) %>%
  cbind(mid_season_2022_playIds)

saveRDS(presnap_coverage_stats_mid_2022_df, 'presnap_coverage_stats_mid_2022.rds')

plays <- read_csv('/kaggle/input/nfl-big-data-bowl-2025/plays.csv')
games <- read_csv('/kaggle/input/nfl-big-data-bowl-2025/games.csv')

game_plays <- plays %>%
  dplyr::left_join(games, by = 'gameId') %>%
  dplyr::mutate(unique_playId = paste(gameId, playId, sep = '_'))

game_plays_w_preds <- presnap_coverage_stats_mid_2022_df %>%
  dplyr::left_join(game_plays, by = c('mid_season_2022_playIds' = 'unique_playId'))

def_team_xcoverage <- game_plays_w_preds %>%
  dplyr::group_by(defensiveTeam) %>%
  dplyr::summarize(acp = mean(acp))

off_team_xcoverage <- game_plays_w_preds %>% 
  dplyr::group_by(possessionTeam) %>%
  dplyr::summarize(acp = mean(acp))