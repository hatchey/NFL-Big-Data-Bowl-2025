library(stringr)
library(pracma)
library(tensorflow)
library(data.table)

pass_plays_clean <- plays %>%
  # Filtering for passing plays
  dplyr::filter(isDropback == TRUE) %>%
  # Mutating for consistent format in the pff_passCoverage columns
  dplyr::mutate(pff_passCoverage = case_when(str_detect(pff_passCoverage, 'Cover-3') ~ 'Cover 3',
                                             str_detect(pff_passCoverage, 'Cover-2')  ~ 'Cover 2',
                                             str_detect(pff_passCoverage, 'Cover-6') | str_detect(pff_passCoverage, 'Cover 6') ~ 'Cover 6',
                                             str_detect(pff_passCoverage, 'Cover-1') ~ 'Cover 1',
                                             TRUE ~ pff_passCoverage)) %>%
  # Removing Coverages that are either have very small representation or unknown
  dplyr::filter(!pff_passCoverage %in% c(NA, 'Miscellaneous', 'Bracket', 'Goal Line', 'Red Zone'),
                #Removing game where tracking data isn't present for one team on offense and one team on defense
                gameId != 2022091808)

# Joining with the player info for each play
player_plays_clean <- pass_plays_clean %>%
  dplyr::left_join(player_play, by = c('gameId', 'playId')) %>%
  # Creating a column to determine whether players is defensive
  dplyr::mutate(is_def = ifelse(teamAbbr == defensiveTeam, 1, 0)) %>%
  # Joining with player info data frame 
  dplyr::left_join(players %>% dplyr::select(nflId, displayName, position), by = ('nflId'))

rm(pass_plays_clean, plays, player_play, games)

gc()

prepare_tracking_data <- function(track_df, plays_df){
  
  # Getting ball tracking info
  train_ball_xy <- track_df %>%
    dplyr::filter(frameId == 1,
                  is.na(nflId)) %>%
    dplyr::select(x, y, gameId, playId) %>%
    dplyr::rename(ball_x = x,
                  ball_y = y)
  
  tracking_clean_df <- track_df %>%
    # Filtering to remove frames after snap, before the offensive line has set, and the ball 
    dplyr::filter(frameType %in% c('BEFORE_SNAP', 'SNAP'),
                  row_number() >= which(event == "line_set")[1],
                  !is.na(nflId)) %>%
    # Joining in the ball tracking data df
    dplyr::left_join(train_ball_xy, by = c('gameId', 'playId')) %>% 
    # Normalizing the x, y, direction, and oreination, create sin and cos oreinatoin, 
    # create distance from the ball columns and angle to the ball columns
    dplyr::mutate(x = ifelse(playDirection == "left", 120 - x, x),
                  y = ifelse(playDirection == "left", 160 / 3 - y, y),
                  dir = ifelse(playDirection == "left", dir + 180, dir),
                  dir = ifelse(dir > 360, dir - 360, dir),
                  # get orientation and direction in x and y direction
                  dir_rad = deg2rad(dir),
                  dir_x = sin(dir_rad),
                  dir_y = cos(dir_rad),
                  o = ifelse(playDirection == "left", o + 180, o),
                  o = ifelse(o > 360, o - 360, o),
                  o_rads = deg2rad(o),
                  o_sin = sin(o_rads),
                  o_cos = cos(o_rads),
                  dist_from_ball_x = ball_x - x,
                  dist_from_ball_y = ball_y - y,
                  o_to_ball = atan2(dist_from_ball_y, dist_from_ball_x) * (180 / pi),
                  o_to_ball = (360 - o_to_ball) + 90,
                  o_to_ball = case_when(o_to_ball < 0 ~ o_to_ball + 360,
                                        o_to_ball > 360 ~ o_to_ball - 360 ,
                                        TRUE ~ o_to_ball),
                  o_to_ball = abs(o - o_to_ball),
                  o_to_ball = pmin(360 - o_to_ball, o_to_ball)
    ) %>%
    # Joined tracking data with player info for each play 
    dplyr::left_join(player_plays_clean %>% 
                       dplyr::select(gameId, playId, nflId, pff_passCoverage, is_def, position),
                     by = c('gameId', 'playId', 'nflId')) %>%
    # Removed NA pass coverage entries
    dplyr::filter(!is.na(pff_passCoverage))
  
  return(tracking_clean_df)
}

training_tracking_clean <- prepare_tracking_data(training_tracking, player_plays_clean)

rm(training_tracking)
gc()

testing_tracking_clean <- prepare_tracking_data(testing_tracking, players_plays_clean)

tracking_features <- c('gameId', 'playId', 'nflId', 'frameId', 'is_def', 'x', 'y', 'o_to_ball', 'dist_from_ball_x',
                       'dist_from_ball_y','pff_passCoverage', 'is_def', 'position')

rm(player_plays_clean)

prepare_model_data <- function(tracking_df, features){
  
  # Create levels for the pass coverage factor column
  coverage_levels <- c("Cover-0", "Cover 1", "Cover 2", "2-Man", "Cover 3", "Quarters", "Cover 6", "Prevent")
  
  model_features <- tracking_df %>%
    # remove all post snap frames
    dplyr::filter(frameType %in% c('BEFORE_SNAP', 'SNAP')) %>%
    # select only columns with features
    dplyr::select(all_of(features)) %>%
    #create unique playId and turning the pff_passCoverage column into a factor
    dplyr::mutate(unique_playId = paste(gameId, playId, sep = '_'),
                  pff_passCoverage = base::factor(pff_passCoverage, levels = coverage_levels))
  
  return(model_features)
}

training_model_features <- prepare_model_data(training_tracking_clean, tracking_features)

rm(training_tracking_clean)

testing_model_features <- prepare_model_data(testing_tracking_clean, tracking_features)

rm(testing_tracking_clean)

gc()

#function to filter data frame to only keep frames where the defense has 11 players on the field
filter_defense_plays_frames <- function(df) {
  # Count unique defensive players by frameId
  valid_defense_frames <- df[is_def == 1, .(num_def_players = uniqueN(nflId)), by = frameId]
  
  # Filter for frames with exactly 11 defensive players
  valid_defense_frames <- valid_defense_frames[num_def_players == 11, frameId]
  
  # Ensure that we reference 'frameId' as a column correctly
  df[frameId %in% valid_defense_frames]
}

prepare_tensor_data_keras <- function(df) {
  setDT(df)  # Convert to data.table
  
  # Define the maximum number of frames to keep per play
  trunc_max_frames <- 250
  
  # Truncate the data to the last `trunc_max_frames` frames for each play
  df <- df[, {
    # Unique frame IDs for this play, sorted
    frame_ids <- unique(frameId)
    n_frames <- length(frame_ids)
    
    # Determine the frames to retain (last `trunc_max_frames` or fewer)
    frames_to_keep <- frame_ids[max(1, n_frames - trunc_max_frames + 1):n_frames]
    
    # Filter rows corresponding to the selected frames
    .SD[frameId %in% frames_to_keep]
  }, by = unique_playId]
  
  # Determine number of plays
  playIds <- unique(df$unique_playId)
  num_plays <- length(playIds)
  
  # Define the maximum number of frames per sequence
  max_frames <- df[, .(max_frames = uniqueN(frameId)), by = unique_playId][, max(max_frames)]
  
  # Preallocate the x and mask arrays
  x <- array(0, dim = c(num_plays, max_frames, 11, 5, 5))  # Example shape (plays, frames, defenders, n_offense, features)
  mask <- array(0, dim = c(num_plays, max_frames, 1))
  
  # Process each play individually
  for (i in seq_along(playIds)) {
    play <- playIds[i]
    
    # Filter data for the current play
    play_df <- df[unique_playId == play]
    
    # Filter for offensive and defensive features
    off_features_dt <- play_df[is_def != 1 & position %in% c('WR', 'TE', 'RB', 'FB')]
    def_features_dt <- play_df[is_def == 1]
    
    # Perform the left join with suffixes
    rel_features_dt <- merge(
      def_features_dt, 
      off_features_dt, 
      by = c('gameId', 'playId', 'frameId'), 
      all.x = TRUE, 
      suffixes = c("", "_off"), 
      allow.cartesian = TRUE
    )
    
    # Create the additional columns
    rel_features_dt[, `:=`(
      diff_x = x_off - x, 
      diff_y = y_off - y
    )]
    
    # Select the relevant columns and arrange the data by unique_playId, frameId, and nflId
    rel_features_dt <- rel_features_dt[, .(unique_playId, nflId, frameId, dist_from_ball_x, dist_from_ball_y,
                                           diff_x, diff_y, o_to_ball, is_def)]
    setorder(rel_features_dt, frameId, nflId)
    
    # Filters for frames where the defense has 11 players relevant frames
    rel_features_dt <- filter_defense_plays_frames(rel_features_dt)
    
    if (nrow(rel_features_dt) > 0) {
      # Determine number of frames, offensive and defensive players for this play
      n_frames <- uniqueN(rel_features_dt$frameId)
      defenders <- uniqueN(rel_features_dt$nflId)
      n_offense <- nrow(rel_features_dt) / (n_frames * defenders)
      
      # Convert data to matrix
      play_array <- array(as.matrix(rel_features_dt[, -c("unique_playId", "nflId", "frameId", "is_def")]))
      
      # Reshape the play matrix to a 4D array (time_steps, defenders, n_offense, features)
      play_array <- array_reshape(t(play_array), dim = c(ncol(play_array), n_frames, defenders, n_offense))
      play_array <- aperm(play_array, c(2, 3, 4, 1))
      
      # Fill the 5d array (plays, time_steps, defenders, n_offense, features)
      x[i, (max_frames - n_frames + 1):max_frames, 1:defenders, 1:n_offense, ] <- play_array
      mask[i, (max_frames - n_frames + 1):max_frames, 1] <- 1
      
    }
  }
  
  # Prepare the labels (y)
  unique_plays <- unique(df[, .(unique_playId, pff_passCoverage)])
  unique_plays[, pff_passCoverage := as.integer(pff_passCoverage)]
  
  y <- array(unique_plays$pff_passCoverage, dim = c(num_plays))
  playId <- array(unique_plays$unique_playId, dim = c(num_plays))
  
  return(list(
    x = x,
    mask = mask,
    y = y,
    playId = playId
  ))
}
# Prepare training and testing data
train_tensors <- prepare_tensor_data_keras(training_model_features)
train_x <- train_tensors$x
train_mask <- train_tensors$mask
train_y <- train_tensors$y - 1
train_playId <- train_tensors$playId

test_tensors <- prepare_tensor_data_keras(testing_model_features)
test_x <- test_tensors$x
test_mask <- test_tensors$mask
test_y <- test_tensors$y - 1
test_playId <- test_tensors$playId

rm(training_model_features, testing_model_features)
gc()

# Saving training, masking and testing to global environment
saveRDS(train_x, 'train_x.rds')
saveRDS(train_mask, 'train_mask.rds')
saveRDS(train_y, 'train_y.rds')
saveRDS(test_x, 'test_x.rds')
saveRDS(train_playId, 'train_playId.rds')
saveRDS(test_mask, 'test_mask.rds')
saveRDS(test_y, 'test_y.rds')
saveRDS(test_playId, 'test_playId.rds')