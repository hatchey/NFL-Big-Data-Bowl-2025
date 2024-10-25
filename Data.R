library(readr)
library(dplyr)
library(data.table)

games <- read_csv('games.csv')
saveRDS(games, 'games.rds')
games <- readRDS('games.rds')

player_play <- read_csv('player_play.csv')
saveRDS(player_play, 'player_play.rds')
player_play <- readRDS('player_play.rds')

plays <- read_csv('plays.csv')
saveRDS(plays, 'plays.rds')
plays <- readRDS('plays.rds')

players <- read_csv('players.csv')
saveRDS(players, 'players.rds')
players <- readRDS('players.rds')

for (week in 1:9) {
  # Create file names dynamically
  csv_file <- paste0('tracking_week_', week, '.csv')
  rds_file <- paste0('tracking_week_', week, '.rds')
  
  # Read the CSV
  tracking_week <- read_csv(csv_file)
  
  # Save as RDS
  saveRDS(tracking_week, rds_file)
  
  # Assign to a variable in the global environment
  assign(paste0('tracking_week_', week), tracking_week)
}

for (week in 1:9){
  rds_file <- paste0('tracking_week_', week, '.rds')
  tracking_week <- readRDS(rds_file)
  assign(paste0('tracking_week_', week), tracking_week)
}

tracking_data <- rbind(tracking_week_1, tracking_week_2, tracking_week_3, tracking_week_4, tracking_week_5, 
                      tracking_week_6, tracking_week_7, tracking_week_8, tracking_week_9)

merged_track_1 <- tracking_data %>% 
  dplyr::left_join(dplyr::inner_join(games, plays, by = c('gameId')), by = c("gameId", 'playId')) %>% 
  dplyr::left_join(player_play, by = c('gameId', 'playId', 'nflId'))












