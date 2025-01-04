library(readr)
library(dplyr)

# Reading in games data
games <- read_csv("C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/games.csv")


# Reading in player info for each play
player_play <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/player_play.csv')

# Reading in plays info 
plays <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/plays.csv')

# Reading in player info
players <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/players.csv')

for (week in 1:9) {
  
  csv_file <- paste0('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/tracking_week_', week, '.csv')
  
  #Reading in weekly tracking info
  tracking_week <- read_csv(csv_file)
  
  assign(paste0('tracking_week_', week), tracking_week)
}

# Combining weeks 1-8 for training data
training_tracking <- base::rbind(tracking_week_1, tracking_week_2, tracking_week_3, tracking_week_4, 
                                 tracking_week_5, tracking_week_6, tracking_week_7, tracking_week_8)

rm(tracking_week_1, tracking_week_2, tracking_week_3, tracking_week_4, tracking_week_5, tracking_week_6, tracking_week_7, tracking_week_8)

gc()

# Renaming week 9 tracking data to testing tracking data
testing_tracking <- tracking_week_9

rm(tracking_week_9)

gc()












