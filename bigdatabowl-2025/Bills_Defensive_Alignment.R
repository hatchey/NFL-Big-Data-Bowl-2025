for (week in 1:9) {
  
  csv_file <- paste0('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/tracking_week_', week, '.csv')
  
  #Reading in weekly tracking info
  tracking_week <- read_csv(csv_file)
  
  assign(paste0('tracking_week_', week), tracking_week)
}

# Combining weeks 1-8 for training data
training_tracking <- base::rbind(tracking_week_1, tracking_week_2, tracking_week_3, tracking_week_4, 
                                 tracking_week_5, tracking_week_6, tracking_week_7, tracking_week_8)

rm(tracking_week_1, tracking_week_2, tracking_week_3, tracking_week_4, 
   tracking_week_5, tracking_week_6, tracking_week_7, tracking_week_8)

gc()

tracking_week_9 <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/tracking_week_9.csv')

#Combining weeks 1-8 with week 9
mid_2022_tracking <- base::rbind(training_tracking, tracking_week_9)

rm(tracking_week_9, training_tracking)
gc()

#Reading in plays and games df
plays <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/plays.csv')
games <- read_csv("C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/games.csv")

#Getting ball data
ball_xy <- mid_2022_tracking %>%
  dplyr::filter(frameId == 1,
                is.na(nflId)) %>%
  dplyr::select(x, y, gameId, playId) %>%
  dplyr::rename(ball_x = x,
                ball_y = y)
#Filtering data frame for only snaps after the line is set and before the ball is snapped
mid_2022_tracking <- mid_2022_tracking %>%
  dplyr::filter(frameType %in% c('BEFORE_SNAP', 'SNAP'), 
                row_number() >= which(event == "line_set")[1])

#combining plays, games, and tracking df filtering for defensive players + Bills players
bills_tracking <- plays %>%
  dplyr::left_join(games, by = 'gameId') %>%
  dplyr::left_join(mid_2022_tracking, by = c('gameId', 'playId')) %>%
  dplyr::filter(club == defensiveTeam, club == 'BUF',
                frameType == 'SNAP') %>%
  #joining ball dataframe
  dplyr::left_join(ball_xy, by = c('gameId', 'playId')) %>%
  #calculating distance from ball
  dplyr::mutate(dis_from_ball_x =  ball_x - x, 
                dist_from_ball_y = ball_y - y)

rm(mid_2022_tracking)
gc()

#reading in players data frame
players <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/players.csv')

#filtering for players who frequently go into coverage
bills_tracking <- bills_tracking %>%
  dplyr::left_join(players, by = c('nflId', 'displayName')) %>%
  dplyr::filter(position %in% c('SS', 'FS', 'ILB', 'MLB', 'DB', 'CB'))

# Create a scatter plot of where Bills defenders lined up
buffalo_defense_alignment <- ggplot(bills_tracking , aes(x = dis_from_ball_x, y = dist_from_ball_y, color = displayName)) +
  geom_point(alpha = 0.9) +
  labs(
    title = "2022 Buffalo Bills' Presnap Coverage Alignment",
    subtitle = "Location of Players at the Snap of the Ball",
    x = "Horizontal Distance from Ball (Yds, Left = Negative, Right = Positive)",
    y = "Distance from Line of Scrimmage (Yds)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)) +
  coord_cartesian(xlim = c(-20, 20), ylim = c(0, 25))

ggsave(plot = buffalo_defense_alignment, 'buffalo_defense_alignment.png', width = 10, height = 4)