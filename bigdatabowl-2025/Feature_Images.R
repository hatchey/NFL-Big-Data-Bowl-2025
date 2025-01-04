teams_colors2 <- nflfastR::teams_colors_logos

plays <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/plays.csv')
games <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/games.csv')
tracking_week_1 <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/tracking_week_1.csv')

features_plays <- plays %>%
  dplyr::left_join(games, by = 'gameId') %>%
  dplyr::filter(gameId == 2022090800,
                playId == 2908)

x_y_features <- features_plays %>%
  dplyr::left_join(tracking_week_1) %>%
  dplyr::mutate(x = ifelse(playDirection == "left", 120 - x, x),
                y = ifelse(playDirection == "left", 160 / 3 - y, y),
                dir = ifelse(playDirection == "left", dir + 180, dir),
                dir = ifelse(dir > 360, dir - 360, dir),
                o = ifelse(playDirection == "left", o + 180, o),
                o = ifelse(o > 360, o - 360, o)) %>%
  dplyr::left_join(teams_colors2 %>%
                     dplyr::select(team_abbr, team_color, team_color2), by = c('club' = 'team_abbr')) %>%
  dplyr::mutate(team_color = ifelse(is.na(nflId), 'black', team_color), 
                team_color2 = ifelse(is.na(nflId), 'brown', team_color2)) %>%
  dplyr::filter(frameType == 'SNAP', club == defensiveTeam)

x_y_features_plot <- geom_football('NFL', 
                                   display_range = 'in_bounds_only',
                                   x_tran = 60,
                                   y_tran = 26.65,
                                   color_updates = list(
                                     field_apron = "#21ae5f",
                                     offensive_half = "#21ae5f",
                                     defensive_half = "#21ae5f",
                                     offensive_endzone = "#177b43",
                                     defensive_endzone = "#177b43")) +
  geom_nfl_logos(data = features_plays, aes(x = 60, y = 26, team_abbr = homeTeamAbbr, width = 0.25, height = 0.25)) + 
  geom_nfl_wordmarks(data = features_plays, aes(x = 5, y = 26, team_abbr = visitorTeamAbbr, width = 0.25, height = 0.25, angle = 90)) + 
  geom_nfl_wordmarks(data = features_plays, aes(x = 115, y = 26, team_abbr = homeTeamAbbr, width = 0.25, height = 0.25, angle = 270)) +
  geom_segment(data = features_plays, aes(x = yardlineNumber + 10, xend = yardlineNumber + 10, y = 0.05, yend = 53.25), colour = 'black') +
  geom_segment(data = features_plays, aes(x = yardlineNumber + 10 + yardsToGo, xend = yardlineNumber + 10 + yardsToGo, y = 0.05, yend = 53.25),
               color = 'yellow') + 
  geom_point(data = x_y_features, aes(x, y), shape = ifelse(is.na(x_y_features$nflId), 18, 21),
             alpha =  ifelse(is.na(x_y_features$nflId), 1, 0.7), size =  ifelse(is.na(x_y_features$nflId), 3, 7),
             fill = ifelse(x_y_features$club == x_y_features$visitorTeamAbbr, x_y_features$team_color2, x_y_features$team_color),
             color = ifelse(x_y_features$club == x_y_features$visitorTeamAbbr, x_y_features$team_color, x_y_features$team_color2)) +
  geom_text(data = x_y_features, aes(x = x, y = y, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) +
  labs(
    title = "Features 1 and 2:",
    caption = "Defensive player's x and y coordinates determined by their horizontal distance from the ball and vertical distance from LOS"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.caption = element_text(size = 6, hjust = 0.5, face = "italic")
  )

rel_x_y_features <- features_plays %>%
  dplyr::left_join(tracking_week_1) %>%
  dplyr::mutate(x = ifelse(playDirection == "left", 120 - x, x),
                y = ifelse(playDirection == "left", 160 / 3 - y, y),
                dir = ifelse(playDirection == "left", dir + 180, dir),
                dir = ifelse(dir > 360, dir - 360, dir),
                o = ifelse(playDirection == "left", o + 180, o),
                o = ifelse(o > 360, o - 360, o)) %>%
  dplyr::left_join(teams_colors2 %>%
                     dplyr::select(team_abbr, team_color, team_color2), by = c('club' = 'team_abbr')) %>%
  dplyr::mutate(team_color = ifelse(is.na(nflId), 'black', team_color), 
                team_color2 = ifelse(is.na(nflId), 'brown', team_color2)) %>%
  dplyr::filter(frameType == 'SNAP', nflId %in% c(43399, 40166, 41290, 44881, 52460, 53678))

rel_x_y_features_off <- rel_x_y_features %>%
  dplyr::filter(club == possessionTeam)

rel_x_y_features_def <- rel_x_y_features %>%
  dplyr::filter(club == defensiveTeam)

# Create a data frame for line segments by repeating the defensive player's coordinates
line_segments <- tibble(
  x = rep(rel_x_y_features_def$x, times = 5),
  y = rep(rel_x_y_features_def$y, times = 5),
  xend = rel_x_y_features_off$x,
  yend = rel_x_y_features_off$y
)

rel_x_y_features_plot <- geom_football('NFL', 
                                       display_range = 'in_bounds_only',
                                       x_tran = 60,
                                       y_tran = 26.65,
                                       color_updates = list(
                                         field_apron = "#21ae5f",
                                         offensive_half = "#21ae5f",
                                         defensive_half = "#21ae5f",
                                         offensive_endzone = "#177b43",
                                         defensive_endzone = "#177b43")) +
  geom_nfl_logos(data = features_plays, aes(x = 60, y = 26, team_abbr = homeTeamAbbr, width = 0.25, height = 0.25)) + 
  geom_nfl_wordmarks(data = features_plays, aes(x = 5, y = 26, team_abbr = visitorTeamAbbr, width = 0.25, height = 0.25, angle = 90)) + 
  geom_nfl_wordmarks(data = features_plays, aes(x = 115, y = 26, team_abbr = homeTeamAbbr, width = 0.25, height = 0.25, angle = 270)) +
  geom_segment(data = features_plays, aes(x = yardlineNumber + 10, xend = yardlineNumber + 10, y = 0.05, yend = 53.25), colour = 'black') +
  geom_segment(data = features_plays, aes(x = yardlineNumber + 10 + yardsToGo, xend = yardlineNumber + 10 + yardsToGo, y = 0.05, yend = 53.25),
               color = 'yellow') + 
  geom_point(data = rel_x_y_features_off, aes(x, y), shape = 21, alpha = 0.7, size = 7, fill = rel_x_y_features_off$team_color, color = rel_x_y_features_off$team_color2) +
  geom_text(data = rel_x_y_features_off, aes(x = x, y = y, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) +
  geom_point(data = rel_x_y_features, aes(x, y), shape = ifelse(is.na(rel_x_y_features$nflId), 18, 21),
             alpha =  ifelse(is.na(rel_x_y_features$nflId), 1, 0.7), size =  ifelse(is.na(rel_x_y_features$nflId), 3, 7),
             fill = ifelse(rel_x_y_features$club == rel_x_y_features$visitorTeamAbbr, rel_x_y_features$team_color2, rel_x_y_features$team_color),
             color = ifelse(rel_x_y_features$club == rel_x_y_features$visitorTeamAbbr, rel_x_y_features$team_color, rel_x_y_features$team_color2)) +
  geom_text(data = rel_x_y_features, aes(x = x, y = y, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) +
  geom_segment(data = line_segments, aes(x = x, y = y, xend = xend, yend = yend), color = "black", alpha = 0.5, size = 0.7) +
  labs(
    title = "Features 3 and 4:",
    caption = "Defensive player's relative x and y coordinates in relation to each of the offensive skilled position players"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(), 
    plot.caption = element_text(size = 6, hjust = 0.5, face = "italic")
  )

o_features <- features_plays %>%
  dplyr::left_join(tracking_week_1) %>%
  dplyr::mutate(x = ifelse(playDirection == "left", 120 - x, x),
                y = ifelse(playDirection == "left", 160 / 3 - y, y),
                dir = ifelse(playDirection == "left", dir + 180, dir),
                dir = ifelse(dir > 360, dir - 360, dir),
                o = ifelse(playDirection == "left", o + 180, o),
                o = ifelse(o > 360, o - 360, o)) %>%
  dplyr::left_join(teams_colors2 %>%
                     dplyr::select(team_abbr, team_color, team_color2), by = c('club' = 'team_abbr')) %>%
  dplyr::mutate(team_color = ifelse(is.na(nflId), 'black', team_color), 
                team_color2 = ifelse(is.na(nflId), 'brown', team_color2)) %>%
  dplyr::filter(frameType == 'SNAP', nflId %in% c(NA, 40166))

o_features_plot <- geom_football('NFL', 
                                 display_range = 'in_bounds_only',
                                 x_tran = 60,
                                 y_tran = 26.65,
                                 color_updates = list(
                                   field_apron = "#21ae5f",
                                   offensive_half = "#21ae5f",
                                   defensive_half = "#21ae5f",
                                   offensive_endzone = "#177b43",
                                   defensive_endzone = "#177b43")) +
  geom_nfl_logos(data = features_plays, aes(x = 60, y = 26, team_abbr = homeTeamAbbr, width = 0.25, height = 0.25)) + 
  geom_nfl_wordmarks(data = features_plays, aes(x = 5, y = 26, team_abbr = visitorTeamAbbr, width = 0.25, height = 0.25, angle = 90)) + 
  geom_nfl_wordmarks(data = features_plays, aes(x = 115, y = 26, team_abbr = homeTeamAbbr, width = 0.25, height = 0.25, angle = 270)) +
  geom_segment(data = features_plays, aes(x = yardlineNumber + 10, xend = yardlineNumber + 10, y = 0.05, yend = 53.25), colour = 'black') +
  geom_segment(data = features_plays, aes(x = yardlineNumber + 10 + yardsToGo, xend = yardlineNumber + 10 + yardsToGo, y = 0.05, yend = 53.25),
               color = 'yellow') + 
  geom_point(data = o_features, aes(x, y), shape = ifelse(is.na(o_features$nflId), 18, 21),
             alpha =  ifelse(is.na(o_features$nflId), 1, 0.7), size =  ifelse(is.na(o_features$nflId), 3, 7),
             fill = ifelse(o_features$club == o_features$visitorTeamAbbr, o_features$team_color2, o_features$team_color),
             color = ifelse(o_features$club == o_features$visitorTeamAbbr, o_features$team_color, o_features$team_color2)) +
  geom_text(data = o_features, aes(x = x, y = y, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) +
  geom_segment(data = o_features, aes(x = x, y = y, xend = x + cos(o * pi / 180) * 5, yend = y + sin(o * pi / 180) * 5), arrow = arrow(type = "closed", length = unit(0.2, "inches")), size = 1) +
  labs(
    title = "Features 5:",
    caption = "Defensive player's oreination relative to the ball"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.caption = element_text(size = 6, hjust = 0.5, face = "italic")
  )

ggsave(plot = x_y_features_plot, 'x_y_features.png', width = 6, height = 4)
ggsave(plot = rel_x_y_features_plot, 'rel_features.png', width = 6, height = 4)
ggsave(plot = o_features_plot, 'o_features.png', width = 6, height = 4)

# Read the images as magick objects
x_y_features <- image_read("x_y_features.png")
rel_x_y_features <- image_read("rel_features.png")
o_features <- image_read("o_features.png")

combined_features <- image_append(c(x_y_features, rel_x_y_features, o_features))

combined_features

image_write(combined_features, "features.png")