library(sportyR)
library(nflplotR)
library(ggplot2)
library(gganimate)
library(dplyr)
library(nflfastR)
library(nflreadr)
library(knitr)

teams_colors2 <- nflfastR::teams_colors_logos

plays <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/plays.csv')
games <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/games.csv')
tracking_week_1 <- read_csv('C:/Users/maxde/OneDrive/Documents/2025 NFL Big Data Bowl/bigdatabowl-2025/data/tracking_week_1.csv')

game_plays <- plays %>%
  left_join(games, by = 'gameId')
  

animate_play <- function(game_plays, tracking, gameId, playId){
  #Joining game_plays df and tracking df
  animate_play_tracking <- df %>%
    left_join(tracking_week_1, by = c('gameId', 'playId')) %>%
    dplyr::filter(gameId == gameId,
                  playId == playId) %>%
    dplyr::mutate(x = ifelse(playDirection == "left", 120 - x, x),
                  y = ifelse(playDirection == "left", 160 / 3 - y, y),
                  dir = ifelse(playDirection == "left", dir + 180, dir),
                  dir = ifelse(dir > 360, dir - 360, dir),
                  o = ifelse(playDirection == "left", o + 180, o),
                  o = ifelse(o > 360, o - 360, o)) %>%
    dplyr::left_join(teams_colors2 %>%
                       dplyr::select(team_abbr, team_color, team_color2), by = c('club' = 'team_abbr')) %>%
    dplyr::mutate(team_color = ifelse(is.na(nflId), 'black', team_color), 
                  team_color2 = ifelse(is.na(nflId), '#825736', team_color2))
  #Creating the field plot 
  animate_plot <- geom_football('NFL', 
                                display_range = 'in_bounds_only',
                                x_tran = 60,
                                y_tran = 26.65,
                                color_updates = list(
                                  field_apron = "#21ae5f",
                                  offensive_half = "#21ae5f",
                                  defensive_half = "#21ae5f",
                                  offensive_endzone = "#177b43",
                                  defensive_endzone = "#177b43")) +
    #Adding home team logo to middle of the field
    geom_nfl_logos(data = game_plays, aes(x = 60, y = 26, team_abbr = homeTeamAbbr, width = 0.25, height = 0.25)) + 
    #Adding vistor team wordmark to the left endzone
    geom_nfl_wordmarks(data = game_plays, aes(x = 5, y = 26, team_abbr = visitorTeamAbbr, width = 0.25, height = 0.25, angle = 90)) + 
    #Adding home team wordmark to the right endzone
    geom_nfl_wordmarks(data = game_plays, aes(x = 115, y = 26, team_abbr = homeTeamAbbr, width = 0.25, height = 0.25, angle = 270)) +
    #Adding the line of scrimmage to the field 
    geom_segment(data = game_plays, aes(x = yardlineNumber + 10, xend = yardlineNumber + 10, y = 0.05, yend = 53.25),
                 colour = 'black') +
    #Adding the first down marker to the field
    geom_segment(data = game_plays, aes(x = yardlineNumber + 10 + yardsToGo, xend = yardlineNumber + 10 + yardsToGo, y = 0.05, yend = 53.25),
                 color = 'yellow') + 
    #Adding the players and ball to the plot
    geom_point(data = animate_play_tracking, aes(x, y), shape = ifelse(is.na(animate_play_tracking$nflId), 18, 21),
               alpha =  ifelse(is.na(animate_play_tracking$nflId), 1, 0.7), size =  ifelse(is.na(animate_play_tracking$nflId), 3, 7),
               fill = ifelse(animate_play_tracking$club == animate_play_tracking$visitorTeamAbbr, animate_play_tracking$team_color2, animate_play_tracking$team_color),
               color = ifelse(animate_play_tracking$club == animate_play_tracking$visitorTeamAbbr, animate_play_tracking$team_color, animate_play_tracking$team_color2)) +
    #Adding the numbers to the players
    geom_text(data = animate_play_tracking, aes(x = x, y = y, label = jerseyNumber), colour = "white", 
              vjust = 0.36, size = 3.5) +
    transition_manual(animate_play_tracking$frameId)
  
  return(animate_plot)
}

#Creating a gif for a disguised coverage look from the Bills
disguised_play_tracking <- animate_play(game_plays = game_plays, tracking = tracking_week_1, 2022090800, 2908)

disguised_animation <- animate(disguised_play_tracking, fps = 10, nframes = 99, width = 800, height = 400)

anim_save('nfl_disguised_coverage.gif', disguised_animation)
knitr::include_graphics('nfl_disguised_coverage.gif')

#Creating a gif of the Rams using motion to force the Bills to show there coverage
motion_play_tracking <- animate_play(game_plays = game_plays, tracking = tracking_week_1, 2022090800, 364)

motion_animation <- animate(motion_play_tracking, fps = 10, nframes = 224, width = 800, height = 400)

anim_save('nfl_shown_coverage.gif', motion_animation)
knitr::include_graphics('nfl_shown_coverage.gif')
