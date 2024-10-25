library(sportyR)
library(nflplotR)
library(ggplot2)
library(gganimate)
library(dplyr)
library(nflfastR)
library(coloratio)


team_colors2 <- nflfastR::teams_colors_logos

schedule <- nflreadr::load_schedules() %>%
  dplyr::select(game_id, week, old_game_id, home_team, away_team)

plays_desc <- plays %>%
  dplyr::mutate(gameId = as.character(gameId)) %>%
  dplyr::left_join(schedule, by = c('gameId' = 'old_game_id')) %>%
  dplyr::filter(game_id = '2022_07_ATL_CIN',
                playId = 2655) %>%
  dplyr::mutate(down_text = case_when(
    down == 1 ~ '1st',
    down == 2 ~ '2nd',
    down == 3 ~ '3rd',
    down == 4 ~ '4th'),
    down_all = paste(down, '&', yardsToGo),
    quarter_text = case_when(
      quarter == 1 ~ '1st',
      quarter == 2 ~ '2nd',
      quarter == 3 ~ '3rd',
      quarter == 4 ~ '4th',
      quarter == 5 ~ 'OT'),
    clock_min = stringr::str_extract(gameClock, '[:digit:]{2}'),
    clock_sec = stringr::str_extract(gameClock, paste0("(?<=^",clock_min,"\\:)[:digit:]{2}")),
    clock_text = paste(clock_min, clock_sec, sep = ':'),
    clock_new = lubridate::ms(paste(clock_text, '.00', sep = '')))

play_track <- tracking_week_7 %>%
  dplyr::left_join(schedule, by = c('gameId' = 'old_game_id')) %>%
  dplyr::filter(game_id = '2022_07_ATL_CIN',
                playId = 2655) %>%
  dplyr::mutate()
