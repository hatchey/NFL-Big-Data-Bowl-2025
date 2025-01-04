library(gt)

def_team_xcoverage_gt <- def_team_xcoverage %>%
  mutate(defensiveTeam = purrr::map(defensiveTeam, gt::html)) %>%
  gt::gt() %>%
  tab_header(title = md("**Weeks 1-9 Defensive Actual Coverage Probability**")) %>%
  cols_label(
    defensiveTeam = md("**Team**"),
    acp = md("**Actual Coverage Probability**"),
  ) %>%
  fmt_number(columns = c(acp), decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(defensiveTeam, acp))) %>% 
  cols_align(align = "center", columns = c(defensiveTeam, acp)) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) %>%
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) %>%
  text_transform(locations = cells_body(c(defensiveTeam)),
                 fn = function(x) web_image(url = paste0('https://github.com/nflverse/nflfastR-data/raw/master/wordmarks/', x, '.png'))) %>%
  cols_width(c(acp) ~ px(75)) %>%
  cols_width(c(defensiveTeam) ~ px(155)) %>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) %>%
  gt::data_color(columns = vars(acp),
                 colors = scales::col_numeric(palette = rcartocolor::carto_pal(n = 10, name = "TealRose"),
                                              domain = range(def_team_xcoverage$acp)), alpha = 0.8) %>%
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) %>%
  gtsave(filename = "defensive_xcoverage.html")

off_team_xcoverage_gt <- off_team_xcoverage %>%
  mutate(possessionTeam = purrr::map(possessionTeam, gt::html)) %>%
  gt::gt() %>%
  tab_header(title = md("**Weeks 1-9 Offensive Actual Coverage Probability**")) %>%
  cols_label(
    possessionTeam = md("**Team**"),
    acp = md("**Actual Coverage Probability**")
  ) %>%
  fmt_number(columns = c(acp), decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(possessionTeam, acp))) %>% 
  cols_align(align = "center", columns = c(possessionTeam, acp)) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) %>%
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) %>%
  text_transform(locations = cells_body(c(possessionTeam)),
                 fn = function(x) web_image(url = paste0('https://github.com/nflverse/nflfastR-data/raw/master/wordmarks/', x, '.png'))) %>%
  cols_width(c(acp) ~ px(75)) %>%
  cols_width(c(possessionTeam) ~ px(155)) %>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) %>%
  gt::data_color(columns = vars(acp),
                 colors = scales::col_numeric(palette = rev(rcartocolor::carto_pal(n = 10, name = "TealRose")),
                                              domain = range(off_team_xcoverage$acp)), alpha = 0.8) %>%
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) %>%
  gtsave(filename = "offensive_xcoverage.html")