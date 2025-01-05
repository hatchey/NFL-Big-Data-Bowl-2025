library(caret)
library(tidyverse)

buffalo_bill_xcoverage_preds <- game_plays_w_preds %>%
  filter(defensiveTeam == 'BUF')

buffalo_bills_xcoverage <- buffalo_bill_xcoverage_preds %>%
  group_by(predictions, label) %>%
  summarize(Frequency = n(),
            EPA = mean(expectedPoints), .groups = "drop") %>%
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  filter(Percentage > 1)

buffalo_bills_coverage <- buffalo_bill_xcoverage_preds %>% 
  group_by(label) %>%
  summarize(Frequency = n(), 
            EPA = mean(expectedPoints), .groups = "drop") %>%
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  filter(Percentage > 1)

buffalo_bills_xcoverage_head <- buffalo_bills_xcoverage %>% 
  dplyr::arrange(dplyr::desc(Percentage)) %>%
  head(8)

bills_coverage_gt <- buffalo_bills_coverage %>%
  gt::gt() %>%
  tab_header(title = md("**Buffalo Bills Coverage Frequencies**")) %>%
  cols_label(
    label = md("**Coverage**"),
    Frequency = md("**Frequency**"),
    EPA = md("**EPA**"),
    Percentage = md("**Percentage**")
  ) %>%
  fmt_number(columns = c(Frequency), decimals = 0) %>%
  fmt_number(columns = c(EPA, Percentage), decimal = 2) %>%
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(label, Frequency, EPA, Percentage))) %>% 
  cols_align(align = "center", columns = c(label, Frequency, EPA, Percentage)) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) %>%
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) %>%
  cols_width(c(Frequency, EPA, Percentage) ~ px(75)) %>%
  cols_width(c(label) ~ px(150)) %>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) %>%
  gt::data_color(columns = vars(Percentage),
                 colors = scales::col_numeric(rev(rcartocolor::carto_pal(n = 10, name = "TealRose")),
                                              domain = range(buffalo_bills_coverage$Percentage)), alpha = 0.8) %>%
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) %>%
  gtsave(filename = "bills_coverage.html")

bills_xcoverage_gt <- buffalo_bills_xcoverage_head %>%
  gt::gt() %>%
  tab_header(title = md("**Buffalo Bills Presnap vs Postsnap Coverage Frequencies**")) %>%
  cols_label(
    predictions = md("**xCoverage**"),
    label = md("**Coverage**"),
    Frequency = md("**Frequency**"),
    EPA = md("**EPA**"),
    Percentage = md("**Percentage**")
  ) %>%
  fmt_number(columns = c(Frequency), decimals = 0) %>%
  fmt_number(columns = c(EPA, Percentage), decimal = 2) %>%
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(predictions, label, Frequency, EPA, Percentage))) %>% 
  cols_align(align = "center", columns = c(predictions, label, Frequency, EPA, Percentage)) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) %>%
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) %>%
  cols_width(c(predictions, label, Frequency, EPA, Percentage) ~ px(75)) %>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) %>%
  gt::data_color(columns = vars(EPA),
                 colors = scales::col_numeric(palette = rcartocolor::carto_pal(n = 10, name = "TealRose"),
                                              domain = range(buffalo_bills_xcoverage_head$EPA)), alpha = 0.8) %>%
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) %>%
  gtsave(filename = "bills_xcoverage.html")