#Creating a scatter plot of acp vs epa
acp_vs_epa <- ggplot(data = game_plays_w_preds, aes(x = acp, y = expectedPoints)) +
  #making dots more translucent 
  geom_point(alpha = 0.5, color = "darkgray") + 
  #adding a linear regression line
  geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1) + 
  #adding titles
  labs(
    title = "Relationship Between Actual Coverage Probability and Expected Points",
    subtitle = "Analyzing the correlation between ACP and EPA",
    x = "Actual Coverage Probability (ACP)",
    y = "Expected Points"
  ) +
  #formatting the titles and axis
  theme_minimal(base_size = 14) + # Clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
    plot.subtitle = element_text(size = 14, hjust = 0.5), 
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 14), 
    panel.grid.minor = element_blank() 
  )

ggsave(plot = acp_vs_epa, 'acp_vs_epa.png', width = 10, height = 4)

# Fitting the linear model between epa and acp
lm_model <- lm(expectedPoints ~ acp, data = game_plays_w_preds)

# Displaying the summary of the model
model_summary <- summary(lm_model)

model_summary

# Extracting the slope (coefficient of acp)
slope <- coef(lm_model)["acp"]
slope

