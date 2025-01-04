acp_vs_epa <- ggplot(data = game_plays_w_preds, aes(x = acp, y = expectedPoints)) +
  geom_point(alpha = 0.5, color = "darkgray") + # Semi-transparent points
  geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1) + # Regression line
  labs(
    title = "Relationship Between Actual Coverage Probability and Expected Points",
    subtitle = "Analyzing the correlation between ACP and EPA",
    x = "Actual Coverage Probability (ACP)",
    y = "Expected Points"
  ) +
  theme_minimal(base_size = 14) + # Clean theme
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # Centered bold title
    plot.subtitle = element_text(size = 14, hjust = 0.5), # Centered subtitle
    axis.text = element_text(size = 12), # Axis tick size
    axis.title = element_text(size = 14), # Axis title size
    panel.grid.minor = element_blank() # Remove minor gridlines
  )

ggsave(plot = acp_vs_epa, 'acp_vs_epa.png', width = 10, height = 4)

# Fit the linear model
lm_model <- lm(expectedPoints ~ acp, data = game_plays_w_preds)

# Display the summary of the model
model_summary <- summary(lm_model)

model_summary

# Extract the slope (coefficient of acp)
slope <- coef(lm_model)["acp"]
slope

