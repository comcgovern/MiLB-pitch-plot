require(tidyverse)
require(ggplot2)

## Change pitcher id for the pitcher in question
pitcher <- 690999
pitcher_data <- payload %>%
  filter(matchup.pitcher.id == pitcher)
pitcher_name <- first(pitcher_data$matchup.pitcher.fullName)
game_date <- first(pitcher_data$game_date)
team_name <- first(pitcher_data$fielding_team)

# Placeholder to transform pitch calls into B, CS, W, F, BIP
pitcher_data <- pitcher_data %>%
  mutate(outcome = recode(details.code,
                          "X" = "Ball in Play",
                          "B" = "Ball",
                          "F" = "Foul",
                          "C" = "Called Strike",
                          "S" = "Swinging Strike",
                          "W" = "Swinging Strike",
                          "*B" = "Ball",
                          "E" = "Ball in Play",
                          "L" = "Foul",
                          "D" = "Ball in Play",
                          "1" = "Ball",
                          .missing = "Ball"))

## CSW (Called Strike + Whiff) percentage
csw_pct <- round(
  sum(pitcher_data$outcome %in% c("Called Strike", "Swinging Strike")) / nrow(pitcher_data) * 100,
  1
)

## Still messing with this, but plotting strike zone
## I think there's a 10 unit "shadow zone" based on the Gameday plot

topKzone = -110
botKzone = -190
inKzone = -60
outKzone = -140
kZone = data.frame(
  x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
  , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

topshzone = -100
botshzone = -200
inshzone = -50
outshzone = -150
shzone = data.frame(
  x = c(inshzone, inshzone, outshzone, outshzone, inshzone)
  , y = c(botshzone, topshzone, topshzone, botshzone, botshzone)
)

center_y <- data.frame(x = c(-100, -100),
y = c(-100, -200))

center_x <- data.frame(x = c(-50, -150),
                       y = c(-150, -150))

##Plotting 

require(ggthemes)
library(showtext)
font_add_google("Ubuntu","Ubuntu")
font_add_google("Racing Sans One","racing")
showtext_auto()

# Theme

theme_pitch <- function() {
  theme_minimal() + # Start with a base theme for simplicity
    theme(
      plot.background = element_rect(fill = "azure", color = NA), # Fill the entire plot background
      panel.background = element_rect(fill = "azure2", color = NA), # Fill the plot panel background
      plot.title = element_text(hjust = 0.5, face = "bold", size=24),
      plot.subtitle = element_text(hjust = 0.5, size=18),
      (base_family = "Ubuntu"),
      axis.title = element_text(face = "bold", family="racing"),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=14),
      strip.background = element_rect(fill = "azure3", color = NA),
      strip.text.x = element_text(face="bold",size = 18),
      caption.text = element_text(size=12)
          )
}

# Plot

batside_names <- c(
  `R` = "RHB",
  `L` = "LHB"
)

order <- c("Swinging Strike", "Called Strike","Ball","Foul","Ball in Play")

pitch_plot <- ggplot() +
  geom_point(size=2.5,shape=16,data=pitcher_data, aes(x=(-1*pitchData.coordinates.x), y=(-1*pitchData.coordinates.y), col=outcome)) +
  scale_color_manual(limits = order,values=c("Called Strike" = "firebrick","Swinging Strike" = "firebrick1","Ball" = "steelblue","Foul" = "grey60","Ball in Play" = "grey5")) + 
  facet_grid(.~factor(matchup.batSide.code, levels=c("R","L")), labeller = as_labeller(batside_names)) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone, linewidth = 1, fill = "azure2") + 
  geom_path(aes(x, y), data = shzone, linewidth = 1, fill = "azure3") +
  geom_path(aes(x,y), data = center_y) +
  geom_path(aes(x,y), data = center_x)

pitch_plot_output <- pitch_plot + theme_pitch() +
  labs(x="",y="") + 
  labs(title = pitcher_name) +
  labs(subtitle = str_c(team_name, ", ", game_date, "  |  CSW: ", csw_pct, "%")) +
  labs(caption="Created by Conor McGovern. Pitch locations are manually inputted by a stringer and should be approached with caution.")

print(pitch_plot_output)

#Output to PNG

require(ragg)

ragg::agg_png(filename = str_c(pitcher_name," - ",game_date, ".png"), width = 1600, height = 900, units = "px", res = 300, scaling = 0.67)
pitch_plot_output
dev.off()