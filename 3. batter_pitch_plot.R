require(baseballr)
require(tidyverse)
require(ggplot2)
require(ggthemes)
library(showtext)
require(ragg)

## Configuration -----------------------------------------------------------
## Set mode to "game" for a single game or "season" for a full season
mode      <- "season"  # "game" | "season"
game_pk   <- NULL      # Required when mode == "game"; e.g. 783714
batter_id <- 698945    # Anderson de Los Santos (default)
season    <- 2026      # Season year (used in season mode)
level_id  <- c(12)     # 12 = AA; change for other levels

## Pull data ---------------------------------------------------------------
if (mode == "game") {
  stopifnot("Set game_pk for single-game mode." = !is.null(game_pk))
  raw_payload <- mlb_pbp(game_pk)
  batter_data <- raw_payload %>% filter(matchup.batter.id == batter_id)
} else {
  # Fetch the full season schedule in a single call, then pull PBP per game
  message("Fetching ", season, " schedule (level ", level_id, ")...")
  schedule <- mlb_schedule(season = season, level_ids = level_id)
  game_pks <- unique(schedule$game_pk)
  message(length(game_pks), " games found. Pulling PBP (this may take a while)...")

  batter_data <- map_dfr(seq_along(game_pks), function(i) {
    pk <- game_pks[i]
    message(sprintf("  [%d/%d] game_pk %d", i, length(game_pks), pk))
    tryCatch({
      pbp <- mlb_pbp(pk)
      pbp %>% filter(matchup.batter.id == batter_id)
    }, error = function(e) tibble())
  })
}

## Metadata ----------------------------------------------------------------
batter_name    <- first(batter_data$matchup.batter.fullName)
team_name      <- first(batter_data$batting_team)
subtitle_label <- if (mode == "game") {
  str_c(team_name, ", ", first(batter_data$game_date))
} else {
  str_c(team_name, ", ", season, " Season")
}

## Recode pitch outcomes ---------------------------------------------------
batter_data <- batter_data %>%
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

## Strike zone geometry ----------------------------------------------------
topKzone <- -110
botKzone <- -190
inKzone  <- -60
outKzone <- -140
kZone <- data.frame(
  x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

topshzone <- -100
botshzone <- -200
inshzone  <- -50
outshzone <- -150
shzone <- data.frame(
  x = c(inshzone, inshzone, outshzone, outshzone, inshzone),
  y = c(botshzone, topshzone, topshzone, botshzone, botshzone)
)

center_y <- data.frame(x = c(-100, -100), y = c(-100, -200))
center_x <- data.frame(x = c(-50, -150),  y = c(-150, -150))

## Theme -------------------------------------------------------------------
font_add_google("Ubuntu", "Ubuntu")
font_add_google("Racing Sans One", "racing")
showtext_auto()

theme_pitch <- function() {
  theme_minimal() +
    theme(
      plot.background  = element_rect(fill = "azure",  color = NA),
      panel.background = element_rect(fill = "azure2", color = NA),
      plot.title    = element_text(hjust = 0.5, face = "bold", size = 24),
      plot.subtitle = element_text(hjust = 0.5, size = 18),
      (base_family = "Ubuntu"),
      axis.title    = element_text(face = "bold", family = "racing"),
      axis.text.x   = element_blank(),
      axis.text.y   = element_blank(),
      axis.ticks    = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title     = element_blank(),
      legend.text      = element_text(size = 14),
      strip.background = element_rect(fill = "azure3", color = NA),
      strip.text.x     = element_text(face = "bold", size = 18),
      caption.text     = element_text(size = 12)
    )
}

## Plot --------------------------------------------------------------------
pitchhand_names <- c(`R` = "RHP", `L` = "LHP")
order <- c("Swinging Strike", "Called Strike", "Ball", "Foul", "Ball in Play")

pitch_plot <- ggplot() +
  geom_point(
    size = 2.5, shape = 16,
    data = batter_data,
    aes(x = (-1 * pitchData.coordinates.x),
        y = (-1 * pitchData.coordinates.y),
        col = outcome)
  ) +
  scale_color_manual(
    limits = order,
    values = c(
      "Called Strike"   = "firebrick",
      "Swinging Strike" = "firebrick1",
      "Ball"            = "steelblue",
      "Foul"            = "grey60",
      "Ball in Play"    = "grey5"
    )
  ) +
  facet_grid(.~factor(matchup.pitchHand.code, levels = c("R", "L")),
             labeller = as_labeller(pitchhand_names)) +
  coord_equal() +
  geom_path(aes(x, y), data = kZone,  linewidth = 1, fill = "azure2") +
  geom_path(aes(x, y), data = shzone, linewidth = 1, fill = "azure3") +
  geom_path(aes(x, y), data = center_y) +
  geom_path(aes(x, y), data = center_x)

pitch_plot_output <- pitch_plot + theme_pitch() +
  labs(
    x = "", y = "",
    title    = batter_name,
    subtitle = subtitle_label,
    caption  = "Created by Conor McGovern. Pitch locations are manually inputted by a stringer and should be approached with caution."
  )

print(pitch_plot_output)

## Save PNG ----------------------------------------------------------------
ragg::agg_png(
  filename = str_c(batter_name, " - ", subtitle_label, ".png"),
  width = 1600, height = 900, units = "px", res = 300, scaling = 0.67
)
pitch_plot_output
dev.off()
