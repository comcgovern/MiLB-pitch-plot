# GitHub Actions runner replicating '3. batter_pitch_plot.R'
# Parameters are read from environment variables; defaults match the original script.

require(baseballr)
require(tidyverse)
require(ggplot2)
require(ggthemes)
library(showtext)
require(ragg)
require(mgcv)
# MASS::kde2d is called with its namespace qualifier to avoid masking dplyr::select.

## Configuration (env vars with original-script defaults) ------------------
mode      <- Sys.getenv("MODE",      "season")   # "game" | "season"
plot_type <- Sys.getenv("PLOT_TYPE", "scatter")  # "scatter" | "heatmap"
game_pk   <- if (nchar(Sys.getenv("GAME_PK")) > 0) as.integer(Sys.getenv("GAME_PK")) else NULL
batter_id <- as.integer(Sys.getenv("BATTER_ID", "698945"))
season    <- as.integer(Sys.getenv("SEASON",    "2026"))
level_id  <- as.integer(Sys.getenv("LEVEL_ID",  "12"))

## Heatmap tuning (ignored when plot_type == "scatter") --------------------
grid_step   <- as.integer(Sys.getenv("GRID_STEP",   "2"))
gam_k       <- as.integer(Sys.getenv("GAM_K",       "30"))
rv_limit    <- as.numeric(Sys.getenv("RV_LIMIT",    "0.08"))
min_facet_n <- as.integer(Sys.getenv("MIN_FACET_N", "25"))

## Pull data ---------------------------------------------------------------
## league_data keeps every pitch in the fetched games (all batters) so we can
## fit a league baseline surface; batter_data is the subset for this batter.
if (mode == "game") {
  stopifnot("Set GAME_PK env var for single-game mode." = !is.null(game_pk))
  league_data <- mlb_pbp(game_pk)
} else {
  # Fetch only the games this batter appeared in (via their game log),
  # rather than pulling PBP for every game in the league schedule.
  message("Fetching game log for batter ", batter_id, " (", season, " season)...")
  # sportId must match the level (12 = AA, 11 = AAA, 13 = A+, 14 = A) so the
  # API returns MiLB games; without it the endpoint returns MLB data only.
  game_log_url <- paste0(
    "https://statsapi.mlb.com/api/v1/people/", batter_id,
    "/stats?stats=gameLog&season=", season,
    "&group=hitting&gameType=R&sportId=", level_id
  )
  game_log_raw <- jsonlite::fromJSON(game_log_url)
  game_pks <- unique(game_log_raw$stats$splits[[1]]$game$gamePk)
  message(length(game_pks), " games found in batter's game log. Pulling PBP...")

  league_data <- map_dfr(seq_along(game_pks), function(i) {
    pk <- game_pks[i]
    message(sprintf("  [%d/%d] game_pk %d", i, length(game_pks), pk))
    tryCatch(mlb_pbp(pk), error = function(e) tibble())
  })
}

batter_data <- league_data %>% filter(matchup.batter.id == batter_id)

## Metadata ----------------------------------------------------------------
batter_name    <- first(batter_data$matchup.batter.fullName)
team_name      <- first(batter_data$batting_team)
bat_sides      <- unique(na.omit(batter_data$matchup.batSide.code))
bat_side_label <- if (length(bat_sides) > 1) "Switch" else if ("L" %in% bat_sides) "LHB" else "RHB"
subtitle_label <- if (mode == "game") {
  str_c(team_name, ", ", first(batter_data$game_date), " | ", bat_side_label)
} else {
  str_c(team_name, ", ", season, " Season | ", bat_side_label)
}

## Recode pitch outcomes ---------------------------------------------------
recode_outcome <- function(df) {
  df %>%
    mutate(outcome = recode(details.code,
                            "X"  = "Ball in Play",
                            "B"  = "Ball",
                            "F"  = "Foul",
                            "C"  = "Called Strike",
                            "S"  = "Swinging Strike",
                            "W"  = "Swinging Strike",
                            "*B" = "Ball",
                            "E"  = "Ball in Play",
                            "L"  = "Foul",
                            "D"  = "Ball in Play",
                            "1"  = "Ball",
                            .missing = "Ball"))
}
batter_data <- recode_outcome(batter_data)
league_data <- recode_outcome(league_data)

## Strike zone geometry ----------------------------------------------------
topKzone <- -110; botKzone <- -190; inKzone <- -60; outKzone <- -140
kZone <- data.frame(
  x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

topshzone <- -100; botshzone <- -200; inshzone <- -50; outshzone <- -150
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
  theme_minimal(base_family = "Ubuntu") +
    theme(
      plot.background  = element_rect(fill = "azure",  color = NA),
      panel.background = element_rect(fill = "azure2", color = NA),
      plot.title    = element_text(hjust = 0.5, face = "bold", size = 24),
      plot.subtitle = element_text(hjust = 0.5, size = 18),
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
      plot.caption     = element_text(size = 12)
    )
}

## Plot --------------------------------------------------------------------
make_hand_labels <- function(df) {
  n <- df %>% filter(matchup.pitchHand.code %in% c("R", "L")) %>%
    count(matchup.pitchHand.code) %>% deframe()
  c(
    `R` = paste0("RHP (n=", if ("R" %in% names(n)) n["R"] else 0L, ")"),
    `L` = paste0("LHP (n=", if ("L" %in% names(n)) n["L"] else 0L, ")")
  )
}
zone_overlay <- list(
  geom_path(aes(x, y), data = kZone,    linewidth = 1),
  geom_path(aes(x, y), data = shzone,   linewidth = 1, linetype = "dashed"),
  geom_path(aes(x, y), data = center_y, linewidth = 0.4),
  geom_path(aes(x, y), data = center_x, linewidth = 0.4)
)

if (plot_type == "heatmap") {

  ## Per-pitch run value ---------------------------------------------------
  ## Pre-pitch run expectancy by count (MLB-average values from Tango et al.,
  ## "The Book"). Used for both non-terminal transitions and as the baseline
  ## subtracted from terminal event values. Reused for MiLB as a reasonable
  ## approximation since the heatmap only needs relative hot/cold signal.
  re_count <- c(
    "0-0" =  0.000, "0-1" = -0.038, "0-2" = -0.088,
    "1-0" =  0.032, "1-1" = -0.015, "1-2" = -0.069,
    "2-0" =  0.086, "2-1" =  0.040, "2-2" = -0.038,
    "3-0" =  0.174, "3-1" =  0.116, "3-2" =  0.057
  )
  event_rv <- c(
    single = 0.44, double = 0.74, triple = 1.01, home_run = 1.40,
    walk = 0.33, hit_by_pitch = 0.35, strikeout = -0.27,
    strikeout_double_play = -0.27
  )
  bip_out_rv <- -0.27

  re <- function(b, s) {
    b <- pmin(pmax(b, 0L), 3L); s <- pmin(pmax(s, 0L), 2L)
    unname(re_count[paste0(b, "-", s)])
  }

  add_run_value <- function(df) {
    df %>%
      mutate(
        px = -1 * pitchData.coordinates.x,
        py = -1 * pitchData.coordinates.y,
        b0 = suppressWarnings(as.integer(count.balls.start)),
        s0 = suppressWarnings(as.integer(count.strikes.start)),
        re_before = re(b0, s0),
        ev_lookup = ifelse(
          outcome == "Ball in Play",
          coalesce(unname(event_rv[as.character(result.eventType)]), bip_out_rv),
          NA_real_
        ),
        run_value = case_when(
          outcome == "Ball in Play"                      ~ ev_lookup                   - re_before,
          outcome == "Ball"            & b0 == 3L        ~ unname(event_rv["walk"])    - re_before,
          outcome == "Ball"                              ~ re(b0 + 1L, s0)             - re_before,
          outcome %in% c("Swinging Strike", "Called Strike") & s0 == 2L ~ unname(event_rv["strikeout"]) - re_before,
          outcome %in% c("Swinging Strike", "Called Strike")            ~ re(b0, s0 + 1L)              - re_before,
          outcome == "Foul"            & s0 <  2L        ~ re(b0, s0 + 1L)             - re_before,
          outcome == "Foul"            & s0 == 2L        ~ 0,
          TRUE                                           ~ NA_real_
        )
      ) %>%
      filter(!is.na(px), !is.na(py), !is.na(run_value),
             matchup.pitchHand.code %in% c("R", "L"))
  }

  batter_rv <- add_run_value(batter_data)
  league_rv <- add_run_value(league_data)
  pitchhand_names <- make_hand_labels(batter_rv)

  x_seq <- seq(-180, -20,  by = grid_step)
  y_seq <- seq(-220, -80,  by = grid_step)
  base_grid <- expand.grid(px = x_seq, py = y_seq)

  fit_surface <- function(d) {
    if (nrow(d) < min_facet_n) return(NULL)
    tryCatch(
      mgcv::gam(run_value ~ s(px, py, k = gam_k), data = d),
      error = function(e) NULL
    )
  }

  heatmap_rows <- purrr::map_dfr(c("R", "L"), function(hand) {
    d_b <- batter_rv %>% filter(matchup.pitchHand.code == hand)
    d_l <- league_rv %>% filter(matchup.pitchHand.code == hand)
    if (nrow(d_b) < min_facet_n) return(tibble())

    gam_b <- fit_surface(d_b)
    gam_l <- fit_surface(d_l)
    if (is.null(gam_b)) return(tibble())

    grid <- base_grid
    grid$pred_b <- as.numeric(predict(gam_b, newdata = grid))
    grid$pred_l <- if (!is.null(gam_l)) as.numeric(predict(gam_l, newdata = grid)) else 0
    grid$rv_delta <- if (mode == "game") grid$pred_b else grid$pred_b - grid$pred_l

    kd <- MASS::kde2d(
      d_b$px, d_b$py,
      n    = c(length(x_seq), length(y_seq)),
      lims = c(min(x_seq), max(x_seq), min(y_seq), max(y_seq))
    )
    dens <- as.vector(kd$z)
    cutoff <- stats::quantile(dens, 0.80, na.rm = TRUE)
    grid$alpha_w <- pmin(1, dens / cutoff)

    grid$matchup.pitchHand.code <- hand
    grid
  })

  pitch_plot <- ggplot() +
    geom_raster(
      data = heatmap_rows,
      aes(x = px, y = py, fill = rv_delta, alpha = alpha_w),
      interpolate = TRUE
    ) +
    geom_contour(
      data = heatmap_rows,
      aes(x = px, y = py, z = rv_delta),
      breaks    = c(-0.04, 0, 0.04),
      colour    = "grey25",
      linewidth = 0.3
    ) +
    scale_fill_gradient2(
      low = "steelblue", mid = "white", high = "firebrick",
      midpoint = 0, limits = c(-rv_limit, rv_limit),
      oob = scales::squish,
      name = if (mode == "game") "Runs/pitch" else "Runs/pitch\nvs league",
      breaks = c(-rv_limit, 0, rv_limit),
      labels = c("Cold", "Avg", "Hot")
    ) +
    scale_alpha_identity() +
    facet_grid(.~factor(matchup.pitchHand.code, levels = c("R", "L")),
               labeller = as_labeller(pitchhand_names)) +
    coord_equal(xlim = c(-180, -20), ylim = c(-220, -80), expand = FALSE) +
    zone_overlay

} else {

  pitchhand_names <- make_hand_labels(batter_data)
  order <- c("Swinging Strike", "Called Strike", "Ball", "Foul", "Ball in Play")

  pitch_plot <- ggplot() +
    geom_point(size = 2.5, shape = 16, data = batter_data,
               aes(x = (-1 * pitchData.coordinates.x),
                   y = (-1 * pitchData.coordinates.y),
                   col = outcome)) +
    scale_color_manual(
      limits = order,
      values = c("Called Strike"   = "firebrick",
                 "Swinging Strike" = "firebrick1",
                 "Ball"            = "steelblue",
                 "Foul"            = "grey60",
                 "Ball in Play"    = "grey5")) +
    facet_grid(.~factor(matchup.pitchHand.code, levels = c("R", "L")),
               labeller = as_labeller(pitchhand_names)) +
    coord_equal() +
    zone_overlay

}

pitch_plot_output <- pitch_plot + theme_pitch() +
  labs(x = "", y = "",
       title    = batter_name,
       subtitle = subtitle_label,
       caption  = "Created by Conor McGovern. Pitch locations are manually inputted by a stringer and should be approached with caution.")

print(pitch_plot_output)

ragg::agg_png(filename = str_c(batter_name, " - ", gsub(" | ", " - ", subtitle_label, fixed = TRUE), ".png"),
              width = 1600, height = 900, units = "px", res = 300, scaling = 0.67)
pitch_plot_output
dev.off()

message("Saved: ", str_c(batter_name, " - ", gsub(" | ", " - ", subtitle_label, fixed = TRUE), ".png"))
