# GitHub Actions runner replicating '4. plate_discipline.R'
# Parameters are read from environment variables; defaults match the original script.

require(baseballr)
require(tidyverse)

## Configuration (env vars with original-script defaults) ---------------------
mode       <- Sys.getenv("MODE",       "game")    # "game" | "dates"
batter_id  <- as.integer(Sys.getenv("BATTER_ID", "698945"))
game_pk    <- if (nchar(Sys.getenv("GAME_PK")) > 0) as.integer(Sys.getenv("GAME_PK")) else NULL
start_date <- if (nchar(Sys.getenv("START_DATE")) > 0) Sys.getenv("START_DATE") else NULL
end_date   <- if (nchar(Sys.getenv("END_DATE"))   > 0) Sys.getenv("END_DATE")   else NULL
season     <- as.integer(Sys.getenv("SEASON", "2025"))
level_id   <- as.integer(Sys.getenv("LEVEL_ID", "12"))  # 12=AA, 11=AAA, 13=A+, 14=A

## Pull data -------------------------------------------------------------------
if (mode == "game") {
  stopifnot("Set GAME_PK env var for single-game mode." = !is.null(game_pk))
  raw_payload <- mlb_pbp(game_pk)
  batter_data <- raw_payload %>% filter(matchup.batter.id == batter_id)
} else {
  stopifnot(
    "Set START_DATE env var for dates mode." = !is.null(start_date),
    "Set END_DATE env var for dates mode."   = !is.null(end_date)
  )
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

  all_data <- map_dfr(seq_along(game_pks), function(i) {
    pk <- game_pks[i]
    message(sprintf("  [%d/%d] game_pk %d", i, length(game_pks), pk))
    tryCatch({
      pbp <- mlb_pbp(pk)
      pbp %>% filter(matchup.batter.id == batter_id)
    }, error = function(e) tibble())
  })

  batter_data <- all_data %>%
    filter(game_date >= start_date, game_date <= end_date)
}

## Metadata --------------------------------------------------------------------
batter_name <- first(batter_data$matchup.batter.fullName)
team_name   <- first(batter_data$batting_team)
date_label  <- if (mode == "game") {
  first(batter_data$game_date)
} else {
  str_c(start_date, " to ", end_date)
}

## Recode pitch outcomes -------------------------------------------------------
batter_data <- batter_data %>%
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

## Strike zone geometry --------------------------------------------------------
topKzone  <- -110; botKzone  <- -190; inKzone  <- -60;  outKzone  <- -140
topshzone <- -100; botshzone <- -200; inshzone <- -50;  outshzone <- -150

## Zone classification ---------------------------------------------------------
batter_data <- batter_data %>%
  mutate(
    px = -1 * pitchData.coordinates.x,
    py = -1 * pitchData.coordinates.y,

    in_zone = !is.na(px) & !is.na(py) &
              px >= outKzone & px <= inKzone &
              py >= botKzone & py <= topKzone,

    in_shadow = !is.na(px) & !is.na(py) &
                px >= outshzone & px <= inshzone &
                py >= botshzone & py <= topshzone &
                !in_zone,

    out_zone = !is.na(px) & !is.na(py) & !in_zone,

    swing   = outcome %in% c("Swinging Strike", "Foul", "Ball in Play"),
    contact = outcome %in% c("Foul", "Ball in Play")
  )

valid  <- batter_data %>% filter(!is.na(px) & !is.na(py))

pct <- function(num, den) {
  if (den == 0) return(NA_real_)
  round(num / den * 100, 1)
}
fmt <- function(x) ifelse(is.na(x), "N/A", paste0(x, "%"))

z_pit  <- valid %>% filter(in_zone)
o_pit  <- valid %>% filter(out_zone)
sh_pit <- valid %>% filter(in_shadow)

z_sw   <- z_pit  %>% filter(swing)
o_sw   <- o_pit  %>% filter(swing)
sh_sw  <- sh_pit %>% filter(swing)
all_sw <- valid  %>% filter(swing)

total_pit           <- nrow(valid)
swing_pct           <- pct(nrow(all_sw),         total_pit)
contact_pct         <- pct(sum(all_sw$contact),  nrow(all_sw))
z_swing_pct         <- pct(nrow(z_sw),           nrow(z_pit))
z_contact_pct       <- pct(sum(z_sw$contact),    nrow(z_sw))
o_swing_pct         <- pct(nrow(o_sw),           nrow(o_pit))
o_contact_pct       <- pct(sum(o_sw$contact),    nrow(o_sw))
shadow_swing_pct    <- pct(nrow(sh_sw),          nrow(sh_pit))
shadow_contact_pct  <- pct(sum(sh_sw$contact),   nrow(sh_sw))

## Print results ---------------------------------------------------------------
cat("\n")
cat("=================================================================\n")
cat(sprintf("  Plate Discipline: %s\n", batter_name))
cat(sprintf("  %s  |  %s\n", team_name, date_label))
cat("=================================================================\n")
cat(sprintf("  Pitches (with coords) : %d\n",                 total_pit))
cat(sprintf("    In-zone             : %d\n",                 nrow(z_pit)))
cat(sprintf("    Out-of-zone         : %d  (incl. shadow)\n", nrow(o_pit)))
cat(sprintf("    Shadow zone         : %d  (subset of out-of-zone)\n", nrow(sh_pit)))
cat("-----------------------------------------------------------------\n")
cat(sprintf("  Swing%%          : %s  (%d/%d pitches)\n",
            fmt(swing_pct),   nrow(all_sw), total_pit))
cat(sprintf("  Contact%%        : %s  (%d/%d swings)\n",
            fmt(contact_pct), sum(all_sw$contact), nrow(all_sw)))
cat("-----------------------------------------------------------------\n")
cat(sprintf("  Z-Swing%%        : %s  (%d/%d in-zone pitches)\n",
            fmt(z_swing_pct),   nrow(z_sw), nrow(z_pit)))
cat(sprintf("  Z-Contact%%      : %s  (%d/%d in-zone swings)\n",
            fmt(z_contact_pct), sum(z_sw$contact), nrow(z_sw)))
cat("-----------------------------------------------------------------\n")
cat(sprintf("  O-Swing%%        : %s  (%d/%d out-of-zone pitches)\n",
            fmt(o_swing_pct),   nrow(o_sw), nrow(o_pit)))
cat(sprintf("  O-Contact%%      : %s  (%d/%d out-of-zone swings)\n",
            fmt(o_contact_pct), sum(o_sw$contact), nrow(o_sw)))
cat("-----------------------------------------------------------------\n")
cat(sprintf("  Shadow-Swing%%   : %s  (%d/%d shadow pitches)\n",
            fmt(shadow_swing_pct),   nrow(sh_sw), nrow(sh_pit)))
cat(sprintf("  Shadow-Contact%% : %s  (%d/%d shadow swings)\n",
            fmt(shadow_contact_pct), sum(sh_sw$contact), nrow(sh_sw)))
cat("=================================================================\n")
cat("Note: Pitch locations are manually inputted by a stringer and\n")
cat("      should be approached with caution.\n\n")

## Write CSV -------------------------------------------------------------------
metrics <- tibble(
  batter              = batter_name,
  team                = team_name,
  scope               = date_label,
  total_pitches       = total_pit,
  zone_pitches        = nrow(z_pit),
  out_zone_pitches    = nrow(o_pit),
  shadow_pitches      = nrow(sh_pit),
  swing_pct           = swing_pct,
  contact_pct         = contact_pct,
  z_swing_pct         = z_swing_pct,
  z_contact_pct       = z_contact_pct,
  o_swing_pct         = o_swing_pct,
  o_contact_pct       = o_contact_pct,
  shadow_swing_pct    = shadow_swing_pct,
  shadow_contact_pct  = shadow_contact_pct
)

csv_filename <- str_c(batter_name, " - ", date_label, " - plate_discipline.csv")
write_csv(metrics, csv_filename)
message("Saved: ", csv_filename)
