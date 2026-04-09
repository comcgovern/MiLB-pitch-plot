# GitHub Actions runner replicating '1. get_pbp.R'
# Parameters are read from environment variables; defaults match the original script.

require(baseballr)
require(tidyverse)
require(openxlsx)

game_date <- Sys.getenv("GAME_DATE", "2025-07-23")
game_pk   <- as.integer(Sys.getenv("GAME_PK",  "783714"))
level_id  <- as.integer(Sys.getenv("LEVEL_ID", "12"))

## Look up games on date
games <- get_game_pks_mlb(date = game_date, level_id = c(level_id))
print(games %>%
  select(game_pk, gameDate, teams.away.team.name, teams.home.team.name) %>%
  slice(1:10))

## Pull all PBP data from game ID
payload <- mlb_pbp(game_pk)

write.xlsx(payload, file = "my_data.xlsx")
message("Saved: my_data.xlsx")
