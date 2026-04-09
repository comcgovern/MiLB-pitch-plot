require(baseballr)
require(tidyverse)
require(openxlsx)

## Look up AA games on date (change date)
games <- get_game_pks_mlb(date = '2025-07-23',
                          level_id = c(12))
games %>%
  select(game_pk, gameDate, teams.away.team.name, teams.home.team.name) %>%
  slice(1:10)

## Pull all PBP data from game ID
payload <- mlb_pbp(783714)

write.xlsx(payload, file = "my_data.xlsx")