library(tidyverse)
input <- read_lines("inputs/day02.txt")
df_input <- tibble(x = input)

# Part one ----------------------------------------------------------------

df <- df_input |> 
  separate_wider_delim(x, ": ", names = c("game_id", "game_record")) |> 
  separate_longer_delim(game_record, "; ") |> 
  separate_longer_delim(game_record, ", ") |> 
  separate_wider_delim(game_record, " ", names = c("cube_num", "cube_color")) |> 
  mutate(across(c(game_id, cube_num), parse_number))
  
df_poss <- df |> 
  group_by(game_id) |> 
  filter(!any(
    (cube_color == "red" & cube_num > 12),
    (cube_color == "green" & cube_num > 13),
    (cube_color == "blue" & cube_num > 14)
  )) |> 
  ungroup()

df_poss |> 
  distinct(game_id) |> 
  summarise(sum(game_id))

# Part two ----------------------------------------------------------------

df_min <- df |> 
  group_by(game_id, cube_color) |> 
  slice_max(cube_num, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  pivot_wider(names_from = cube_color, values_from = cube_num) |> 
  mutate(result = red * green * blue)

df_min$result |> sum()
