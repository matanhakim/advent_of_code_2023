library(tidyverse)
input <- read_lines("inputs/day04.txt") |> 
  as_tibble()

# Part one ----------------------------------------------------------------

split_numbers <- function(string, pattern = " ") {
  x <- str_split_1(string, pattern = pattern) |> 
    parse_integer()
  x[!is.na(x)]
}

df <- input |> 
  separate_wider_delim(value, ": ", names = c("id", "value")) |> 
  separate_wider_delim(value, " | ", names = c("num_win", "num_have")) |> 
  mutate(
    id = parse_number(id),
    num_win = map(num_win, split_numbers),
    num_have = map(num_have, split_numbers),
    num_common = map2(num_win, num_have, intersect),
    num_common_length = map_int(num_common, length),
    score = if_else(
      num_common_length == 0,
      0,
      2 ^ (num_common_length - 1)
    )
  )

sum(df$score)

# Part two ----------------------------------------------------------------

df <- df |> 
  mutate(card_num = 1)

for (i in 1:(nrow(df) - 1)) {
  if (df$num_common_length[i] != 0)
    df$card_num[(i + 1):(i + df$num_common_length[i])] <- df$card_num[(i + 1):(i + df$num_common_length[i])] + df$card_num[i]
}

sum(df$card_num)