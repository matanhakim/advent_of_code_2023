library(tidyverse)
library(janitor)
input <- read_lines("inputs/day07.txt") |> 
  as_tibble()

# Part one ----------------------------------------------------------------

number_hand <- function(hand) {
  x <- rep(0, str_length(hand))
  for (i in 1:str_length(hand)) {
    x[i] <- case_match(
      str_sub(hand, i, i),
      "A" ~ 14,
      "K" ~ 13,
      "Q" ~ 12,
      "J" ~ 11,
      "T" ~ 10,
      .default = suppressWarnings(parse_number(str_sub(hand, i, i))),
      .ptype = integer()
    )
  }
  x
}

classify_hand <- function(hand_list) {
  repeats <- hand_list |> 
    tabyl() |> 
    arrange(desc(n)) |> 
    pull(n)
  
  add <- 0
  if (!is.na(repeats[2])) {
    if (repeats[2] == 2)
      add <- 0.5
  }
  repeats[1] + add
}

df <- input |> 
  separate_wider_delim(value, " ", names = c("hand_cards", "bid")) |> 
  mutate(
    bid = parse_number(bid),
    hand_list = map(hand_cards, number_hand),
    hand_type = map_dbl(hand_list, classify_hand),
    c = hand_list
  ) |> 
  unnest_wider(c, names_sep = "") |> 
  arrange(desc(hand_type), across(c1:c5, desc)) |> 
  mutate(
    rank = length(hand_cards):1,
    result = bid * rank
  )
sum(df$result)

# Part two ----------------------------------------------------------------

classify_hand2 <- function(hand_list) {
  repeats <- hand_list |> 
    tabyl() |> 
    filter(hand_list != 1) |> 
    arrange(desc(n)) |> 
    pull(n)
  
  n_j <- hand_list[which(hand_list == 1)] |> 
    length()
  
  add <- 0
  if (!is.na(repeats[2])) {
    if (repeats[2] == 2)
      add <- 0.5
  }
  if (length(repeats) > 0)
    repeats[1] + n_j + add
  else
    n_j
}

df2 <- df |> 
  mutate(
    across(c1:c5, \(x) replace(x, x == 11, 1)),
    hand_list = map(hand_list, \(x) replace(x, x == 11, 1)),
    j_times = str_count(hand_cards, "J"),
    hand_type = map_dbl(hand_list, classify_hand2)
  ) |> 
  arrange(desc(hand_type), across(c1:c5, desc)) |> 
  mutate(
    rank = length(hand_cards):1,
    result = bid * rank
  )
sum(df2$result)