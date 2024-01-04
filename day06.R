library(tidyverse)
input <- read_lines("inputs/day06.txt") |> 
  as_tibble()

# Part one ----------------------------------------------------------------

df <- input |> 
  separate_wider_delim(value, ":", names = c("var", "value")) |> 
  mutate(value = str_squish(value)) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  separate_longer_delim(everything(), " ") |> 
  mutate(across(everything(), parse_number))

poss_hold_time <- function(t, d) {
  x <- 1:t
  x[which(x * (t - x) > d)]
}

df <- df |> 
  mutate(
    poss_hold_time = map2(Time, Distance, poss_hold_time),
    num_ways = map_int(poss_hold_time, length)
  )

prod(df$num_ways)

# Part two ----------------------------------------------------------------

df2 <- input |> 
  separate_wider_delim(value, ":", names = c("var", "value")) |> 
  mutate(value = str_remove_all(value, " ")) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  mutate(across(everything(), parse_number))


poss_hold_time(df2$Time, df2$Distance) |> 
  length()