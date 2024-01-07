library(tidyverse)
input <- read_lines("inputs/day09.txt") |> 
  as_tibble()

# Part one ----------------------------------------------------------------

predict_next <- function(x) {
  result <- 0
  while (!all(x == 0)) {
    result <- result + last(x)
    x <- (x - lag(x))[-1]
  }
  result
}

df <- input |> 
  mutate(
    value = map(value, \(x) str_split_1(x, " ") |> parse_integer()),
    result = map_int(value, predict_next)
  )

sum(df$result)

# Part two ----------------------------------------------------------------

predict_next2 <- function(x) {
  result <- first(x)
  counter <- 1
  while (!all(x == 0)) {
    x <- (x - lag(x))[-1]
    result <- result + ((-1) ^ counter) * first(x)
    counter <- counter + 1
  }
  result
}

df <- df |> 
  mutate(
    result2 = map_int(value, predict_next2)
  )

sum(df$result2)
