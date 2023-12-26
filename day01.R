library(tidyverse)
library(stringi)
library(english)
input <- read_lines("inputs/day01.txt")

# Part one ----------------------------------------------------------------

recalibrate <- function(x) {
  str_c(
    str_extract(x, "[:digit:]"),
    str_extract(stri_reverse(x), "[:digit:]")
  ) |> 
  parse_integer()
}

map_int(input, recalibrate) |> 
  sum()

# Part two ----------------------------------------------------------------
num_names <- english(1:9) |> 
  as.character()

df_nums <- tibble(
  num_id = as.character(1:9),
  num_names = num_names
)

first_regex <- df_nums$num_names |> 
  str_flatten(collapse = "|") |>
  str_c("|[:digit:]")

last_regex <- c(1:9, num_names)

name_to_num <- function(x) {
  if (x %in% num_names) {
    df_nums |> 
      filter(x == num_names) |> 
      pull(num_id)
  } else 
    x
}

find_last_match <- function(string, pattern) {
  start_end <- stri_locate_all_fixed(string, pattern, overlap = TRUE) |> 
    list_c() |> 
    as_tibble() |> 
    rename(
      start = V1,
      end = V2
    ) |> 
    slice_max(order_by = end, n = 1) |> 
    as_vector()
  
  str_sub(string, start = start_end[[1]], end = start_end[[2]])
}

df <- tibble(input_line = input) |> 
  mutate(
    first_match = str_match(input_line, first_regex) |>
      map(first) |>
      map_chr(as.character) |> 
      map_chr(name_to_num),
    last_match = map(input_line, \(x) find_last_match(x, last_regex)) |>
      map_chr(name_to_num),
    result = as.numeric(first_match) * 10 + as.numeric(last_match)
  )

df$result |> sum()
