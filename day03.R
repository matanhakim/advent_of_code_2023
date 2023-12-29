library(tidyverse)
input <- read_lines("inputs/day03.txt")
mt <- str_split_fixed(input, "", n = str_length(input[1]))

# Part one ----------------------------------------------------------------

digits <- as.character(0:9)

check_num <- function(mt, i, j1, j2) {
  u <- mt[i - 1, (j1 - 1):j2]
  d <- character()
  l <- mt[i, j1 - 1]
  r <- character()
  if (i != nrow(mt))
    d <- mt[i + 1, (j1 - 1):j2]
  if (j2 != ncol(mt))
    r <- mt[(i - 1):i, j2 + 1]
  if (i != nrow(mt) & j2 != ncol(mt))
    d <- c(d, mt[i + 1, j2 + 1])
  
  !all(c(u, d, l, r) %in% c(digits, "."))
}

roll_sum <- 0
x <- 0
num_count <- 0
df_numbers <- tibble(
  num = 0,
  i = 0,
  j_start = 0,
  j_end = 0,
  .rows = 0
)
for (i in 1:nrow(mt)) {
  j_start <- 0
  for (j in 1:ncol(mt)) {
    if (j_start == 0) {
      if (mt[i, j] %in% digits)
        j_start <- j
    } else {
      if (!mt[i, j] %in% digits | j == ncol(mt)) {
        j_end <- j - 1 + (j == ncol(mt) & mt[i, j] %in% digits)
        if (check_num(mt, i, j_start, j_end)) {
          x <- mt[i, j_start:j_end] |> 
            str_flatten() |> 
            parse_integer()
          roll_sum <-  roll_sum + x
          df_numbers <- df_numbers |> 
            add_row(
              num = x,
              i = i,
              j_start = j_start,
              j_end = j_end,
            )
        }
        j_start <- 0
      }
    }
  }
}

roll_sum

# Part two ----------------------------------------------------------------

df_gears <- which(mt == "*", arr.ind = TRUE) |> 
  as_tibble()

count_gear <- function(row, col) {
  df_part_numbers <- df_numbers |> 
    filter(
      row >= i - 1 & row <= i + 1,
      (col >= j_start - 1 & col <= j_start + 1) | 
        (col >= j_end - 1 & col <= j_end + 1)
    )
  
  if (nrow(df_part_numbers) == 2)
    prod(df_part_numbers$num)
  else
    0
}

df_gears <- df_gears |> 
  mutate(
    gear_ratio = map2_int(row, col, count_gear)
  )

sum(df_gears$gear_ratio)