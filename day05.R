library(tidyverse)
input <- read_lines("inputs/day05.txt") |> 
  as_tibble()

# Part one ----------------------------------------------------------------
df_seeds <- input |> 
  slice(1) |> 
  separate_longer_delim(value, " ") |> 
  filter(!str_detect(value, ":")) |> 
  mutate(value = parse_number(value))

df <- input |>
  slice(-1) |> 
  filter(value != "") |> 
  mutate(
    value = str_trim(value),
    map_type = if_else(
      str_detect(value, ":"),
      str_remove(value, " map:"),
      NA
    )
  ) |> 
  fill(map_type) |> 
  filter(!str_detect(value, ":")) |> 
  separate_wider_delim(value, " ", names = c("dest_range_start", "source_range_start", "range_length")) |> 
  mutate(
    across(contains("range"), parse_number),
    range_diff = dest_range_start - source_range_start
  ) |> 
  relocate(map_type) |> 
  nest(.by = map_type) |> 
  separate_wider_delim(map_type, "-to-", names = c("source", "dest"))

for (i in 1:nrow(df_seeds)) {
  loc <- df_seeds$value[i]
  for (j in 1:nrow(df)) {
    for (k in 1:nrow(df$data[[j]])) {
      if (between(
        loc,
        df$data[[j]]$source_range_start[k],
        df$data[[j]]$source_range_start[k] + df$data[[j]]$range_length[k] - 1
      )) {
        loc <- loc + df$data[[j]]$range_diff[k]
        break
      }
    }
  }
  df_seeds$loc[i] <- loc
}

df_seeds |> 
  slice_min(loc, n = 1) |> 
  pull(loc)

# Part two ----------------------------------------------------------------

df_seeds2 <- df_seeds |> 
  select(value) |> 
  mutate(
    id = 2:(nrow(df_seeds) + 1) %/% 2,
    type = rep(c("seed_range_start", "seed_range_length"), length.out = nrow(df_seeds))
  ) |> 
  pivot_wider(id_cols = id, names_from = type, values_from = value)

check_location <- function(loc) {
  x <- loc
  for (i in nrow(df):1) {
    has_changed <- FALSE
    for (j in 1:nrow(df$data[[i]])) {
      if (between(x, df$data[[i]]$dest_range_start[j], df$data[[i]]$dest_range_start[j] + df$data[[i]]$range_length[j] - 1)) {
        x <- x - df$data[[i]]$range_diff[j]
        has_changed <- TRUE
        break
      }
    }
    print(c(loc, i, j, x))
    if (!has_changed & j == nrow(df$data[[i]]))
      break
  }
  i == 1 & has_changed & any(x >= df_seeds2$seed_range_start & x <= df_seeds2$seed_range_start + df_seeds2$seed_range_length - 1)
}

stop_all <- FALSE

loc <- 0
while (!stop_all) {
  stop_all <- check_location(loc)
  if (!stop_all)
    loc <- loc + 1
}


# for (m in 1:nrow(df_seeds2)) {
#   for (seed in seq_along(df_seeds2$seed_range_start[m]:(df_seeds2$seed_range_start[m] + df_seeds2$seed_range_length[m] - 1))) {
#     loc <- seed
#     for (j in 1:nrow(df)) {
#       for (k in 1:nrow(df$data[[j]])) {
#         if (between(
#           loc,
#           df$data[[j]]$source_range_start[k],
#           df$data[[j]]$source_range_start[k] + df$data[[j]]$range_length[k] - 1
#         )) {
#           loc <- loc + df$data[[j]]$range_diff[k]
#           break
#         }
#       }
#     }
#     df_seeds2$loc[m] <- min(loc, df_seeds2$loc[m])
#   }
# }
# 
# df_seeds |> 
#   slice_min(loc, n = 1) |> 
#   pull(loc)