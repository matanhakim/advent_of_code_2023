library(tidyverse)
input <- read_lines("inputs/day08.txt") |> 
  as_tibble()

# Part one ----------------------------------------------------------------

instructions <- input[[1, 1]] |> 
  str_split_1("")

df <- input |> 
  slice_tail(n = -2) |> 
  separate_wider_delim(value, " = (", names = c("node", "sides")) |> 
  separate_wider_delim(sides, ", ", names = c("L", "R")) |> 
  mutate(R = str_remove(R, "\\)"))

current_node <- "AAA"
counter <- 0
i <- 1
while (current_node != "ZZZ") {
  df_node <- df |> 
    filter(current_node == node)
  
  current_node <- case_match(
    instructions[i],
    "R" ~ df_node$R,
    "L" ~ df_node$L
  )
  
  counter <- counter + 1
  if (i == length(instructions))
    i <- 1
  else
    i <- i + 1
}
counter

# Part two ----------------------------------------------------------------

counter <- 0
i <- 1
all_z <- FALSE
df_node <- df |> 
  filter(str_sub(node, 3, 3) == "A")

while (!all_z) {
  df_node <- df_node |> 
    rename_with(\(x) "new_node", contains(instructions[i]))
  
  df_node <- df |> 
    semi_join(df_node, join_by(node == new_node))
  
  counter <- counter + 1
  if (i == length(instructions))
    i <- 1
  else
    i <- i + 1
  
  if (counter %% 10000 == 0)
    print(counter)
  
  if (all(str_sub(df_node$node, 3, 3) == "Z"))
    all_z <- TRUE
}
