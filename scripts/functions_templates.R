library(tidyverse)


# 4.3.4. ------------------------------------------------------------------
read_table("input/logger/23274.txt", col_names =F) |>
  rename(id = 1, Year = 2 ) |>
  pivot_longer(cols = !c(id,Year), names_to= "Month", values_to = "Tavg") |>
                 mutate(Month = parse_number(Month) -2)


# 4.3.5. ------------------------------------------------------------------
reading <- function(path){
  read_table(path, col_names = F) |> 
    rename(id = 1, Year = 2) |> 
    pivot_longer(cols = !c(id, Year), names_to = "Month", values_to = "Tavg") |> 
    mutate(Month = parse_number(Month)-2)
}



# 4.3.6. ------------------------------------------------------------------

paths <- list.files("input/logger", full.names = T)

log_total <-  map_df(paths, reading)


