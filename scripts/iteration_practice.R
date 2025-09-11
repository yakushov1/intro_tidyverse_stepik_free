library(tidyverse)
library(readxl)
library(R.utils)
library(stringr)


# 4.4.2.  ------------------------------------------------
#запишем все пути к файлам gz
paths_gz <- list.files("input/rp5_archive/archive/", pattern = "\\.gz$", full.names = T)



# 4.4.3. ------------------------------------------------------------------
# распакуем все архивы, архивы сотрем
map(paths_gz, \(x) gunzip(x, remove = T, overwrite = T))


# 4.4.4. ------------------------------------------------------------------
paths_csv <- list.files("input/rp5_archive/archive/", pattern = "\\.csv$", full.names = T)
name <-  str_extract(paths_csv[1], "(?<=input/rp5_archive/archive//)\\w*(?=\\.)")


# 4.4.5. ------------------------------------------------------------------
test <- read_csv2(paths_csv[1],skip=6) |>
  select(Local_time = 1, T, P, DD, RRR, sss)|>
  mutate(id = name, .before =1)


# 4.4.6. ------------------------------------------------------------------

test <- test |>
  mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R")) |>
  mutate(Year = year(Local_time),
         Month = month(Local_time),
         Day = mday(Local_time),
         .after= id) |>
  select(!Local_time)


# 4.4.7. ------------------------------------------------------------------

test <- test |>
  mutate(sss = str_replace(sss, ";",""),
   DD = str_replace(DD, "Ветер, дующий с ","")) |>
  mutate(across(c(RRR,sss),parse_double))



# 4.4.8. ------------------------------------------------------------------

test <- test |>
  mutate(across(c(id,Year,Month,Day, DD), as.factor),
    T = as.numeric(T))

# или 

test <- test |>
  mutate(across(c(id:Day, DD), as.factor),
         T = as.numeric(T))


# 4.4.9. ------------------------------------------------------------------
reading <- function(path){
  name <-  str_extract(path, "(?<=input/rp5_archive/archive/)\\w*(?=\\.)")
  read_csv2(path,
            skip = 6) |> 
    select(Local_time = 1, T, P, DD, RRR, sss) |> 
    mutate(sss = as.character(sss)) |> 
    mutate(id = name,
         .before = 1)
}


# 4.4.10 ------------------------------------------------------------------
cleaning <- function(df){
  df |> 
    mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R")) |> 
    mutate(Year = year(Local_time),
           Month = month(Local_time),
           Day = mday(Local_time),
           .after = id) |> 
    select(-Local_time) |> 
    mutate(sss = str_replace(sss, ";", ""),
           DD  = str_replace(DD, "Ветер, дующий с ", "")) |>
    mutate(across(c(RRR, sss), parse_double)) |> 
    mutate(across(c(id:Day, DD), as.factor),
           T = as.numeric(T))
}


# 4.4.11 ------------------------------------------------------------------
data <- map_df(paths_csv, reading)


# 4.4.12. -----------------------------------------------------------------
data_cleaned <- cleaning(data)
