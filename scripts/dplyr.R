library(tidyverse)
library(readxl)

length(system.file(package='readxl'))>0


# 2.1.7. ------------------------------------------------------------------

read_table("input/02_example.txt")


# 2.1.8. ------------------------------------------------------------------

example <- read_table("input/02_example.txt", col_names = F)


# 2.1.10 ------------------------------------------------------------------

example <- read_table("input/02_example.txt", 
                      col_names = c("Station, Year, Month, Day, Tmin, Tavg, Tmax, Pr") )


# 2.1.11 ------------------------------------------------------------------

a <- c("1", 2, 3, 4)


# 2.1.12 ------------------------------------------------------------------

a <- c(1, 2, 3, F)


# 2.1.13 ------------------------------------------------------------------

read_csv2("input/03_prec_info.csv")


# 2.2.2. ------------------------------------------------------------------

weather_stations <- read_table("input/02_example.txt", 
                               col_names = c("Station", "Year", "Month", "Day",
                                             "Tmin", 'Tavg', "Tmax", "Pr"))

summary(weather_stations)


# 2.2.3 -------------------------------------------------------------------

weather_stations |> 
  filter(Tmin<5)


# 2.2.4. ------------------------------------------------------------------
weather_stations |> 
  filter(Year == 1960)


# 2.2.5. ------------------------------------------------------------------

weather_stations |> 
  filter(Year %in% c(1960, 1978, 1981))

# 2.2.6 -------------------------------------------------------------------

weather_stations |> 
  filter(Month %in% c(6,7,8))

# 2.2.7. ------------------------------------------------------------------

weather_stations |> 
  filter(Year %in% c(1961:1970) & Month %in% c(10, 11,12))

# 2.2.8 -------------------------------------------------------------------

weather_stations |> 
  filter(!(Month %in% c(12, 1, 2)))


# 2.2.9 -------------------------------------------------------------------

weather_stations |> 
  filter(Year %in% c(1961:1969, 1978:1990) &
           !(Month %in% c(1,3,7))
         & Tmax < 25)


# 2.2.10 ------------------------------------------------------------------

round(nrow(weather_stations |> 
  filter(Tmin>Tmax)) / nrow(weather_stations), 3)


# 2.2.12 ------------------------------------------------------------------

weather_stations |> 
  filter(Year>=1979 & Month == 6) |> 
  arrange(Tavg)


# 2.2.13 ------------------------------------------------------------------

weather_stations |> 
  filter(!(Year %in% c(1978, 1990)) & Month %in% c(1,6) & Tmin>-5) |> 
  arrange(Year, desc(Tavg))


# 2.2.14 ------------------------------------------------------------------

weather_stations |> 
  filter((Station == 21982 & Year %in% c(1967:1990) & Month %in% c(6,7,8) & Tmin>-4) |
        (Station == 28440 & Year <=1970 & Month <=6)) |> 
  arrange(Tavg)



# 2.3.2. ------------------------------------------------------------------

sochi <- read_excel('input/01_import_sochi.xls')

sochi_selected <- sochi |> 
  select(1:4, 13) |> 
  filter(T > 15 & T < 22 & P<764) |> 
  arrange(desc(Td))

summary(sochi_selected)


# 2.3.3. ------------------------------------------------------------------

sochi |> 
  select(Ff, ff10) |> 
  filter(is.na(ff10)==F & Ff>ff10)


# 2.3.4. ------------------------------------------------------------------

sochi |>
  select(where(is.character))

# 2.3.5. ------------------------------------------------------------------

sochi |> 
  select(Local_time=1) |> 
  arrange(Local_time)

# 2.3.6. ------------------------------------------------------------------

sochi |> 
  rename(Local_time = 1)

# 2.3.8. ------------------------------------------------------------------
min((sochi |> 
  mutate(P_diff = P-P0))$P_diff)


# 2.3.10 ------------------------------------------------------------------
sochi <- read_excel('input/01_import_sochi.xls')
a <- sochi |> 
  mutate(P_diff = P - P0,
         T_sqr = T**2,
         Wind = length(WW),
         .before = 1) |> 
  rename(Local_time = 4)

max(a[1])


# 2.3.12. -----------------------------------------------------------------
a |> 
  mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R")) |> 
  arrange(Local_time)


# 2.3.14 ------------------------------------------------------------------

max((sochi |> 
  relocate(where(is.numeric), .after = DD))[6])


# 2.4.1 -------------------------------------------------------------------

climatic_data <- read_table('input/02_example.txt', col_names = c("Station", "Year", 
                                                                  "Month", "Day", "Tmin",
                                                                  "Tavg", "Tmax", "Pr"))

# 2.4.2. ------------------------------------------------------------------

climatic_data |> 
  group_by(Station) |> 
  summarise(Pr_avg = mean(Pr))


# 2.4.3. ------------------------------------------------------------------
climatic_data |> 
  group_by(Station) |> 
  summarise(Pr_avg = mean(Pr, na.rm = T)) |> 
  filter(Station == 34880)


# 2.4.4. ------------------------------------------------------------------

climatic_data |> 
  group_by(Year) |> 
  summarise(Tmin = mean(Tmin, na.rm = T),
            Tmax = max(Tmax, na.rm = T),
            Pr = sum(Pr, na.rm = T)) |> 
  arrange(desc(Pr))


# 2.4.5. ------------------------------------------------------------------

climatic_data |> 
  group_by(Station, Month) |> 
  summarise(Tmin = mean(Tmin, na.rm = T)) |> 
  arrange(desc(Tmin))


# 2.4.6. ------------------------------------------------------------------

climatic_data |> 
  group_by(Month) |> 
  summarise(Tmin = round(mean(Tmin, na.rm = T), 2))


# 2.4.7. ------------------------------------------------------------------
climatic_data |> 
  group_by(Station) |> 
  summarise(Pr = sum(Pr, na.rm = T)) |> 
  slice_max(Pr, n = 5)


# 2.4.8. ------------------------------------------------------------------
# без ungroup
climatic_data |> 
  group_by(Station, Month) |> 
  summarise(Tavg_max = max(Tavg, na.rm = T),
            Tavg_min = min(Tavg, na.rm = T)) |> 
  mutate(Tdiff = Tavg_max - Tavg_min) |> 
  filter(Month == 7) |> 
  arrange(desc(Tdiff))

# с ungroup
climatic_data |> 
  group_by(Station, Month) |> 
  summarise(Tavg_max = max(Tavg, na.rm = T),
            Tavg_min = min(Tavg, na.rm = T)) |> 
  mutate(Tdiff = Tavg_max - Tavg_min) |> 
  filter(Month == 7) |> 
  ungroup() |> 
  slice_max(Tdiff, n = 3)

# 2.4.10 ------------------------------------------------------------------
climatic_data |> 
  filter(Station == 21982) |> 
  mutate(Pr_filled = case_when(is.na(Pr) == T ~ median(Pr, na.rm = T), .default = Pr),
         Tavg_filled = case_when(is.na(Tavg) == T~ median(Tavg, na.rm = T), .default = Tavg)) |> 
  summarise(Tavg_mean = mean(Tavg, na.rm = T), 
            Pr_mean = mean(Pr, na.rm = T),
            Tavg_filled_mean = mean(Tavg_filled),
            Pr_filled_mean = mean(Pr_filled)) |> 
  mutate(Tavg_diff = round(Tavg_mean - Tavg_filled_mean, 2),
         Pr_diff = round(Pr_mean - Pr_filled_mean, 2))\


# 2.4.11. -----------------------------------------------------------------

climatic_data |> 
  group_by(Station, Year, Month) |> 
  mutate(Tmin_new = case_when(Tmin>Tmax ~ mean(Tmin, na.rm = T), .default = Tmin)) |> 
  mutate(T_diff = Tmin-Tmin_new) |> 
  ungroup() |> 
  summarise(T_diff_sum = round(sum(T_diff, na.rm = T)))


# 2.5.2. ------------------------------------------------------------------
annual_temp <- climatic_data |> 
  group_by(Station, Year) |> 
  summarise(Tavg = mean(Tavg, na.rm = T))

annual_temp |> 
  filter(Year == 1969) |> 
  arrange(desc(Tavg))


# 2.5.3. ------------------------------------------------------------------
annual_temp |> 
  pivot_wider(id_cols = Year, names_from = Station, values_from = Tavg) |> 
  summarise(mean(21982))


# 2.5.4. ------------------------------------------------------------------

"St_21982"


# 2.5.5. ------------------------------------------------------------------

climatic_data |> 
  filter(Station == 21982) |> 
  group_by(Year, Month) |> 
  summarise(Pr_sum = sum(Pr)) |> 
  pivot_wider(id_cols = Year, names_from = Month, values_from = Pr_sum)



# 2.5.6. ------------------------------------------------------------------
climatic_data |>
  filter(Station ==  22028) |>
  group_by(Year, Month) |>
  summarise(Tavg_median= median(Tavg)) |>
  pivot_wider(id_cols = Year,
              names_from = Month,
              values_from = Tavg_median,
              names_prefix = "M_") |>
  ungroup() |>
  summarise(N = mean(M_10, na.rm = T))


# 2.5.7. ------------------------------------------------------------------

sochi <- read_excel("input/01_import_sochi.xls") |>
  select(1:4) |>
  rename(Local_time = 1) |>
  mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R"))


# 2.5.8. ------------------------------------------------------------------

sochi <- read_excel("input/01_import_sochi.xls") |>
  select(1:4) |>
  rename(Local_time = 1) |>
  mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R")) |> 
  mutate(Y = year(Local_time),
         M = month(Local_time),
         D = mday(Local_time)) |> 
  select(-Local_time) |> 
  group_by(Y, M) |> 
  summarise(T = mean(T),
            P0 = mean(P0),
            P = mean(P))


# 2.5.10 ------------------------------------------------------------------
sochi <- read_excel("input/01_import_sochi.xls") |> 
  select(1:4) |> 
  rename(Local_time = 1) |> 
  mutate(Local_time = as_datetime(Local_time, format = "%d.%m.%Y %R")) |> 
  mutate(Y = year(Local_time),
         M = month(Local_time),
         D = mday(Local_time)) |> 
  select(-Local_time) |> 
  group_by(Y) |> 
  summarise(T = min(T),
            P0 = min(P0),
            P = min(P)) |> 
  pivot_longer(cols = !Y, names_to = 'Parametr', values_to ='Val')
  


# 2.5.11 ------------------------------------------------------------------
round(min(sochi$Val),2)



# 2.6.2. ------------------------------------------------------------------

temperature <- read_excel("input/dplyr_join/23776_TTTR.xlsx") |>
  select(Y = "год", M = "месяц", D = "день", Tavg = "Тср", Pr = "осадки") |>
  group_by(Y, M) |>
  summarise(
    Tavg = mean(Tavg, na.rm = T),
    Pr_avg = mean(Pr, na.rm = T)
  )


# 2.6.3. ------------------------------------------------------------------

snow <- read_csv2("input/dplyr_join/23776__snow.csv") |>
  select(Y = "Год", M = "Месяц", D = "День", Sn_depth = "Высота_снежного_покрова") |>
  group_by(Y, M) |>
  summarise(Sn_depth_avg = mean(Sn_depth, na.rm = T))



# 2.6.4. ------------------------------------------------------------------
climatic_data <- temperature |>
  left_join(snow, by = c("Y", "M"))

nrow(climatic_data) - nrow(snow)


# 2.6.5. ------------------------------------------------------------------

summary(climatic_data)


# 2.6.6. ------------------------------------------------------------------
climatic_data <- temperature |>
  right_join(snow, by = c("Y", "M"))

summary(climatic_data)

# 2.6.8. ------------------------------------------------------------------

full_join_test <- temperature |>
  full_join(snow, by = c("Y", "M"))

summary(full_join_test)



# 2.6.9. ------------------------------------------------------------------
snow <- read_csv2('input/dplyr_join/23776__snow.csv') |> 
  select(Y = 'Год',
         Snow_depth = Высота_снежного_покрова) |> 
  group_by(Y) |> 
  summarise(Sn_depth = mean(Snow_depth, na.rm = T))


temp <-  read_excel('input/dplyr_join/23776_TTTR.xlsx') |> 
  select(Y = "год", M = "месяц", D = "день", Tavg = "Тср", Pr = "осадки") |>
  group_by(Y) |>
  summarise(
    Tavg = mean(Tavg, na.rm = T),
    Pr_avg = mean(Pr, na.rm = T)
  )

total <- snow |> 
  full_join(temp, by = 'Y') |> 
  filter(Tavg < 0 & Sn_depth>5 & Pr_avg <15) |> 
  slice_max(Tavg, n = 5) |> 
  mutate(Sum = Tavg + Sn_depth + Pr_avg) |> 
  summarise(round(median(Sum)))
