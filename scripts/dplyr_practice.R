library(tidyverse)
library(janitor)


# 2.7.2. ------------------------------------------------------------------
# Смертность от алкогольных зависимостей
alcohol_total <- read_csv("input/dplyr_practice/WHOMortalityDatabase_DeathsAlcohol.csv",
                      skip = 5) |> # пропустим строки с аннотацией
  clean_names() # заменит пробелы в названиях нижним подчеркиванием.

# другие примеры использования clean_names() https://rpubs.com/jenrichmond/clean_names

# проверить временные границы исследований
range(alcohol_total$year)


# 2.7.3. ------------------------------------------------------------------
# Найти топ стран, в которых за все время исследований
# от алкогольных зависимостей умерло больше всего мужчин (без учета возрастных групп)
alcohol_total |> 
  filter(sex == "Male" & age_group == "[All]") |> 
  group_by(country_code) |> 
  summarise(number = sum(number, na.rm = T)) |> 
  arrange(desc(number))


# 2.7.4 -------------------------------------------------------------------
#то же самое, но для женщин
alcohol_total |> 
  filter(sex == "Female" & age_group == "[All]") |>
  group_by(country_code) |> 
  summarise(number = sum(number, na.rm = T)) |> 
  arrange(desc(number))


# 2.7.5. ------------------------------------------------------------------
# Смертность от сердечно-сосудистых заболеваний


cardiovascular <- read_csv("input/dplyr_practice/WHOMortalityDatabase_DeathsCardiovascular_diseases.csv",
                           skip = 5) |> 
                  clean_names()
# проверить временные границы исследований
range(cardiovascular$year)


# 2.7.6. ------------------------------------------------------------------
# Найти топ стран, в которых за все время исследований
# от сердечно-сосудистых заболеваний умерло меньше всего мужчин
cardiovascular |> 
  filter(sex == "Male" & age_group == "[All]") |> 
  group_by(country_code) |> 
  summarise(number = sum(number, na.rm = T)) |> 
  arrange(number)



# 2.7.7. ------------------------------------------------------------------
#топ стран, в которых умерло больше всего женщин в возрасте от 25 до 39 лет
cardiovascular |> 
  filter(sex == "Female" & age_group %in%c("[25-29]", "[30-34]", "[35-39]" )) |> 
  group_by(country_code) |> 
  summarise(number = sum(number, na.rm = T)) |> 
  arrange(desc(number))



# 2.7.8. ------------------------------------------------------------------
# смертность в  ДТП 

road_acc <- read_csv("input/dplyr_practice/WHOMortalityDatabase_Deaths_Road_traffic_accidents.csv",
                     skip = 5) |> 
                       clean_names()
# Найти топ европейских стран, в которых с 2000 по 2020 годы
# от дтп  умерло больше всего мужчин в возрасте 20-29 лет
road_acc |> 
  filter(region_code == "EU" & sex == "Male" & year %in%c(2000:2020) & age_group %in%c("[20-24]", "[25-29]")) |> 
  group_by(country_code) |> 
  summarise(number = sum(number, na.rm = T)) |> 
  arrange(desc(number))





# 2.7.9. ------------------------------------------------------------------

alcohol_selected <- alcohol_total |> 
  select(country_code,
         year,
         sex,
         age_group,
         alc_per_100t = death_rate_per_100_000_population)

cardio_vascular_selected <- cardiovascular |> 
  select(country_code,
         year,
         sex,
         age_group,
         card_per_100t = death_rate_per_100_000_population)

road_acc_selected <- road_acc |> 
  select(country_code,
         year,
         sex,
         age_group,
         road_per_100_t = death_rate_per_100_000_population)


total <- alcohol_selected |> 
  full_join(cardio_vascular_selected, by = c("country_code", "year", "sex", "age_group")) |> 
  full_join(road_acc_selected, by = c("country_code", "year", "sex", "age_group")) |> 
  mutate(alc_per_100t = as.numeric(str_replace_all(alc_per_100t, ",", "")),
         card_per_100t = as.numeric(str_replace_all(card_per_100t, ",", "")),
         road_per_100_t = as.numeric(str_replace_all(road_per_100_t, ",", "")))




# 2.7.10 ------------------------------------------------------------------

# Среднее количество смертей на 100 000 человек с 2000 по 2010 год
# без учета половозрастных различий
average_death_per_100_2000_2010 <- total |> 
  filter(sex == "All" & age_group == "[All]" & year %in% c(2000:2010)) |> 
  group_by(country_code) |> 
  summarise(alc_per_100t = mean(alc_per_100t, na.rm = T),
            card_per_100t = mean(card_per_100t, na.rm = T),
            road_per_100_t = mean(road_per_100_t, na.rm = T))


# максимальная смертность от алкогольной зависимости
average_death_per_100_2000_2010 |> 
  slice_max(alc_per_100t)

#  от кардиологических заболеваний
average_death_per_100_2000_2010 |> 
  slice_max(card_per_100t)

# от ДТП
average_death_per_100_2000_2010 |> 
  slice_max(road_per_100_t)



# 2.7.11. -----------------------------------------------------------------
# Минимальное количество смертей
# от алкогольной зависимости
average_death_per_100_2000_2010 |> 
  slice_min(alc_per_100t)

# от кардиологических заболеваний
average_death_per_100_2000_2010 |> 
  slice_min(card_per_100t)

# от ДТП
average_death_per_100_2000_2010 |> 
  slice_min(road_per_100_t)

# и найдем полные названия этих стран
alcohol_total |> 
  filter(country_code == "MDV")

alcohol_total |> 
  filter(country_code == "QAT")
