library(tidyverse)
library(janitor) #для очистки имен
library(patchwork)



# 3.7.2. ------------------------------------------------------------------
# Смертность от алкогольных зависимостей
alcohol_total <- read_csv("input/dplyr_practice/WHOMortalityDatabase_DeathsAlcohol.csv",
                          skip = 5) |> # пропустим строки с аннотацией
  clean_names() |>  # заменит пробелы в названиях нижним подчеркиванием. Дать ссылку на другие случаиhttps://rpubs.com/jenrichmond/clean_names
  select(region_name, year, sex, alc_per_100t = death_rate_per_100_000_population) |> 
  filter(!(sex %in% c("Unknown", "All"))) |> 
  mutate(alc_per_100t = str_replace_all(alc_per_100t, ",", "")) |> 
  mutate(alc_per_100t = as.numeric(alc_per_100t)) |> 
  group_by(region_name, year, sex) |> 
  summarise(alc_per_100t = mean(alc_per_100t, na.rm = T)) |> 
  mutate(region_name = case_when(
    region_name == "Africa" ~ "Африка",
    region_name == "Asia" ~ "Азия",
    region_name == "Central and South America" ~ "Центральная и Южная Америка",
    region_name == "Europe" ~ "Европа",
    region_name == "North America and the Caribbean" ~ "Северная Америка и Карибы",
    region_name == "Oceania" ~ "Океания"),
        sex = case_when(sex == "Female" ~ "Ж",
                        sex == "Male" ~ "М"))


# 3.7.3. ------------------------------------------------------------------


alcohol_graph <- ggplot(alcohol_total, aes(year, alc_per_100t,  fill = sex))+
  geom_col()+
  facet_wrap(~region_name, ncol = 3)+
  theme_minimal()+
  scale_fill_manual(values = c("#CD5C5C", "#66CDAA"))+
  labs(title = "Смерность от алкогольной зависимости в разных регионах мира",
       x = NULL,
       y = NULL,
       fill = "Пол")+
  theme(legend.position = "bottom")



# 3.7.4. ------------------------------------------------------------------
# Смертность от сердечно-сосудистых заболеваний 
cardiovascular <- read_csv("input/dplyr_practice/WHOMortalityDatabase_DeathsCardiovascular_diseases.csv",
                           skip = 5) |> 
  clean_names() |>  # заменит пробелы в названиях нижним подчеркиванием. Дать ссылку на другие случаиhttps://rpubs.com/jenrichmond/clean_names
  select(region_name, year, sex, cv_per_100t = death_rate_per_100_000_population) |> 
  filter(!(sex %in% c("Unknown", "All"))) |> 
  mutate(cv_per_100t = str_replace_all(cv_per_100t, ",", "")) |> 
  mutate(cv_per_100t = as.numeric(cv_per_100t)) |> 
  group_by(region_name, year, sex) |> 
  summarise(cv_per_100t = mean(cv_per_100t, na.rm = T)) |> 
  mutate(region_name = case_when(
    region_name == "Africa" ~ "Африка",
    region_name == "Asia" ~ "Азия",
    region_name == "Central and South America" ~ "Центральная и Южная Америка",
    region_name == "Europe" ~ "Европа",
    region_name == "North America and the Caribbean" ~ "Северная Америка и Карибы",
    region_name == "Oceania" ~ "Океания"),
    sex = case_when(sex == "Female" ~ "Ж",
                    sex == "Male" ~ "М"))



# 3.7.5. ------------------------------------------------------------------


cardiovascular_graph <- ggplot(cardiovascular, aes(year, cv_per_100t,  fill = sex))+
  geom_col()+
  facet_wrap(~region_name, ncol = 3)+
  theme_minimal()+
  scale_fill_manual(values = c("#CD5C5C", "#66CDAA"))+
  labs(title = "Смертность от сердечно-сосудистых заболеваний в разных регионах мира",
       x = "Год",
       y = NULL,
       fill = "Пол")+
  theme(legend.position = "bottom")




# 3.7.6 -------------------------------------------------------------------
# Смертность от ДТП 

road_acc <- read_csv("input/dplyr_practice/WHOMortalityDatabase_Deaths_Road_traffic_accidents.csv",
                     skip = 5) |> 
  clean_names() |> 
  select(region_name, year, sex, racc_per_100t = death_rate_per_100_000_population) |> 
  filter(!(sex %in% c("Unknown", "All"))) |> 
  mutate(racc_per_100t = str_replace_all(racc_per_100t, ",", "")) |> 
  mutate(racc_per_100t = as.numeric(racc_per_100t)) |> 
  group_by(region_name, year, sex) |> 
  summarise(racc_per_100t = mean(racc_per_100t, na.rm = T)) |> 
  mutate(region_name = case_when(
    region_name == "Africa" ~ "Африка",
    region_name == "Asia" ~ "Азия",
    region_name == "Central and South America" ~ "Центральная и Южная Америка",
    region_name == "Europe" ~ "Европа",
    region_name == "North America and the Caribbean" ~ "Северная Америка и Карибы",
    region_name == "Oceania" ~ "Океания"),
    sex = case_when(sex == "Female" ~ "Ж",
                    sex == "Male" ~ "М"))



road_acc_graph <- ggplot(road_acc, aes(year, racc_per_100t,  fill = sex))+
  geom_col()+
  facet_wrap(~region_name, ncol = 3)+
  theme_minimal()+
  scale_fill_manual(values = c("#CD5C5C", "#66CDAA"))+
  labs(title = "Смертность от ДТП в разных регионах мира",
       x = NULL,
       y = "Количество смертей на 100 000 населения",
       fill = "Пол")+
  theme(legend.position = "bottom")

# 3.7.7. ------------------------------------------------------------------

alcohol_graph/road_acc_graph/cardiovascular_graph

