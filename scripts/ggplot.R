library(tidyverse)


# 3.1.6. ------------------------------------------------------------------

pressure <- pressure
ggplot(pressure, aes(temperature, pressure))+
  geom_point()+
  labs(x = "Температура, \u00B0C",
       y = "Давление, мм. рт. ст.",
       title = "Зависимость давления паров ртути от температуры")


# 3.1.7. ------------------------------------------------------------------


ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species))+
  geom_point()



# 3.1.8. ------------------------------------------------------------------
df <- women |> 
  mutate(height_cm = height * 2.54,
         weight_kg = weight *  0.4536)

ggplot(df, aes(weight_kg, height_cm))+
  geom_point()+
  labs(x = 'Вес, кг',
       y = 'Рост, см',
       title = 'Вес женщин 30-39 лет') +
  theme_bw()

# 3.2.8. ------------------------------------------------------------------
iris |>
  pivot_longer(cols = !Species, names_to= "Parametr", values_to = "Val") |>
  ggplot(aes(Parametr, Val, col = Species))+
  geom_boxplot()+
  theme_minimal()+
  labs(x = NULL,
   y = "Линейные размеры, мм",
   col = "Вид")


# 3.2.9. ------------------------------------------------------------------
iris |>
  pivot_longer(cols = !Species, names_to = "Parametr", values_to = "Val") |>
  mutate(Parametr = case_when(Parametr == "Petal.Length" ~ "Длина лепестка",
               Parametr == "Petal.Width" ~ "Ширина лепестка",
               Parametr == "Sepal.Length" ~ "Длина чашелистика",
               Parametr == "Sepal.Width" ~ "Ширина чашелистика")) |> 
  ggplot(aes(Parametr, Val, col = Species))+
  geom_boxplot()+
  theme_minimal()+
  labs(x = NULL,
       y = "Линейные размеры, мм",
       col = "Вид")



# 3.2.10 ------------------------------------------------------------------


iris |>
  select(Species, Petal.Length, Sepal.Length) |>
  pivot_longer(cols = !Species, names_to = "Parametr", values_to = "Val") |> 
  mutate(Parametr = case_when(Parametr == "Petal.Length" ~ "Длина лепестка",
                            Parametr == "Sepal.Length" ~ "Длина чашелистика")) |>
  mutate(Parametr = factor(Parametr, levels = c("Длина лепестка", "Длина чашелистика"))) |>
  ggplot(aes(Parametr, Val, col = Species))+
  geom_boxplot()+
  theme_minimal()+
  labs(x = NULL,
       y = "Линейные размеры, мм",
       col = "Вид")



# 3.2.11 ------------------------------------------------------------------

beaver1 |> 
  filter(day == 346) |> 
  ggplot(aes(time, temp))+
  geom_line()+
  geom_smooth(method = 'lm', se = F)+
  theme_minimal()


# 3.2.12 ------------------------------------------------------------------

library(palmerpenguins)                                      

penguins |> 
  group_by(species, year) |> 
  summarise(bill_length_mm = mean(bill_length_mm)) |> 
  ggplot(aes(year,bill_length_mm, col = species))+
  geom_line()



# 3.3.4. ------------------------------------------------------------------

ggplot(penguins, aes(species, body_mass_g))+
  geom_boxplot()+
  facet_wrap(~sex)+
  theme_minimal()


# 3.3.5. ------------------------------------------------------------------
ggplot(penguins, aes(bill_depth_mm, bill_length_mm, col = species))+
  geom_point()+
  facet_wrap(~island)+
  theme_minimal()


# 3.3.6. ------------------------------------------------------------------
ggplot(penguins, aes(bill_depth_mm, bill_length_mm, col = species))+
  geom_point()+
  geom_smooth(method='lm', se = F)+
  facet_wrap(~island)+
  theme_minimal()



# 3.3.7. ------------------------------------------------------------------
ggplot(penguins, aes(bill_depth_mm, bill_length_mm))+
  geom_point(aes(col = species))+
  geom_smooth()+
  facet_wrap(~island)+
  theme_minimal()



# 3.3.8. ------------------------------------------------------------------

ggplot(penguins, aes(bill_depth_mm, bill_length_mm))+
  geom_point(aes(col = species))+
  geom_smooth()+
  facet_wrap(~island, ncol = 1)+
  theme_minimal()



# 3.3.9 -------------------------------------------------------------------
ggplot(penguins, aes(bill_depth_mm, bill_length_mm, fill = species))+
  geom_dotplot()+ #Новый вид geom!
  facet_grid(sex~island)+
  theme_minimal()


# 3.3.10. -----------------------------------------------------------------

penguins |> 
  filter(is.na(sex)==F) |> 
  ggplot(aes(body_mass_g, bill_length_mm, col = island))+
  geom_point()+
  facet_grid(sex~species)+
  theme_bw()


# 3.4.2. ------------------------------------------------------------------

df <-  read_delim("input/02_example.txt", 
           col_names = c("Station", "Year", "Month", 
                         "Day", "Tmin", "Tavg", "Tmax", "Pr")) |> 
  group_by(Station, Year) |> 
  summarise(Tavg = mean(Tavg, na.rm = T),
            Tmin = mean(Tmin, na.rm = T)) |> 
  pivot_longer(cols = !c(Station,Year),
               names_to = 'Parametr',
               values_to = 'Val')

ggplot(df, aes(Year, Val, col = Parametr))+
  geom_line()+
  geom_point(aes(shape = Parametr))+
  facet_wrap(~Station, scales = "free_y")+
  theme_bw()

# 3.4.3. ------------------------------------------------------------------
penguins |> 
  mutate(extr_body_mass = case_when(body_mass_g<quantile(body_mass_g, na.rm=T)[2] ~ body_mass_g)) |> 
  ggplot(aes(body_mass_g, bill_length_mm))+
  geom_point()+
  geom_point(aes(extr_body_mass), size = 3, col = "red")+
  theme_minimal()


# 3.4.8. ------------------------------------------------------------------
library(palmerpenguins)
library(tidyverse)
library(ggthemes)

ggplot(penguins, aes(body_mass_g, bill_length_mm, col = sex))+
  geom_point()+
  facet_grid(island~species) +
  coord_cartesian(xlim = c(NA, 4500))+
  theme_fivethirtyeight(base_size = 15)


# 3.5.4. ------------------------------------------------------------------

mass_vs_bill_len <- ggplot(penguins, aes(body_mass_g, bill_length_mm, col = species))+
  geom_point()+
  theme_minimal()+
  scale_color_manual(values  = c("#DC143C", "#20B2AA", "#778899"))+
  labs(x = 'Масса тела, гр',
       y = 'Длина клюва, мм')

# 3.5.5. ------------------------------------------------------------------

flipper_len_boxplot <- ggplot(penguins, aes(island, flipper_length_mm))+
  geom_boxplot(fill="red", alpha = 0.3, col = "blue")+
  labs(x = "Остров",
       y = "Длина ласт, мм")+
  theme_minimal()


# 3.5.6. ------------------------------------------------------------------
body_mass_vs_bill_len_facet <- ggplot(penguins, aes(body_mass_g, bill_length_mm, col = sex))+
  geom_point()+
  facet_grid(species~island)+
  scale_color_manual(values = c("#DC143C", "#20B2AA"))+
  labs(x = "Масса тела, гр",
       y = "Длина клюва, мм",
       col = "Пол") 


# 3.5.7. ------------------------------------------------------------------

library(patchwork)
(mass_vs_bill_len + flipper_len_boxplot )/ body_mass_vs_bill_len_facet


# 3.5.8. ------------------------------------------------------------------
mass_vs_bill_len + (flipper_len_boxplot / body_mass_vs_bill_len_facet)
  

