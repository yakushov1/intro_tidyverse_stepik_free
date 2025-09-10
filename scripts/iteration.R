library(tidyverse)
library(palmerpenguins)


# 4.2.6. -----------------------------------------------------------------------

data <- penguins |> 
  mutate(across(c(bill_length_mm:flipper_length_mm), list(
    m = \(x) x/1000
    )),
    body_mass_kg = body_mass_g / 1000) |> 
  rename_with(~(gsub("_mm_m", "_m", .x)))


# 4.2.7.  -----------------------------------------------------------------------
data |> 
  group_by(species) |> 
  summarise(across(ends_with("_m"), \(x) mean(x, na.rm = T)))



# 4.2.8. -----------------------------------------------------------------------
data |> 
  group_by(species, island) |> 
  summarise(across(ends_with("_m"), list(
    avg = \(x) mean(x, na.rm = T),
    max = \(x) max(x, na.rm = T),
    median = \(x) median(x, na.rm = T))))


# 4.2.9. -----------------------------------------------------------------------
data |> 
  filter(is.na(sex)==F) |> 
  group_by(species, sex) |> 
  summarize(across(ends_with("_m"), min)) |> 
  arrange(desc(bill_depth_m))



# 4.2.10. -----------------------------------------------------------------------
# Chinstrap


