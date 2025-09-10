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




