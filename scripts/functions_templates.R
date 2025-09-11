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



ggplot(log_total, aes(Year, Tavg))+
  geom_line()+
  facet_grid(id~Month)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60))

log_total |> 
  filter(id == 23274) |> 
  ggplot(aes(Year, Tavg))+
  geom_line()+
  geom_smooth()+
  facet_wrap(~Month)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60))


graph <- function(df, id){
  df |> 
    filter(id == {id}) |> 
    ggplot(aes(Year, Tavg))+
      geom_point()+
      facet_wrap(~Month)+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 60))
  }




logg_id <- levels(log_total$id) 

map(logg_id, graph(log_total, logg_id[1]))


