# call libraries ----------------------------------------------------------
library(tidyverse)
library(janitor)
library(tsibble)



# read data ---------------------------------------------------------------
airlines <- clean_names(read_csv(here::here("raw_data/airlines.csv")))
airports <- clean_names(read_csv(here::here("raw_data/airports.csv")))
flights  <- clean_names(read_csv(here::here("raw_data/flights.csv")))
planes   <- clean_names(read_csv(here::here("raw_data/planes.csv")))
weather  <- clean_names(read_csv(here::here("raw_data/weather.csv")))


# explore data (summary, glimpse) --------------------------------------------
glimpse(flights)
summary(flights)

glimpse(airports)
summary(airports)

glimpse(airlines)
summary(airlines)

glimpse(planes)
summary(planes)

glimpse(weather)
summary(weather)


# explore NA from each dataset individually ---------------------------------
flights %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

weather %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

airlines %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

planes %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

airports %>% 
  summarise(across(.fns = ~sum(is.na(.x))))


# join datasets ------------------------------------------------------------

## create a single key to join flights  and weather ------------------------
flights_key <- flights %>% 
  mutate(key = str_c(time_hour, origin, sep = ""))

weather_key <- weather %>% 
  mutate(key = str_c(time_hour, origin, sep = "")) 


## join --------------------------------------------------------------------
flights_joined <- 
  left_join(x = flights_key,
            y = weather_key,
            suffix = c("", "_weather"),
            by = "key") %>%
  left_join(x = .,
            y = airlines,
            suffix = c("", "_airline"),
            by = "carrier") %>%
  left_join(x = .,
            y =  airports,
            suffix = c("", "_airport"),
            by = c("origin" = "faa")) %>%
  left_join(x = .,
            y = planes,
            suffix = c("", "_plane"),
            by = "tailnum")



## remove datasets  ---------------------------------------------------------
# Remove datasets created with a unique key to join flighs and weather
rm(flights_key, weather_key)


# Explore data joined  ------------------------------------------------------
na_summary <- tibble(
  flights_joined %>% 
    summarise(across(.fns = ~sum(is.na(.x))))
)



# some clean/organize dataset ----------------------------------------------
flights_joined <- flights_joined %>% 
  select(-ends_with("_weather"), -year_plane, - key, - year, -month, - day) %>% 
  relocate(time_hour, .before = 1) %>% 
  rename(name_airline = name) %>% 
  filter(tzone == "America/New_York")


# write csv file --------------------------------------------------------
flights_joined %>%
  write.csv(here::here("clean_data/flights_joined.csv"))


