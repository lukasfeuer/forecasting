
# Forecasting Practice 2 --------------------------------------------------

library(tidyverse)
library(fpp3)


prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison %>% 
  mutate(
    Quarter = yearquarter(Date), 
    Date = NULL
  ) %>% 
  tsibble(key = c(State, Gender, Legal, Indigenous)
             , index = Quarter)

aus_arrivals %>% 
  group_by(Origin) %>% 
  #autoplot() +
  #gg_season()
  gg_subseries()
  facet_wrap(vars(Origin), ncol = 1)

aus_livestock %>% 
  filter(Animal == "Pigs",
         #year(Month) >= 1990, year(Month) <= 1995,
         State == "Victoria") %>% 
  #ACF() %>% 
  #gg_subseries(Count)
  #gg_season(Count)
  autoplot()

dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

dgoog %>% 
  select(diff) %>% 
  ACF() %>% 
  autoplot()

lambda <- us_employment %>% 
  filter(Title == "Mining and Logging: Logging") %>% 
  features(features = guerrero) %>% 
  pull(lambda_guerrero)

us_employment %>% 
  filter(Title == "Mining and Logging: Logging") %>% 
  autoplot(box_cox(Employed, lambda = -2))

us_employment %>% 
  filter(Title == "Mining and Logging: Logging",
         !is.na(Employed)) %>% 
  model(
    stl = STL(box_cox(Employed, lambda = -1))
  ) %>% 
  components() %>% 
  autoplot()


global_economy %>%
  mutate(GDP_per_Capita = GDP / Population) %>% 
  group_by(Country) %>% 
  autoplot(GDP_per_Capita) +
  theme(legend.position = "none") 
plotly::ggplotly()

canadian_gas %>% 
  autoplot()

canadian_gas %>% 
  features(features = guerrero, Volume) %>% 
  pull(lambda_guerrero) -> x

canadian_gas %>% 
  model(
    stl = STL(box_cox(Volume, lambda = x))
  ) %>% 
  components() %>% 
  autoplot()

canadian_gas %>% 
  mutate(Volume2 = box_cox(Volume, lambda = x)) %>% 
  pivot_longer(-1, "var", "val") %>% 
  ggplot(aes(Month, value)) +
  geom_line(aes(color = var))

aus_production %>% 
  features(.var = Tobacco, features = guerrero) %>% 
  pull(lambda_guerrero) -> x
aus_production %>% 
  mutate(Tobacco2 = box_cox(Tobacco, lambda = x)) %>% 
  pivot_longer(-1, "var", "val") %>% 
  filter(str_detect(var, "Tobacco")) %>% 
  ggplot(aes(Quarter, value)) +
  geom_line(aes(color = var))

gas <- tail(aus_production, 5*4) %>% 
  select(Gas)
gas %>% 
  gg_subseries()
  gg_season()
  autoplot()
gas[20, "Gas"] <- 500
gas %>% 
  model(
    classical_decomposition(Gas, "multiplicative") 
  ) %>% 
  components() %>% 
  # select(Quarter, Gas, season_adjust) %>% 
  # pivot_longer(-1, "var", "value") %>% 
  autoplot()
