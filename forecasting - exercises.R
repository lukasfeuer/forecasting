
# Notes -------------------------------------------------------------------

library(fpp3)

y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)
y


autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)") + 
  theme_minimal()

plotly::ggplotly()


# 2 Exercises -------------------------------------------------------------

aus_arrivals
aus_arrivals %>% 
  autoplot() + facet_grid(vars(Origin), scales = "free_y")
aus_arrivals %>% 
  gg_season()
aus_arrivals %>% 
  gg_subseries()

set.seed(36156)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
myseries %>% 
  autoplot(Turnover)
myseries %>% 
  gg_season(Turnover)
myseries %>% 
  gg_subseries(Turnover)
myseries %>% 
  gg_lag(Turnover)
myseries %>% 
  ACF(Turnover) %>% 
  autoplot()

us_gasoline %>% 
  autoplot()
us_gasoline %>% 
  gg_season()
us_gasoline %>% 
  gg_subseries()
us_gasoline %>% 
  gg_lag()
us_gasoline %>% 
  ACF() %>% 
  autoplot()

d <- aus_livestock %>% 
  filter(Animal == "Pigs" & 
           #year(Month) >= 1990 & 
           #year(Month) <= 1995 &
           State == "Victoria") 
d %>% autoplot(Count)
d %>% gg_season()
d %>% gg_subseries()
d %>% gg_lag()
d %>% ACF() %>% autoplot()


# TODO: Why was it necessary to re-index the tsibble???
dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

dgoog %>% autoplot(diff)
dgoog %>% ACF(diff) %>% autoplot() # white noise


# Chapter 3
global_economy %>%
  filter(Country == "Australia") %>%
  select(Year, GDP, Population) %>% 
  autoplot(GDP/Population) +
  labs(title= "GDP per capita", y = "$US")


print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adjusted_turnover = Turnover / CPI * 100) %>%
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") %>%
  mutate(name = factor(name,
                       levels=c("Turnover","Adjusted_turnover"))) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)
dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp) %>% 
  autoplot()


# notice: for monthly data only 2x12 (or multiples of 12) result in a smoth trend-cycle estimate
us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 10, .after = 11, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )
us_retail_employment_ma %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

global_economy %>%
  filter(Country == "Australia") %>%
  select(Exports) %>% 
  mutate(
    `3-MA` = slider::slide_dbl(Exports, mean,
                               .before = 1, .after = 1, .complete = TRUE),
    `3x5-MA` = slider::slide_dbl(`3-MA`, mean,
                                 .before = 2, .after = 2, complete = TRUE),
    test = slider::slide_mean(`3-MA`, before = 2, after = 2, complete = TRUE)
  ) 

lambda <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing",
         State == "Australian Capital Territory") %>% 
  features(Turnover, guerrero) %>% 
  pull(lambda_guerrero)

aus_retail %>%
  filter(Industry == "Newspaper and book retailing",
         State == "Australian Capital Territory") %>% 
  autoplot(box_cox(Turnover, lambda))

gas <- tail(aus_production, 5*4) %>% select(Gas)
gas %>% 
  autoplot()

decomp <- gas %>% 
  model(
    c_dec = classical_decomposition(Gas, "multiplicative")
  ) %>% 
  components()
gas %>% 
  autoplot(Gas) + 
  geom_line(data = decomp, aes(Quarter, season_adjust), color = "blue")

canadian_gas %>% 
  autoplot()
canadian_gas %>% 
  gg_subseries()
canadian_gas %>% 
  gg_season()
canadian_gas %>% 
  model(
    stl = STL(Volume ~ trend(window = 13) + 
                       season(window = 9), robust = T) # how to choose a window? --> review 3.3
  ) %>% 
  components() %>% 
  autoplot()
# Awesome visual of Season component ----
canadian_gas %>% 
  model(
    stl = STL(Volume ~ trend() + 
                season(window = 19), robust = T) # how to choose a window?
  ) %>% 
  components() %>% 
  gg_season(season_year) # make this 3D interactive


PBS
mean_sd <- function(x) {
  out <- list()
  out$mean_x <- mean(x, na.rm = T)
  out$sd_x <- sd(x, na.rm = T)
  return(out)
}

PBS %>% 
  tibble() %>% 
  group_by(Concession, Type, ATC1, ATC2) %>% 
  summarise(mean_sd(Cost)) %>% 
  map(2)

PBS %>%
  tibble %>% 
  mutate(group = str_c(Concession, Type, ATC1, ATC2, sep = "_")) %>% 
  pivot_wider(names_from = group,
              values_from = c(Scripts))
  map(mean_sd)