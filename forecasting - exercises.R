
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

print_retail