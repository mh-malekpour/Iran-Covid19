# Loading

library(tidyverse)

confirmed <- as_tibble(read_csv("time_series_covid19_confirmed_global.csv"))
recoverd <- as_tibble(read_csv("time_series_covid19_recovered_global.csv"))
deaths <- as_tibble(read_csv("time_series_covid19_deaths_global.csv"))

confirmed_ir =  filter(confirmed, `Country/Region` == "Iran")
recoverd_ir =  filter(recoverd, `Country/Region` == "Iran") 
deaths_ir =  filter(deaths, `Country/Region` == "Iran")


# Wrangling

confirmed_ir %>% 
  gather(key = "date", value = "count", `1/22/20`:`9/24/20`) %>% 
  mutate(lable = "confirmed",
         count = c(count[1] , diff(count)),
         date=as.Date(date, format = "%m/%d/%y")) %>% 
  select(date, lable, count) -> confirmed_ir

recoverd_ir %>% 
  gather(key = "date", value = "count", `1/22/20`:`9/24/20`) %>%
  mutate(lable = "recoverd" ,
         count = c(count[1] , diff(count)),
         date=as.Date(date, format = "%m/%d/%y")) %>% 
  select(date, lable, count) -> recoverd_ir

deaths_ir %>% 
  gather(key = "date", value = "count", `1/22/20`:`9/24/20`) %>%
  mutate(lable = "death",
         count = c(count[1] , diff(count)),
         date=as.Date(date, format = "%m/%d/%y")) %>% 
  select(date, lable, count) -> deaths_ir


covid_ir <- bind_rows(confirmed_ir, recoverd_ir, deaths_ir) %>% 
  mutate(date=as.Date(date, format = "%m/%d/%y"))


tmp <-bind_rows(confirmed_ir, recoverd_ir) %>% 
  mutate(date=as.Date(date, format = "%m/%d/%y")) %>% 
  separate(date, sep = "-",
         into = c("year", "month", "day")) %>% 
  group_by(lable, month) %>% 
  summarize(count = sum(count))

sum(confirmed_ir$count)
sum(deaths_ir$count)
sum(recoverd_ir$count)


# Visualization

library(scales)
library(patchwork)
library(ggpmisc)

p1 <- ggplot(data = confirmed_ir, aes(x = date, y = count)) + 
  geom_line(color = "#00AFBB", size = 1) +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("1 month")) +
  stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") +
  labs(
    x = 'Date',
    y = 'Count',
    title = 'Daily Confirmed Cases',
    subtitle = 'total cases: 436319') 


p2 <- ggplot(data = recoverd_ir, aes(x = date, y = count)) + 
  geom_line(color = "#00AFBB", size = 1) +
  stat_smooth( color = "#FC4E07", fill = "#FC4E07", method = "loess") +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("1 month")) +
  labs(
    x = 'Date',
    y = 'Count',
    title = 'Daily Recoverd Cases',
    subtitle = 'total recoverd: 367829')


p3 <- ggplot(data = deaths_ir, aes(x = date, y = count)) + 
  geom_line(color = "#00AFBB", size = 1) +
  stat_smooth(color = "#FC4E07", fill = "#FC4E07", method = "loess") +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("1 month")) +
  scale_y_continuous(breaks = seq(0, 300, by = 50)) +
  labs(
    x = 'Date',
    y = 'Count',
    title = 'Daily Deaths Cases',
    subtitle = 'total deaths: 25015') 


# Multiple line plot
p4 <- ggplot(covid_ir, aes(x = date, y = count)) + 
  geom_line(aes(color = lable), size = 1) +
  scale_color_manual(values = c("#00AFBB",  "#bf1313", "#E7B800")) +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("1 month")) +
  scale_y_continuous(breaks = seq(0, 7000, by = 1000)) +
  theme_minimal() +
  labs(
    x = 'Date',
    y = 'Count',
    title = 'IR COVID-19',
    subtitle = 'Last Updated at 9/24/2020',
    caption = 'Mohammad Hossein Malekpour',
    color = 'Case')


(p1 | p2 | p3) /
p4
