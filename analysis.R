# Load incarceration data and tidyverse
library(tidyverse)
library(readr)
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Summary Information -------------------------------------------------------

# 1. Which year had the highest total jail population?
highest_pop_year <- data %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(year)

# 2. In 1993, which state had the highest total jail population?
highest_pop_state_1993 <- data %>%
  filter(year == "1993") %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(state)

# 3. Most recently, which state had the highest total jail population?
highest_pop_state <- data %>%
  filter(year == max(year)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(state)

# 4. Most recently, which state had the highest total jail population of black people?
highest_black_pop_state <- data %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(state)

# 5. Which county in CA had the highest total jail population of black people?
highest_black_pop_county <- data %>%
  filter(state == "CA") %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

# Trends Over Time Chart ----------------------------------------------------

pop_by_race <- data %>%
  group_by(year) %>%
  filter(year >= 2015 && year <= 2018) %>%
  filter(state == "CA") %>%
  filter(county_name == "Los Angeles County") %>%
  summarise(Year = year, Black_population = black_jail_pop_rate, White_population = white_jail_pop_rate)

race_trend_chart <- pop_by_race %>%
  pivot_longer(cols = c("Black_population", "White_population"), 
               names_to = "Race", values_to = "value") %>%
  ggplot() + geom_line(aes(x = year, y = value, group = Race, color = Race)) +
  labs(x = "Years", y = "Rate of Jail Population", title = "Jail Population of white vs. black in
       Los Angeles County (2015 ~ 2018)")

# Variable Comparison Chart -------------------------------------------------

filtered_data <- data %>%
  filter(year == 2018) %>%
  filter(state == "CA") %>%
  filter(county_name == "Los Angeles County") %>%
  summarise(Year = year, Black_population = black_jail_pop, White_population = white_jail_pop, 
            Total_population = total_jail_pop)

race_comparison_chart <- filtered_data %>%
  pivot_longer(cols = c("Black_population", "White_population", "Total_population"), 
               names_to = "Race", values_to = "value") %>%
  ggplot(mapping = aes(x = Race, y = value, fill = Race)) + geom_col() + geom_text(aes(label = value), vjust = -0.6) + 
  labs(x = "Population groups",
       y = "Jail Population", 
       title = "Comparison of Jail Population between Races, 2018 (Total population includes all race groups)",
       fill = "Population",
       theme(axis.text.x = element_blank()))
  
# Map -----------------------------------------------------------------------

library(usmap)
library(ggplot2)

blank_theme <- theme_bw() + 
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

black_pop_data <- data %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarise(black_pop = sum(black_jail_pop, na.rm = TRUE))

black_pop_data_map <- plot_usmap(
  data = black_pop_data, values = "black_pop", color = "red") + blank_theme + 
  scale_fill_gradient(low = "#132B43",
                      high = "#56B1F7",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  labs(title = "Jail Population of Black People in the United States, 2018", 
       fill = "Population")

