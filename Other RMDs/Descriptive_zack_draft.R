anes_cleaned_0928->anes

library(tidyverse)

anes_plot <- anes %>%
  mutate(
    V162079 = ifelse(V162079 >= 1 & V162079 <= 100, V162079, NA),
    V202144 = ifelse(V202144 >= 1 & V202144 <= 100, V202144, NA),
    V242126 = ifelse(V242126 >= 1 & V242126 <= 100, V242126, NA)
  ) %>%
  select(V162079, V202144, V242126) %>%
  pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = recode(
      year,
      "V162079" = "2016",
      "V202144" = "2020",
      "V242126" = "2024"
    )
  )

ggplot(anes_plot, aes(x = year, y = value)) +
  geom_boxplot() +
  scale_y_continuous(
    limits = c(1, 100),
    breaks = seq(0, 100, 10)
  ) +
  labs(
    x = "Year",
    y = "Score"
  ) +
  theme_minimal()

anes_freq <- anes_plot %>%
  filter(!is.na(value)) %>%
  count(year, value)

ggplot(anes_freq, aes(
  x = value,
  y = year,
  size = n,
  color = year
)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100)
  ) +
  scale_size_continuous(
    name = "Frequency",
    range = c(1, 12)
  ) +
  labs(
    x = "Feeling Thermometer Score",
    y = "Year",
    color = "Year",
    title = "Feeling Thermometer Scores by Year"
  ) +
  theme_minimal()


anes_freq_20 <- anes_plot %>%
  filter(!is.na(value)) %>%
  mutate(
    therm_group = cut(
      value,
      breaks = c(0, 20, 40, 60, 80, 100),
      labels = c("1-20", "21-40", "41-60", "61-80", "81-100")
    )
  ) %>%
  count(year, therm_group)

ggplot(anes_freq_20, aes(
  x = therm_group,
  y = year,
  size = n,
  color = year
)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(
    name = "Frequency",
    range = c(3, 15)
  ) +
  labs(
    x = "Feeling Thermometer Score",
    y = "Year",
    color = "Year",
    title = "Feeling Thermometer Scores by Year"
  ) +
  theme_minimal()
