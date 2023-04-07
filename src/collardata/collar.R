library(readr)
library(dplyr)
library(ggplot2)
library(here)
library(janitor)

collar <- read_csv2(here("data", "collars_ziepbeek.csv")) %>%
  clean_names()

collar <- collar %>%
  group_by(periode) %>%
  mutate(
    gewogen_aantal_posities =
      aantal_posities_per_biotoop * opp_biotoop / sum(opp_biotoop),
    aandeel_gewogen_aantal_posities =
      gewogen_aantal_posities / sum(gewogen_aantal_posities)
  )

collar %>%
  ggplot() +
  geom_col(aes(x = periode, y = gewogen_aantal_posities, fill = biotoop),
           position = position_dodge2(width = 0.5))

collar %>%
  ggplot() +
  geom_col(aes(x = periode, y = aandeel_gewogen_aantal_posities,
               fill = biotoop)) +
  scale_y_continuous(labels = scales::percent)
