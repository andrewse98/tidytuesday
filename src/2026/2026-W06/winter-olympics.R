library(tidyverse)
set_theme(theme_bw())

tuesdata <- tidytuesdayR::tt_load("2026-02-10")
schedule <- tuesdata$schedule

events <- schedule |>
  select(event_code, event_description, is_training) |>
  unique()

events |> sample_n(10)

# Events may have multiple days or stretches across multiple occurences:

schedule |>
  group_by(discipline_name) |>
  count() |>
  ungroup() |>
  mutate(discipline_name = fct_reorder(discipline_name, n)) |>
  ggplot(aes(x = discipline_name, y = n)) +
  geom_bar(stat = "identity", fill = "#5e81ac", alpha = .6) +
  coord_flip() +
  labs(
    title = "Curling has by far the most total events compared to other discipline",
    x = "",
    y = "# of events"
  )

schedule |>
  filter(discipline_name == "Curling") |>
  mutate(duration = difftime(end_time, start_time, units = "mins")) |>
  group_by(event_description) |>
  summarise(
    n_event = n(),
    avg_duration = mean(duration)
  ) |>
  print(n = Inf)
