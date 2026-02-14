library(tidyverse)
set_theme(theme_bw())

tuesdata <- tidytuesdayR::tt_load("2026-02-10")
schedule <- tuesdata$schedule

events <- schedule |>
  select(event_code, event_description, is_training) |>
  unique()

events |> sample_n(10)

schedule_df <- schedule |>
  mutate(
    duration = difftime(end_time, start_time, units = "mins"),
    event_gender_code = str_extract(event_code, "CUR(M|X|W)TEAM", group = 1),
    event_gender = case_when(
      event_gender_code == "M" ~ "Men",
      event_gender_code == "W" ~ "Women",
      event_gender_code == "X" ~ "Mixed",
      .default = NA_character_
    ),
    event_stage_code = str_extract(
      event_code,
      "-([A-Z]+)(?:\\d|-)",
      group = 1
    ),
    event_stage = case_when(
      event_stage_code == "PREL" ~ "Preliminary",
      event_stage_code == "HEAT" ~ "Heat",
      event_stage_code == "QUAL" ~ "Qualifier",
      event_stage_code == "8FNL" ~ "One-eight final",
      event_stage_code == "QFNL" ~ "Quarterfinal",
      event_stage_code == "SFNL" ~ "Semifinal",
      event_stage_code == "FNL" ~ "Final",
      .default = NA_character_
    )
  )

schedule_df |>
  pull(event_stage_code) |>
  unique()


# Events may have multiple days or stretches across multiple occurences:

schedule_df |>
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
