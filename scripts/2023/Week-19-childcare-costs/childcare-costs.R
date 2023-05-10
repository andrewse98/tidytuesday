# 📚 Library -------------------------------------------------------------------

library(lme4)
library(glue)
library(ggtext)
library(showtext)
library(tidyverse)

# 💾 Load data -----------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 19)

costs <- dat$childcare_costs
counties <- dat$counties

# ✍️ fonts and palettes --------------------------------------------------------

bg <- "grey12"
txt <- "#FFFFFF"

font_add("fa-brands", regular = "ext/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Roboto")
showtext_auto()

ft <- "Roboto"

# 🤼 Wrangle -------------------------------------------------------------------

# states with 30 counties
state_30_county <- counties |>
  group_by(state_name) |>
  summarise(n_county = n()) |>
  filter(n_county >= 100)

# population and center-based childcare price
df_costs <- costs |>
  left_join(counties, by = "county_fips_code") |>
  inner_join(state_30_county, by = "state_name") |>
  filter(study_year == 2018) |>
  select(
    state_name,
    county_name,
    total_pop,
    mcsa
  ) |>
  drop_na() |>
  mutate(log_pop = log(total_pop)) |>
  group_by(state_name) |>
  mutate(mean_log_pop = mean(log_pop, na.rm = TRUE)) |>
  ungroup() |>
  mutate(cen_log_pop = log_pop - mean_log_pop)  # group centered log population

# simple multilevel model
  ## empty model
  mla1 <- lmer(mcsa ~ (1|state_name),
               data = df_costs, REML = FALSE)

  # calculate ICCs
  ranef_mla1 <- as.data.frame(VarCorr(mla1))
  tau_2 <- ranef_mla1[1,4]
  sigma_2 <- ranef_mla1[2,4]
  icc = tau_2 / (tau_2 + sigma_2) # 34% ICCs

  ## level-1 fixed effect
  mla2 <- lmer(mcsa ~ cen_log_pop + (1|state_name),
               data = df_costs, REML = FALSE)

  ## random slope model
  mla3 <- lmer(mcsa ~ cen_log_pop + (1 + cen_log_pop|state_name),
               data = df_costs, REML = FALSE)

# random slope model coefficient
mla3_coef <- coef(mla3)$state_name |>
  rename(intercept = `(Intercept)`, slope = cen_log_pop) |>
  rownames_to_column("state_name")

# prep data for mixed model
mla3_data <- left_join(df_costs, mla3_coef, by = "state_name")

# 🔤 Text ----------------------------------------------------------------------

twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
caption <- glue("{twitter} @setiono_andrew • {github} andrewse98/tidytuesday")

main_title <- "Relationsip between County's Population and
Center-based Childcare Costs per State"

subtitle <- "Only states with more than 100 counties"

legend_title <- "States"

x_title <- "Group-centered log(County's Population)"
y_title <- "Weekly full-time median price charged \nfor Center-Based Care"

# 📊 Plot ----------------------------------------------------------------------

mla3_data |>
  ggplot(aes(x = cen_log_pop, y = mcsa, color = state_name)) +
  geom_point(size = 3) +
  geom_abline(aes(intercept = intercept,
                  slope = slope,
                  color = state_name),
              linewidth = 0.5) +
  annotate("text", x = -5.5, y = 300, label = y_title,
           hjust = 0, vjust = 1, size = 16, lineheight = 0.35,
           family = ft, colour = txt) +
  scale_x_continuous(breaks = seq(-5, 5, 2)) +
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  coord_cartesian(xlim = c(min(mla3_data$cen_log_pop),
                           max(mla3_data$cen_log_pop))) +
  scale_color_viridis_d() +
  labs(
    title    = main_title,
    subtitle = subtitle,
    x        = x_title,
    color    = legend_title,
    caption  = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.title = element_text(size = 108, face = "bold", margin = margin(b=10), hjust = 0),
    plot.subtitle = element_text(size = 80, margin = margin(b = 0), hjust = 0),
    plot.background = element_rect(fill = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t=20)),
    plot.margin = margin(b = 30, t = 50, r = 50, l = 50),
    axis.text = element_text(),
    axis.title.x = element_text(size = 64, margin = margin(30, 10, 30, 10)),
    # axis.title.y = element_text(margin = margin(10, 30, 10, 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 48),
    legend.text = element_text(size = 48)
  )

ggsave("scripts/2023/Week-19-childcare-costs/childcare-costs.png", height = 12, width = 18, dpi = 72)

# 🔗 Link ----------------------------------------------------------------------

# https://github.com/rfordatascience/tidytuesday

# inspired by:
# https://github.com/doehm/tidytues/blob/main/scripts/2023/week-18-portal/portal.R

