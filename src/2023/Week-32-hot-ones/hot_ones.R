# 📚 Library -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(magick)
library(glue)
library(ggtext)
library(showtext)

# 💾 Load data -----------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 32)

episodes <- dat$episodes
sauces <- dat$sauces
seasons <- dat$seasons

# 🤼 Wrangle -------------------------------------------------------------------

sauces_summary <- sauces %>%
  group_by(season) %>%
  summarise(
    avg_scoville = mean(log(scoville)),
    min_scoville = min(log(scoville)),
    max_scoville = max(log(scoville))
  )

# ✍️ fonts and palettes --------------------------------------------------------

bg <- "#000101"
txt <- "#F5F5F5"

font_add("fa-brands", regular = "ext/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Roboto")
showtext_auto()

ft <- "Roboto"

# 🔤 Text ----------------------------------------------------------------------

twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
caption <- glue("{twitter} @setiono_andrew • {github} andrewse98/tidytuesday")

main_title <- "Hot Ones keeps on Getting Hotter!"
subtitle <- "Across the 20 seasons, we saw a steady increase in the average Scoville Heat Unit."

x_title <- "Hot Ones Season"
y_title <- "Scoville Heat Unit (in log scale)"

# 📊 Plot ----------------------------------------------------------------------

ggplot(sauces_summary, aes(x = season, y = avg_scoville)) +
  geom_ribbon(aes(ymin = min_scoville, ymax = max_scoville),
              col = "#E5322F", fill = "#E5322F", alpha = 0.1) +
  geom_line(col = "#FECD00", size = 1) +
  geom_point(col = "#FECD00", size = 5) +
  scale_x_continuous(breaks = seq(1, 25, 1)) +
  scale_y_continuous(limits = c(5, 15), breaks = seq(5, 15, 3)) +
  labs(
    title    = main_title,
    subtitle = subtitle,
    x        = x_title,
    y        = y_title,
    caption  = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.title = element_text(size = 108, face = "bold", margin = margin(b = 10), hjust = 0),
    plot.subtitle = element_text(size = 80, margin = margin(b = 0), hjust = 0),
    plot.background = element_rect(fill = bg),
    plot.caption = element_markdown(size = 50, colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 30, t = 50, r = 50, l = 50),
    axis.text = element_text(),
    axis.title.x = element_text(size = 64, margin = margin(30, 10, 30, 10)),
    axis.title.y = element_text(size = 64, margin = margin(10, 30, 10, 10), angle = 90),
    legend.position = "bottom",
    legend.title = element_text(size = 48),
    legend.text = element_text(size = 48)
  )

ggsave("plots/2023-W32-hot-ones.png", width = 21, height = 13)

# 🔗 Link ----------------------------------------------------------------------

# https://github.com/rfordatascience/tidytuesday

