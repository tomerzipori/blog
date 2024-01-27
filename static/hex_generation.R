library(hexSticker)
library(tidyverse)
library(bayestestR)
library(tidybayes)
library(ggdist)

draws <- gather_draws(lyengar2022_b_ordinal_model_impersonation, b_Intercept[Response]) |>
  mutate(.value = case_when(Response == 3 ~ 4.1*.value,
                            Response != 3 ~ .value)) |>
  mutate(.value = case_when(Response == 3 ~ .value - mean(.value) + 0.41,
                            Response != 3 ~ .value))

p <- draws |>
  ggplot() +
  stat_slab(aes(x = .value, fill = ordered(Response)),
            color = "gray14", alpha = 1, key_glyph = "polygon", linewidth = 0.36) +
  scale_fill_discrete(type = c("#f94144", "#f8961e", "#f9c74f", "#90be6d", "#43aa8b", "#577590")) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

s <- sticker(p, package = "tomfooleRy", p_size = 20, s_x=1, s_y=.87, s_width=1.77, s_height=1.2,
             h_fill="#815854", h_color="black", filename = "static/favicon.png")

plot(s)

