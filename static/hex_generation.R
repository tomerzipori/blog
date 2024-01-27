library(hexSticker)
library(tidyverse)
library(bayestestR)
library(tidybayes)
library(ggdist)

lyengar2022_b_ordinal_model_impersonation <- read_rds("C:/Users/tomer/OneDrive/GitHub/SDT_inoculation/models/lyengar2022_b_ordinal_model_impersonation.rds")

draws <- gather_draws(lyengar2022_b_ordinal_model_impersonation, b_Intercept[Response])

mean(draws$.value[draws$Response == 1]) # -6.345
sd(draws$.value[draws$Response == 1]) # 1.785

mean(draws$.value[draws$Response == 2]) # -2.595
sd(draws$.value[draws$Response == 2]) # 0.740

mean(draws$.value[draws$Response == 3]) # 0.412
sd(draws$.value[draws$Response == 3]) # 0.18

mean(draws$.value[draws$Response == 4]) # 3.124
sd(draws$.value[draws$Response == 4]) # 0.885

mean(draws$.value[draws$Response == 5]) # 5.156
sd(draws$.value[draws$Response == 5]) # 1.449

mean(draws$.value[draws$Response == 6]) # 7.536
sd(draws$.value[draws$Response == 6]) # 2.114

draws <- gather_draws(lyengar2022_b_ordinal_model_impersonation, b_Intercept[Response]) |>
  mutate(.value = case_when(Response == 2 ~ 2.1*.value,
                            Response == 3 ~ 7.9*.value,
                            Response == 4 ~ 2*.value,
                            .default = .value)) |>
  mutate(.value = case_when(Response == 2 ~ .value - mean(.value) + -2.595,
                            Response == 3 ~ .value - mean(.value) + 0.41,
                            Response == 4 ~ .value - mean(.value) + 3.124,
                            .default = .value))

p <- draws |>
  ggplot() +
  stat_slab(aes(x = .value, fill = ordered(Response)),
            color = "gray14", alpha = 1, key_glyph = "polygon", linewidth = 0.36) +
  scale_fill_discrete(type = c("#f94144", "#f8961e", "#f9c74f", "#90be6d", "#43aa8b", "#577590")) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

s <- sticker(p, package = "tomfooleRy", p_size = 20, s_x=1, s_y=.57, s_width=2, s_height=1.8,
             h_fill="#815854", h_color="black", filename = "static/favicon.png")

plot(s)

