library(hexSticker)
library(tidyverse)
library(ggpubr)
library(bayestestR)
library(tidybayes)
library(ggdist)

lyengar2022_b_ordinal_model_impersonation <- read_rds("~/GitHub/SDT_inoculation/models/lyengar2022_bayes_ordinal_model_impersonation.rds")

draws <- gather_draws(lyengar2022_b_ordinal_model_impersonation, b_Intercept[Response])

mean(draws$.value[draws$Response == 1]) # -1.222616
sd(draws$.value[draws$Response == 1]) # 0.03846351

mean(draws$.value[draws$Response == 2]) # -0.5005143
sd(draws$.value[draws$Response == 2]) # 0.02984227

mean(draws$.value[draws$Response == 3]) # 0.07778103
sd(draws$.value[draws$Response == 3]) # 0.02788536

mean(draws$.value[draws$Response == 4]) # 0.6029847
sd(draws$.value[draws$Response == 4]) # 0.03023413

mean(draws$.value[draws$Response == 5]) # 1.000049
sd(draws$.value[draws$Response == 5]) # 0.03424669

mean(draws$.value[draws$Response == 6]) # 1.471166
sd(draws$.value[draws$Response == 6]) # 0.04095912

draws <- gather_draws(lyengar2022_b_ordinal_model_impersonation, b_Intercept[Response]) |>
  mutate(.value = 45*.value) |>
  mutate(.value = case_when(Response == 1 ~ .value + 51,
                            Response == 2 ~ .value + 22,
                            Response == 3 ~ .value,
                            Response == 4 ~ .value - 20,
                            Response == 5 ~ .value - 34,
                            Response == 6 ~ .value - 51))

image <- png::readPNG("static/chill-guy.png")

im2 <- matrix(rgb(image[,,1],image[,,2],image[,,3], image[,,4] * 0.5), nrow = dim(image)[1]) # alpha = 0.5

p <- draws |>
  ggplot() +
  background_image(im2) +
  stat_slab(aes(x = .value, fill = ordered(Response)),
            color = "gray14", alpha = 0.8, key_glyph = "polygon", linewidth = 0.36) +
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

p

s <- sticker(p, package = "chill hex logo", p_color = "#1e1f21", p_size = 20, s_x=1, s_y=.57, s_width=2, s_height=1.8,
             h_fill="#6e5774", h_color="black", filename = "static/favicon.png", white_around_sticker = T)

plot(s)

