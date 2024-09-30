# biblioteki

library(parsnip)
library(tidymodels)
library(tidyverse)

library(readr)
library(broom.mixed)
library(dotwhisker)

# import danych
urchins <- 
  read_csv("https://tidymodels.org/start/models/urchins.csv") |> 
  setNames(c("food_regime", "initial_volume", "width")) |> 
  mutate(food_regime = factor(food_regime, 
                              levels = c("Initial", "Low", "High")))


# opis dancyh i sprawdzenie ich wyglądu
# grupa eksperymentalnego reżimu żywieniowego(food_regime: albo Initial, Low, albo High),
# wielkość w mililitrach na początku doświadczenia (initial_volume), oraz
# szerokość szwu na końcu doświadczenia (width).
urchins


# sprawdzanie czy nasze wartości mają braki
urchins |> is.na() |> as_tibble() |> summarise_all(sum)


# pierwsza analiza wykresu
urchins |> ggplot(aes(
  x = initial_volume,
  y = width,
  col = food_regime,
  group = food_regime
)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  scale_color_viridis_d(option = "C", end = .9)

# dopasowanie modelu

lm_mod <- linear_reg() |> 
  set_engine("lm")

lm_fit <- 
  lm_mod |> 
  fit(width ~ initial_volume * food_regime, data=urchins)

# różne przedstawienie modelu
print(lm_fit, digits = 5)

lm_fit$fit |> summary()

lm_fit |> tidy()

lm_fit |> tidy(conf.int = T)

# wykres wyników regresji z pakietem dotwhisker

lm_fit |> 
  tidy() |> 
  dwplot(vline = geom_vline(xintercept = 0, color = "grey50", linetype = 2),
         dot_args = list(size=2, color = "black"),
         whisker_args = list(color = "black")) + 
  theme_bw()


# prognozowanie

new_points <- expand.grid(initial_volume = seq(5,45,5),
                          food_regime = c("Initial", "Low", "High"))

# prognoza średniej wartości
mean_pred <- predict(object = lm_fit, new_data = new_points)

# prognoza przedziału ufności
conf_pred <- predict(object = lm_fit, new_data = new_points, type = "conf_int")

# łączenie danych
lm_pred <- 
  new_points |>
  bind_cols(mean_pred) |> 
  bind_cols(conf_pred)

# wykres danych
lm_pred |> 
  ggplot(aes(x = food_regime, y = .pred)) +
  geom_point() + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper),
                width = 0.2) + 
  facet_wrap(~ initial_volume) +
  theme_bw() + 
  labs(y="urchni size")
  