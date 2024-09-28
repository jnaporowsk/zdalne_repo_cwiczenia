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


#
urchins |> ggplot(aes(
  x = initial_volume,
  y = width,
  col = food_regime,
  group = food_regime
)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  scale_color_viridis_d(option = "C", end = .9)
