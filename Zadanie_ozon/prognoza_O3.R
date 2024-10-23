library(parsnip)
library(tidymodels)
library(tidyverse)

library(readr)
library(broom.mixed)
library(dotwhisker)
install.packages("GGally")
library(GGally)

colnames(airquality) <- tolower(colnames(airquality))

# zmiana danych
air <-
  airquality |>
  as_tibble() |>
  na.omit() |> 
  select(-day) |> 
  mutate(month = factor(month)) 

view(air)

# tworzenie modelu
lm_mod <- linear_reg() |> 
  set_engine("lm")

lm_fit <- 
  lm_mod |> 
  fit(ozone ~ solar.r + wind * temp, data=air)

lm_fit$fit |> tidy()

# tworzenie predykcji
air$predict_ozone <- predict(lm_fit, air)

head(air)

# wykres
ggplot(air, aes(x = ozone, y = predict_ozone$.pred)) +
  geom_point(color = "blue") +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Rzeczywiste vs Prognozowane wartości stężenia ozonu",
       x = "Rzeczywiste Ozone",
       y = "Prognozowane Ozone") +
  theme_minimal()

library(yardstick)

multi <- metric_set(mae, rmse, rsq, rsq_trad)

air |> augment(lm_fit, new_data = _) |> 
  group_by(month) |> 
  multi(truth = ozone, estimate = .pred) |>  
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  gt::gt() |> 
  gt::fmt_number(n_sigfig = 3)
  

# ggally
ggpairs(air[, c("ozone", "solar.r", "wind", "temp", "month")])



