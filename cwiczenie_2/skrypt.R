library(tidymodels) 
library(tidyverse)
library(skimr) 
library(GGally) 
library(openair) 
tidymodels_prefer()

air <- mydata |> selectByDate(year = 2004) 
air |> skim()

air <- air |> na.omit()


set.seed(222)
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  select(nox, no2) |> 
  ggpairs()



library(ggpubr)
# wykres regresji liniowej, do sprawdzenia danych 
set.seed(222)
air[sample(1:nrow(air), size = 300, replace = F),] |> 
  select(nox, no2) |> 
  ggplot(aes(nox, no2)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, formula = y ~ x) + 
  stat_cor(label.x = 10, label.y = 80) + 
  stat_regline_equation(label.x = 10, label.y = 82) +
  theme_bw()


air |>    
  ggplot(aes(date, o3)) +     
  geom_line() +     
  theme_bw()


air |> 
  pull(o3) |> 
  range()  

air <-
  air |>
  mutate(ozone = cut(
    o3,
    breaks = c(-0.1, 10, 43),
    labels = c("Niskie", "Wysokie")
  ))
air |> count(ozone)


air |>
  skimr::skim()

# Podział danych
set.seed(222)
data_split <- initial_split(air, strata = ozone)
train_data <- training(data_split)
test_data <- testing(data_split)


air_recipe <- recipe(ozone ~ ., data = train_data) |> 
  step_date(date, features = c("month", "doy")) |> 
  step_time(date, features = c("hour")) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

air_recipe   |> summary()

air_recipe |> prep() |> bake(train_data) |> _[1:100,] |> DT::datatable()

air_recipe |> prep()
