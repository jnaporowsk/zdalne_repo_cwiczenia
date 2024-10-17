library(tidymodels) 
library(tidyverse)
library(skimr) 
library(GGally) 
library(openair) 
library(glmnet)
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

# pierwsze recipe, zły model, krzywa roc była kwadratowa, model nie popełniał błędów

# air_recipe <- recipe(ozone ~ ., data = train_data) |> 
#   update_role(date, new_role="Data") |> 
#   step_date(date, features = c("month", "doy")) |> 
#   step_time(date, features = c("hour")) |> 
#   # step_normalize(all_numeric_predictors()) |> 
#   step_YeoJohnson(all_numeric_predictors(), -all_outcomes()) |> 
#   step_dummy(all_nominal_predictors()) |> 
#   step_zv(all_predictors()) |> 
#   step_corr(all_numeric_predictors(), threshold = 0.9)  




# reicpe z usuniętym o3
air_recipe_o3 <- recipe(ozone ~ ., data = train_data) |> 
  step_rm(o3) |> 
  update_role(date, new_role="Data") |> 
  step_date(date, features = c("month", "doy")) |> 
  step_YeoJohnson(all_numeric_predictors(), -all_outcomes()) |> # wartość roc jest lepsza o 0.009
  step_time(date, features = c("hour")) |> 
  # step_normalize(all_numeric_predictors()) |> # normalizacja nic nie zmienia
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

air_recipe_o3 |> prep() |> bake(train_data) |> _[1:100,] |> DT::datatable()

lr_mod <- 
  logistic_reg(penalty = 0.0001, mixture = 1) |> 
  set_engine("glmnet")


# workflow
final_model <- workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(air_recipe_o3) 

air_fit <- 
  final_model |> 
  fit(data = train_data)

air_fit |> 
  extract_fit_parsnip() |> 
  tidy()

air_fit |> 
  extract_recipe()

predictions <- predict(air_fit, test_data, type = "prob")

pred_test <- 
  augment(air_fit, test_data) |> 
  select(.pred_class:date, ozone)

pred_test  |> 
  roc_curve(truth = ozone, .pred_Niskie) |> 
  autoplot()

pred_test |> conf_mat(truth = ozone, estimate = .pred_class)

pred_test |> 
  roc_auc(truth = ozone, .pred_Niskie)

