pkg = c(
  "tidymodels",
  "glmnet",
  "ranger",
  "rpart",
  "readr",
  "vip",
  "ggthemes",
  "openair",
  "gt",
  "tidyverse"
)

pkg |> 
  purrr::map(.f = ~ require(.x, character.only = T)) ; rm(pkg)
tidymodels_prefer()


importMeta(source = "aurn") |> knitr::kable()

# Nottingham Centre
dane <- importAURN(site = "nott", year = 2020)

skimr::skim(dane)

dane <- dane |> 
  select(o3, nox, no2, no, ws, wd, air_temp, date) |> 
  na.omit()

# wczytuje funkcje wd_factor, która konwertuej stopnie na 16 kierunków waitru
source(file = "function_wd_factor.R") 

dane <- dane |> wd_factor() ; dane


dane |> select(-wd, - wd_cardinal) |> 
  GGally::ggpairs()

dane |> select(-wd, -wd_cardinal, -no, -nox) |> 
  GGally::ggpairs()

data_split <- initial_split(data = dane,
                            prop = 3/4,
                            strata = "o3")

data_train <- training(data_split)
data_test <- testing(data_split)
data_train <-  na.omit(data_train)


set.seed(123)
val_set <- validation_split(data = data_train,
                            prop = 3/4,
                            strata = "o3")

# Model regresji logistycznej

# model
lr_mod <- 
  linear_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# recipe
data_recipe <- recipe(o3 ~ ., data = data_train) |> 
  step_rm(no, nox) |> 
  update_role(date, new_role="Data") |> 
  step_date(date, features = c("month", "doy")) |> 
  step_YeoJohnson(all_numeric_predictors(), -all_outcomes()) |> 
  step_time(date, features = c("hour")) |>   
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

data_recipe |> prep() |> bake(data_train) |> _[1:100,] |> DT::datatable()

# workflow
lr_workflow <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(data_recipe)

lr_grid <- grid_regular(penalty(), mixture(), levels = 25)

view(lr_grid)
lr_res <-
  lr_workflow |>
  tune_grid(
    resamples = val_set,
    grid = lr_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rsq, rmse)
  )

# rmse takie samo,mae też, wsm każda statysytka miała praktycznie takie same wartości niezależnie od penalty, 
# próbowałem zmieniać range w penalty ale pojawiały się błędy których nie wiedziałem jak rozwiązać więc wybrałem 
# wartość z najwyższym penalty, czyli Preprocessor1_Model031
top_models <- 
  lr_res |>
  show_best(metric = "rsq", n=100) |> 
  arrange(desc(penalty))

top_models |> gt::gt()

lr_best <- 
  lr_res |> 
  collect_metrics() |> 
  filter(.config == "Preprocessor1_Model031", .metric == "rsq")

lr_best


# Model lasu losowego
cores <- parallel::detectCores()

rf_mod <-
  rand_forest(mtry = tune(),
              min_n = tune(),
              trees = 1000) |>
  set_engine(engine = "ranger",
             num.threads = cores - 1) |>
  set_mode(mode = "regression")

rf_workflow <- 
  workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(data_recipe)

rf_res <- 
  rf_workflow |> 
  tune_grid(resamples = val_set, 
            grid = 25, 
            control = control_grid(save_pred = T),
            metrics = metric_set(rsq, rmse))

rf_res |> show_best(metric = "rsq", n=5)

rf_best <- rf_res |> select_best(metric="rsq")

rf_best

# Model drzewa decyzyjnego

dt_mod <- 
  decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune()) |> 
  set_engine("rpart") |> 
  set_mode("regression")

dt_workflow <- 
  workflow() |> 
  add_model(dt_mod) |> 
  add_recipe(data_recipe)

dt_res <- 
  dt_workflow |> 
  tune_grid(resamples = val_set, 
            grid = 25, 
            control = control_grid(save_pred = T),
            metrics = metric_set(rsq, rmse))

# wybieramy model z jak najmniejszym cost_complecity Preprocessor1_Model24
dt_res |> show_best(metric = "rsq", n=5) |>  arrange(desc(cost_complexity))

dt_best <- 
  dt_res |>   
  collect_metrics() |> 
  filter(.config == "Preprocessor1_Model24", .metric == "rsq")

dt_best
                                                     