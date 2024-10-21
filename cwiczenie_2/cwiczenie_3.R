library(ranger)
library(modeldata)
library(tidymodels)
tidymodels_prefer()

data("cells", package = "modeldata")
cells
str(cells)


cells |> 
  count(class) |> 
  mutate(prop = n/sum(n) * 100 |> round(x = _, digits = 1))

set.seed(123)
cell_split <- initial_split(data = cells |> select(-case),
                            strata = class, prop = 3/4)

cell_train <- training(cell_split)
cell_test <- testing(cell_split)

nrow(cell_test) ; nrow(cell_train) # liczba

nrow(cell_test)/nrow(cells) ; nrow(cell_train)/nrow(cells) # udział

cell_test |> 
  count(class) |> 
  mutate(prop = n/sum(n))

cell_train |> 
  count(class) |> 
  mutate(prop = n/sum(n))

# modelowanie

rf_mod <- 
  rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("classification")

set.seed(234)
rf_fit <- 
  rf_mod |> 
  fit(class ~ ., data = cell_train)
rf_fit

# Predykcja
rf_pred_train <-
  predict(rf_fit, new_data = cell_train) |> 
  bind_cols(predict(rf_fit, new_data =
                      cell_train, type = "prob")) |> 
  bind_cols(cell_train |> select(class))

# Krzywa ROC
rf_pred_train |> 
  roc_curve(truth = class, .pred_PS) |> 
  autoplot()

# Pole powierzchni
rf_pred_train |> 
  roc_auc(truth = class, .pred_PS)

# Dokładność
rf_pred_train |> 
  accuracy(truth = class, .pred_class)


# testowy
rf_pred_test <- 
  predict(rf_fit, new_data = cell_test) |> 
  bind_cols(predict(rf_fit, new_data = cell_test, type = "prob")) |> 
  bind_cols(cell_test |> select(class))

# Krzywa roc
rf_pred_test |> 
  roc_curve(truth = class, .pred_PS) |> 
  autoplot()

# Powierzchnia
rf_pred_test |> 
  roc_auc(truth = class, .pred_PS)

# Dokładność
rf_pred_test |> 
  accuracy(truth = class, .pred_class)


# 1) rsample - split

set.seed(123)
cell_split <- initial_split(data = cells |> select(-case),
                            strata = class, prop = 3/4)

cell_train <- training(cell_split)
cell_test <- testing(cell_split)


# 2) rsmaple - CV folds

set.seed(345)
folds <- vfold_cv(data = cell_train, v = 10)

# 3) parsnip - model

rf_mod <- 
  rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("classification")

# print data
folds 

# 4) workflow
rf_wf <- 
  workflow() |> 
  add_model(rf_mod) |> 
  add_formula(class ~ .)

# 5) tune 
set.seed(456)
rf_fit_rs <- 
  rf_wf |> 
  fit_resamples(folds)

rf_fit_rs

rf_fit_rs |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

bind_rows(
  rf_pred_test |>
    roc_auc(truth = class, .pred_PS),
  
  rf_pred_test |>
    accuracy(truth = class, .pred_class)
) |>
  knitr::kable(digits = 3)

# CV fold

set.seed("123")
vfold_cv_resample = vfold_cv(train_data, v=10)
vfold_cv_resample
vfold_cv_resample$splits[[1]] %>% analysis() %>% dim()

air_fit_rs <- final_model |> fit_resamples(vfold_cv_resample)
air_fit_rs |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)



# powtarzane cvfold
set.seed("123")
vfold_cv_resample_rep = vfold_cv(train_data, v=10, repeats = 5)
vfold_cv_resample_rep
vfold_cv_resample_rep$splits[[1]] %>% analysis() %>% dim()

air_fit_rs_rep <- final_model |> fit_resamples(vfold_cv_resample_rep)
air_fit_rs_rep |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)


# monte carlo cv
set.seed("123")
mc_cv_resample = mc_cv(train_data, prop=9/10, times = 5)
mc_cv_resample
mc_cv_resample$splits[[1]] %>% analysis() %>% dim()

air_fit_rs_mc <- final_model |> fit_resamples(mc_cv_resample)
air_fit_rs_mc |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)
bind_cols(
  c(rep("Vfold", 3), rep("Vfold-repeated",3), rep("Monte carlo cv",3)),

bind_rows(
  air_fit_rs |> collect_metrics() ,
  air_fit_rs_rep |> collect_metrics() ,
  air_fit_rs_mc |> collect_metrics() 
) )|> 
  knitr::kable(digits = 3)
