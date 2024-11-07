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


## Regresja logistyczna
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

# bootstrap
air_rs_bootstrap <- bootstraps(train_data, times = 5)
air_rs_bootstrap
air_rs_bootstrap$splits[[1]] %>% analysis() %>% dim()

air_fit_rs_bootstrap <- final_model |> fit_resamples(air_rs_bootstrap)
air_fit_rs_bootstrap |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

log_reg_wyniki <- bind_cols(
  c(rep("Vfold", 3), rep("Vfold-repeated",3), rep("Monte carlo cv",3), rep("Bootstrap",3)),

bind_rows(
  air_fit_rs |> collect_metrics() ,
  air_fit_rs_rep |> collect_metrics() ,
  air_fit_rs_mc |> collect_metrics(),
  air_fit_rs_bootstrap |> collect_metrics()
) )|> tibble()

log_reg_wyniki <- rename(log_reg_wyniki, nazwa_metody = ...1)





## metoda lasu losowego

rf_mod <- 
  rand_forest(trees = tune(),
              mtry = tune()) |> 
  set_engine("ranger") |> 
  set_mode("classification")

mtry_param <- finalize(mtry(), x = train_data)
rf_mod

siatka <- grid_regular(mtry_param,
                       trees(), 
                       levels = 5)
siatka
siatka |> count(mtry)


rf_wflow <- workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(air_recipe_o3) 

miary_oceny <-
  yardstick::metric_set(
    accuracy,
    mcc,
    npv,
    roc_auc)



# cv fold
set.seed("123")
air_rf_folds = vfold_cv(train_data, v=10)

library(doParallel)

# Ustawienie liczby wątków równej liczbie rdzeni procesora
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

air_fit_rf_folds <-
  rf_wflow |>
  tune_grid(
    resamples = air_rf_folds,
    grid = siatka,
    metrics = miary_oceny
  )

stopCluster(cl)

air_rf_folds$splits[[1]] %>% analysis() %>% dim()

air_fit_rf_folds |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

air_fit_rf_folds |> select_best(metric = "accuracy")

air_fit_rf_folds_best <- rf_wflow |> finalize_workflow(air_fit_rf_folds |> select_best(metric = "accuracy"))

air_fit_rf_folds_final <- air_fit_rf_folds_best |> last_fit(split = data_split)

air_fit_rf_folds_final |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

# cv fold repeated
set.seed("123")
air_rf_folds_rep = vfold_cv(train_data, v=10, repeats = 5)

# Ustawienie liczby wątków równej liczbie rdzeni procesora
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

air_fit_rf_folds_rep <-
  rf_wflow |>
  tune_grid(
    resamples = air_rf_folds_rep,
    grid = siatka,
    metrics = miary_oceny
  )

stopCluster(cl)

air_rf_folds_rep$splits[[1]] %>% analysis() %>% dim()

air_fit_rf_folds_rep |> select_best(metric = "accuracy")

air_fit_rf_folds_rep_best <- rf_wflow |> finalize_workflow(air_fit_rf_folds_rep |> select_best(metric = "accuracy"))

air_fit_rf_folds_rep_final <- air_fit_rf_folds_rep_best |> last_fit(split = data_split)

air_fit_rf_folds_rep_final |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)

 

# monte carlo cv
set.seed("123")
air_rf_mc = mc_cv(train_data, prop=9/10, times = 10)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

air_fit_rf_mc <-
  rf_wflow |>
  tune_grid(
    resamples = air_rf_mc,
    grid = siatka,
    metrics = miary_oceny
  )

stopCluster(cl)

air_rf_mc$splits[[1]] %>% analysis() %>% dim()

air_fit_rf_mc |> select_best(metric = "accuracy")

air_fit_rf_mc_best <- rf_wflow |> finalize_workflow(air_fit_rf_mc |> select_best(metric = "accuracy"))

air_fit_rf_mc_final <- air_fit_rf_mc_best |> last_fit(split = data_split)

air_fit_rf_mc_final |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)



# bootstrap
set.seed("123")
air_rf_bootstrap <- bootstraps(train_data, times = 10)
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

air_fit_rf_bootstrap <-
  rf_wflow |>
  tune_grid(
    resamples = air_rf_bootstrap,
    grid = siatka,
    metrics = miary_oceny
  )

stopCluster(cl)

air_rf_bootstrap$splits[[1]] %>% analysis() %>% dim()

air_fit_rf_bootstrap |> select_best(metric = "accuracy")

air_fit_rf_bootstrap_best <- rf_wflow |> finalize_workflow(air_fit_rf_bootstrap |> select_best(metric = "accuracy"))

air_fit_rf_bootstrap_final <- air_fit_rf_bootstrap_best |> last_fit(split = data_split)

air_fit_rf_bootstrap_final |> 
  collect_metrics() |> 
  knitr::kable(digits = 3)


rf_wyniki <- bind_cols(
  c(rep("Vfold", 3), rep("Vfold-repeated",3), rep("Monte carlo cv",3), rep("Bootstrap", 3)),
  
  bind_rows(
    air_fit_rf_folds_final |> collect_metrics() ,
    air_fit_rf_folds_rep_final |> collect_metrics() ,
    air_fit_rf_mc_final |> collect_metrics() ,
    air_fit_rf_bootstrap_final |> collect_metrics() 
  ))|> tibble()

rf_wyniki <- rename(rf_wyniki, nazwa_metody = ...1)

wykres_log_reg <- ggplot(log_reg_wyniki, aes(x = nazwa_metody, y = mean, fill = nazwa_metody)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Regresja logistyczna - Porównanie wyników w zależności od metody walidacji",
       y = "Wartość metryki",
       x = " ",
       fill = "Metoda walidacji") +
  facet_wrap(~.metric, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_blank())

wykres_rf <- ggplot(rf_wyniki, aes(x = nazwa_metody, y = .estimate, fill = nazwa_metody)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(.estimate, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Las losowy - Porównanie wyników w zależności od metody walidacji",
       y = "Wartość metryki",
       x = " ",
       fill = "Metoda walidacji") +
  facet_wrap(~.metric, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_blank())
 
library(patchwork)
wykres_rf/wykres_log_reg


save(rf_fit_rs, 
     air_fit_rs_rep, 
     air_fit_rf_folds, 
     air_fit_rf_folds_rep, 
     air_fit_rf_mc,
     air_fit_rf_bootstrap,
     file = "data.RData")
load("data.RData")
