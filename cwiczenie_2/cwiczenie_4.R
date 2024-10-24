library(tidymodels)

# Dodatkowe pakiety
library(rpart.plot)  # wizualizacja drzew decyzyjnych 
library(vip)  


data("cells", package = "modeldata")
cells

set.seed(123)
split <- initial_split(data = cells |> select(-case), 
                       prop = 3/4, 
                       strata = class)

train <- training(split)
test <- testing(split)

tune_spec <- 
  decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune(), 
    min_n = tune()) |> 
  set_engine("rpart") |> 
  set_mode("classification")

tune_spec


siatka <- grid_regular(cost_complexity(), 
                       tree_depth(),
                       min_n(), 
                       levels = 5)
siatka


# podgląd parametrów 

siatka |> 
  count(tree_depth)
siatka |> 
  count(cost_complexity)
siatka |> 
  count(min_n)


set.seed(234)
folds <- vfold_cv(train)


set.seed(345)

# workflow

work <- 
  workflow() |> 
  add_model(tune_spec) |> 
  add_formula(class ~ .)

# statystyki oceny dokładnosci modelu 

miary_oceny <-
  yardstick::metric_set(# tym parametrem możesz definiować
    accuracy,
    mcc,
    npv,
    roc_auc)

# Optymalizacja 

fit_tree <-
  work |>
  tune_grid(
    resamples = folds,
    grid = siatka,
    metrics = miary_oceny
  )

fit_tree

fit_tree |> collect_metrics()

fit_tree |> 
  collect_metrics() |> 
  mutate(tree_depth = factor(tree_depth)) |> 
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

fit_tree |> show_best(metric = "accuracy")
fit_tree |> show_best(metric = "roc_auc")


fit_tree |> select_best(metric = "accuracy")

best_mod <- fit_tree |> select_best(metric = "accuracy")

final_mod <-  
  work |> 
  finalize_workflow(best_mod)

final_fit <- 
  final_mod |> 
  last_fit(split = split)

final_fit |> 
  collect_metrics()

final_fit |> 
  collect_predictions() |> 
  roc_curve(truth = class, .pred_PS) |> 
  autoplot()

final_fit |> extract_workflow()

final_fit |> 
  extract_workflow() |> 
  extract_fit_engine() |> 
  rpart.plot(roundint = F)


# wykres 

final_fit |> 
  extract_workflow() |> 
  extract_fit_parsnip() |>
  vip() 

# eksport danych do tabeli

final_fit |>
  extract_workflow() |>
  extract_fit_parsnip() |>
  vip() |> 
  _$data |> 
  knitr::kable(digits = 1)

# 4.6
args(decision_tree)
# lub 
?decision_tree()

# Można zmienić jeszcze min_n.Ustawienie wysokiej wartości dla min_n sprawia, że drzewo będzie bardziej płytkie i prostsze, 
# ponieważ mniej węzłów będzie miało wystarczającą liczbę próbek do dalszego podziału. W rezultacie model jest 
# mniej narażony na przeuczenie (overfitting), ale może nie być w stanie w pełni dopasować się do danych (underfitting).

# Z kolei niska wartość min_n pozwala na głębsze drzewa z większą liczbą podziałów, co zwiększa szansę na lepsze 
# dopasowanie do danych treningowych, ale także może prowadzić do nadmiernego dopasowania.