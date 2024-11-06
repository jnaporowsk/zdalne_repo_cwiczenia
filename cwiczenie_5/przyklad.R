pkg = c("tidymodels",
        "glmnet",
        "ranger",
        "readr",
        "tidymodels",
        "vip",
        "ggthemes")

pkg |> 
  purrr::map(.f = ~ require(.x, character.only = T))

tidymodels_prefer()

hotele <- 
  read_csv(file = "https://tidymodels.org/start/case-study/hotels.csv") |>  
  mutate_if(is.character, as.factor)  

hotele |> dim() 

hotele |> glimpse()


hotele |>
  count(children) |> 
  mutate(prop = n/sum(n)) |> 
  gt::gt() |> 
  gt::tab_header(title = "Zmienna objaśniana")

set.seed(123)

splits <- 
  initial_split(data = hotele,
                prop = 3/4,
                strata = "children")

hotel_other <- training(splits)
hotel_test <- testing(splits)


hotel_other |> 
  count(children) |> 
  mutate(prop = n/sum(n))


hotel_test |>    
  count(children) |>    
  mutate(prop = n/sum(n))

set.seed(234)

val_set <- validation_split(data = hotel_other,
                            prop = 3/4,
                            strata = "children")
val_set


lr_mod <-
  logistic_reg(penalty = tune(),
               mixture = 1) |>
  set_engine(engine = "glmnet") |>
  set_mode("classification")


holidays <-
  c(
    "AllSouls",
    "AshWednesday",
    "ChristmasEve",
    "Easter",
    "ChristmasDay",
    "GoodFriday",
    "NewYearsDay",
    "PalmSunday"
  )

lr_recipe <-    
  recipe(children ~ ., data = hotel_other) |>    
  step_date(arrival_date) |>    
  step_holiday(arrival_date, holidays = holidays) |>    
  step_rm(arrival_date) |>    
  step_dummy(all_nominal_predictors()) |>    
  step_zv(all_predictors()) |>    
  step_normalize(all_predictors())

lr_recipe |> prep() |> bake(hotel_other) |> glimpse()

lr_workflow <- 
  workflow() |> 
  add_model(lr_mod) |> 
  add_recipe(lr_recipe)

# automat
lr_grid <- grid_regular(penalty(), levels = 30)

lr_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))


lr_res <- 
  lr_workflow |> 
  tune_grid(
    resamples = val_set,
    grid = lr_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )

library(ggthemes)

lr_plot <-
  lr_res |>
  collect_metrics() |>
  ggplot(aes(penalty, mean)) +
  geom_point(size = 2) +
  geom_line(linetype = 2) +
  ylab("Pole powierzchni pod krzywa ROC") +
  scale_x_log10() +
  geom_text(aes(
    x = penalty,
    y = mean + 0.03,
    label = .config |> stringr::str_sub(20, 21)
  )) + 
  theme_solarized(light=TRUE) +
  scale_colour_solarized()

lr_plot

top_models <- 
  lr_res |> 
  show_best(metric = "roc_auc", n = 15) |> 
  arrange(penalty) |> 
  mutate(mean = mean |> round(x = _, digits = 3))

top_models |> gt::gt()

lr_best <- 
  lr_res |> 
  collect_metrics() |> 
  arrange(penalty) |> 
  slice(22)

lr_best

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc) +
  ggdark::dark_theme_gray()


# model lasu losowego

cores <- parallel::detectCores()


rf_mod <-
  rand_forest(mtry = tune(),
              min_n = tune(),
              trees = 1000) |>
  set_engine(engine = "ranger",
             num.threads = cores - 1) |>
  set_mode(mode = "classification")

rf_recipe <- 
  recipe(children ~ ., data = hotel_other) |> 
  step_date(arrival_date) |> 
  step_holiday(holidays = holidays) |> 
  step_rm(arrival_date)

rf_workflow <- 
  workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(rf_recipe)

rf_mod

extract_parameter_set_dials(rf_mod)

set.seed(345)
rf_res <- 
  rf_workflow |> 
  tune_grid(resamples = val_set, 
            grid = 25, 
            control = control_grid(save_pred = T),
            metrics = metric_set(roc_auc))

rf_res |> show_best(n = 5)

autoplot(rf_res) + 
  geom_line() + 
  geom_point(size = 2) + 
  ggdark:::dark_theme_dark()

rf_best <- 
  rf_res |> select_best()
rf_best


rf_auc <-
  rf_res |>
  collect_predictions(parameters = rf_best) |>
  roc_curve(children, .pred_children) |>
  mutate(model = "Random Forest")

rf_auc |> 
  autoplot() + 
  ggdark::dark_theme_dark()


bind_rows(lr_auc,rf_auc) |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) +
  geom_path(lwd = 1.5) +
  geom_abline(lty = 3) + 
  coord_equal() +
  scale_color_manual(values = c("black", "white")) +
  ggdark::dark_theme_dark()


last_rf_mod <- 
  rand_forest(mtry = 3, min_n = 3, trees = 1000) |> 
  set_engine("ranger", num.threads = cores-2, importance = "impurity") |> 
  set_mode("classification")

last_rf_work <- 
  rf_workflow |> 
  update_model(last_rf_mod)

set.seed(345)
last_rf_fit <- 
  last_rf_work |> 
  last_fit(split = splits)

last_rf_fit

last_rf_fit |> 
  collect_metrics()


last_rf_fit |> 
  extract_fit_parsnip() |> 
  vip(num_features = 20) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_boxplot(color = "black", fill = "grey85") +
  ggdark::dark_theme_dark()


last_rf_fit |> 
  collect_predictions() |> 
  roc_curve(children, .pred_children) |> 
  autoplot() +
  ggdark::dark_theme_dark()
