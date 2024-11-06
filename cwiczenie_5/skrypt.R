pkg = c(
  "tidymodels",
  "glmnet",
  "ranger",
  "rpart",
  "readr",
  "tidymodels",
  "vip",
  "ggthemes",
  "openair",
  "gt"
)

pkg |> 
  purrr::map(.f = ~ require(.x, character.only = T)) ; rm(pkg)
tidymodels_prefer()


importMeta(source = "aurn") |> knitr::kable()

# Nottingham Centre
dane <- importAURN(site = "nott", year = 2020)

skimr::skim(dane)

dane <- dane |> 
  select(o3, nox, no2, no, ws, wd, air_temp) |> 
  na.omit()

# wczytuje funkcje wd_factor, która konwertuej stopnie na 16 kierunków waitru
source(file = "function_wd_factor.R") 

dane <- dane |> wd_factor() ; dane


dane |> select(-wd, - wd_cardinal) |> 
  GGally::ggpairs()

dane |> select(-wd, -wd_cardinal, -no, -nox) |> 
  GGally::ggpairs()
