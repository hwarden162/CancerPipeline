suppressMessages({
  library(doParallel)
  library(fastshap)
  library(logger)
  library(shapviz)
  library(tidymodels)
  library(tidyverse)
  library(stacks)
})

log_info("STARTED: 05SHAPValues.R")

registerDoParallel()

full_data_train <- suppressMessages(read_csv("./data/full_data_train_balanced.csv"))
area_data_train <- suppressMessages(read_csv("./data/area_data_train_balanced.csv"))
spatial_data_train <- suppressMessages(read_csv("./data/spatial_data_train_balanced.csv"))

full_data_test <- suppressMessages(read_csv("./data/full_data_test_balanced.csv"))
area_data_test <- suppressMessages(read_csv("./data/area_data_test_balanced.csv"))
spatial_data_test <- suppressMessages(read_csv("./data/spatial_data_test_balanced.csv"))

full_data_model <- readRDS("./models/full_data_model.rds")
area_data_model <- readRDS("./models/area_data_model.rds")
spatial_data_model <- readRDS("./models/spatial_data_model.rds")

get_shap_vals <- function(data_train, data_test, data_model) {
  
  X_test <- data_test |> 
    select(-Cancerous) |> 
    as.matrix()
  
  X_train <- data_train |> 
    select(-Cancerous) |> 
    as.matrix()
  
  predict_fn <- function(object, newdata) {
    preds <- predict(object, new_data = newdata |> as_tibble(), type = "prob")
    dplyr::pull(preds, ".pred_TRUE")
  }
  
  baseline <- mean(predict_fn(data_model, newdata = data_train))
  
  log_info("Starting shap explainer run")
  vals <- fastshap::explain(
    object = data_model,
    feature_names = X_test |> colnames(),
    X = X_train,
    nsim = 200,
    pred_wrapper = predict_fn,
    newdata = X_test,
    baseline = baseline
  )
  
  shapviz(vals, X=X_test, baseline=baseline)
}

full_data_vals <- get_shap_vals(full_data_train, full_data_test, full_data_model)
log_info("Full Data SHAP Values: CALCULATED")
area_data_vals <- get_shap_vals(area_data_train, area_data_test, area_data_model)
log_info("Morphological Data SHAP Values: CALCULATED")
spatial_data_vals <- get_shap_vals(spatial_data_train, spatial_data_test, spatial_data_model)
log_info("Spatial Data SHAP Values: CALCULATED")

full_data_vals |> 
  saveRDS("./data/full_shap_vals.rds")

area_data_vals |> 
  saveRDS("./data/area_shap_vals.rds")

spatial_data_vals |> 
  saveRDS("./data/spatial_shap_vals.rds")

log_info("COMPLETED: 05SHAPValues.R")