suppressMessages({
  library(tidymodels)
  library(tidyverse)
})

source("./R/90TrainModel.R")

full_data_train <- read_csv("./data/full_data_train_balanced.csv") |> 
  mutate(Cancerous = Cancerous |> factor())
area_data_train <- read_csv("./data/area_data_train_balanced.csv") |> 
  mutate(Cancerous = Cancerous |> factor())
spatial_data_train <- read_csv("./data/spatial_data_train_balanced.csv") |> 
  mutate(Cancerous = Cancerous |> factor())

full_data_recipe <- readRDS("./models/full_data_recipe.rds")
area_data_recipe <- readRDS("./models/area_data_recipe.rds")
spatial_data_recipe <- readRDS("./models/spatial_data_recipe.rds")

full_data_model <- train_model(full_data_train, full_data_recipe)
area_data_model <- train_model(area_data_train, area_data_recipe)
spatial_data_model <- train_model(spatial_data_train, spatial_data_recipe)

full_data_model |> 
  saveRDS("./models/full_data_model.rds")

area_data_model |> 
  saveRDS("./models/area_data_model.rds")

spatial_data_model |> 
  saveRDS("./models/spatial_data_model.rds")
