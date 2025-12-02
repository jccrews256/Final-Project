#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

#Building the model
library(tidyverse)
library(tidymodels)
library(rsample)
library(ranger)

raw_data<-read_csv("diabetes_binary_health_indicators_BRFSS2015.csv") |>
  select(Diabetes_binary, PhysActivity, Fruits, Veggies, Smoker, HvyAlcoholConsump, Sex, Age)

data<-raw_data |>
  mutate(diabetes_binary = factor(Diabetes_binary, levels = 0:1, labels = c("No", "Yes"))) |>
  mutate(phys_activity = factor(PhysActivity, levels = 0:1, labels = c("No", "Yes"))) |>
  mutate(fruits = factor(Fruits, levels = 0:1, labels = c("No", "Yes"))) |>
  mutate(veggies = factor(Veggies, levels = 0:1, labels = c("No", "Yes"))) |>
  mutate(smoker = factor(Smoker, levels = 0:1, labels = c("No", "Yes"))) |>
  mutate(hvy_alc_consump = factor(HvyAlcoholConsump, levels = 0:1, labels = c("No", "Yes"))) |>
  mutate(sex = factor(Sex, levels = 0:1, labels = c("Female", "Male"))) |>
  mutate(age = factor(Age, levels = 1:13, labels = c("18 to 24", "25 to 29",
                                                     "30 to 34", "35 to 39",
                                                     "40 to 44", "45 to 49",
                                                     "50 to 54", "55 to 59",
                                                     "60 to 64", "65 to 69",
                                                     "70 to 74", "75 to 79",
                                                     "80+"))) |>
  select(!(Diabetes_binary:Age))

#Constructing tidymodels recipe
recipe<-recipe(diabetes_binary ~ ., data = train) |>
  #Creating dummies for all predictors (all are factors)
  step_dummy(all_predictors())

#Printing recipe variable list with roles
recipe |>
  prep(training = train) |>
  summary()

#Specifying model and engine for random forest
rf_model<-rand_forest(mtry = 7,
                      min_n = 5,
                      trees = 500) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

#Creating random forest workflow
rf_wkf<-workflow() |>
  add_recipe(recipe) |>
  add_model(rf_model)

rf_fit<-rf_wkf |>
  fit(data)

#* @apiTitle Diabetes Predictor
#* @apiDescription This API is designed to predict whether or not an individual has diabetes based on user-specified characteristics.

#* Find diabetes prediction
#* @param phys physical activity indicator
#* @param fruit fruit consumption indicator
#* @param veg vegetable consumption indicator
#* @param smoker smoking history indicator
#* @param alc heavy alcohol consumption indicator
#* @param age age in years
#* @get /predict
function(phys, fruit, veg, smoker, alc, age){
  log(as.numeric(num))
}

#query with http://localhost:PORT/ln?num=1
#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}
