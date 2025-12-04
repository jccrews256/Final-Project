################################################################################
#
#Title: Diabetes Prediction API
#Author: Cass Crews
#
#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
################################################################################

#Reading in required packages
library(plumber)
library(tidyverse)
library(tidymodels)
library(rsample)
library(ranger)
library(future)

#Reading in data and subsetting to variables of interest
raw_data<-read_csv("../diabetes_binary_health_indicators_BRFSS2015.csv") |>
  select(Diabetes_binary, HighBP, HighChol, PhysActivity, Fruits, Veggies, 
         Smoker, HvyAlcoholConsump, Sex, Age, BMI, Income)

#Converting all but bmi to factors with clean labels and names
data<-raw_data |>
  mutate(diabetes_binary = factor(Diabetes_binary, levels = 0:1, labels = c("No", "Yes"))) |>
  mutate(high_bp = factor(HighBP, levels = 0:1, labels = c("No", "Yes"))) |>
  mutate(high_chol = factor(HighChol, levels = 0:1, labels = c("No", "Yes"))) |>
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
  mutate(income = factor(Income, levels = 1:8, labels = c("Less than $10k", "$10k to $15k",
                                                          "$15k to $20k", "$20k to $25k",
                                                          "$25k to $35k", "$35k to $50k",
                                                          "$50k to $75k", "$75k+"))) |> 
  rename(bmi = BMI) |>
  select(!c(Diabetes_binary:Age, Income))

#Deploying five cores to speed up model fitting
plan(multisession, workers = 5)

#Constructing tidymodels recipe
recipe<-recipe(diabetes_binary ~ ., data = data)

#Specifying model and engine for random forest
rf_model<-rand_forest(mtry = 3,
                      min_n = 20,
                      trees = 500) |>
  set_engine("ranger", seed = 25) |>
  set_mode("classification")

#Creating random forest workflow
rf_wkf<-workflow() |>
  add_recipe(recipe) |>
  add_model(rf_model)

#Fitting final model
rf_fit<-rf_wkf |>
  fit(data)

#Extracting predictions for training set for confusion matrix
predictions<-predict(rf_fit,new_data = data)$.pred_class

#* @apiTitle Diabetes Prediction API
#* @apiDescription This API is designed to predict whether or not an individual has diabetes based on user-specified characteristics.

#* API Information
#* @serializer html
#* @get /info
function() {
  "
  <h3>Diabetes Prediction API</h3>
  <br>
  <b>Author:</b> Cass Crews
  <br>
  <p>This API is designed to predict whether an individual has diabetes based on user-specified characteristics. 
  The <b>pred</b> endpoint is used to generate predictions and corresponding probabilities,
  while the <b>confusion</b> endpoint is used to generate a confusion matrix plot for the underlying predictive model.
  To learn more about the underlying model, see this <a href='https://jccrews256.github.io/Final-Project/Modeling.html'>page</a>.</p>
  
  <p>For reference, the parameters for the <b>pred</b> endpoint are:
  <ul>
    <li>phys: Physical activity indicator (1 if active once per week, 0 if not)</li>
    <li>chol: High cholesterol indicator (1 if cholesterol high, 0 if not)</li>
    <li>bp: High blood pressure indicator (1 if blood pressure high, 0 if not)</li>
    <li>fruit: Fruit consumption indicator (1 if consumes at least one serving of fruit per day, 0 if not)</li>
    <li>veg: Vegetable consumption indicator (1 if consumes at least one serving of vegetables per day, 0 if not)</li>
    <li>smoker: Smoking history indicator (1 if smoked at least 100 cigarettes in life, 0 if not)</li>
    <li>alc: Heavy alcohol consumption indicator (1 if heavy drinker, 0 if not)</li>
    <li>sex: Sex of individual (0 if female, 1 if male)</li>
    <li>age: Age in years (18+)</li>
    <li>bmi: Body mass index (BMI)</li>
    <li>income: Annual income in dollars</li>
  </ul>
  <p>
  "
}

#Query the info via
#http://127.0.0.1:8500/info

#* Generate diabetes prediction
#* @param phys Physical activity indicator (1 if active once per week, 0 if not)
#* @param chol High cholesterol indicator (1 if cholesterol high, 0 if not)
#* @param bp High blood pressure indicator (1 if blood pressure high, 0 if not)
#* @param fruit Fruit consumption indicator (1 if consumes at least one serving of fruit per day, 0 if not)
#* @param veg Vegetable consumption indicator (1 if consumes at least one serving of vegetables per day, 0 if not)
#* @param smoker Smoking history indicator (1 if smoked at least 100 cigarettes in life, 0 if not)
#* @param alc Heavy alcohol consumption indicator (1 if heavy drinker, 0 if not)
#* @param sex Sex of individual (0 if female, 1 if male)
#* @param age Age in years (18+)
#* @param bmi Body mass index (BMI)
#* @param income Annual income in dollars
#* @get /pred
function(phys, chol, bp, fruit, veg, smoker, alc, sex, age, bmi, income){
  #Converting age input to numeric
  age<-as.numeric(age)
  
  #Coding age
  age_code<-case_when(
    age %in% 18:24 ~ 1,
    age %in% 25:29 ~ 2,
    age %in% 30:34 ~ 3,
    age %in% 35:39 ~ 4,
    age %in% 40:44 ~ 5,
    age %in% 45:49 ~ 6,
    age %in% 50:54 ~ 7,
    age %in% 55:59 ~ 8,
    age %in% 60:64 ~ 9,
    age %in% 65:69 ~ 10,
    age %in% 70:74 ~ 11,
    age %in% 75:79 ~ 12,
    age>=80 ~ 13
  )
  
  #Converting income to numeric
  income<-as.numeric(income)
  
  #Coding income
  income_code<-case_when(
    income< 10000 ~ 1,
    income>=10000 & income< 15000 ~ 2,
    income>=15000 & income< 20000 ~ 3,
    income>=20000 & income< 25000 ~ 4,
    income>=25000 & income< 35000 ~ 5,
    income>=35000 & income< 50000 ~ 6,
    income>=50000 & income< 75000 ~ 7,
    income>=75000 ~ 8
  )
  
  #Constructing new observation based on user inputs to API call
  new_obs<-tibble(.rows = 1) |>
    mutate(phys_activity = factor(as.numeric(phys), levels = 0:1, labels = c("No", "Yes"))) |>
    mutate(high_bp = factor(as.numeric(bp), levels = 0:1, labels = c("No", "Yes"))) |>
    mutate(high_chol = factor(as.numeric(chol), levels = 0:1, labels = c("No", "Yes"))) |>
    mutate(fruits = factor(as.numeric(fruit), levels = 0:1, labels = c("No", "Yes"))) |>
    mutate(veggies = factor(as.numeric(veg), levels = 0:1, labels = c("No", "Yes"))) |>
    mutate(smoker = factor(as.numeric(smoker), levels = 0:1, labels = c("No", "Yes"))) |>
    mutate(hvy_alc_consump = factor(as.numeric(alc), levels = 0:1, labels = c("No", "Yes"))) |>
    mutate(sex = factor(as.numeric(sex), levels = 0:1, labels = c("Female", "Male"))) |>
    mutate(age = factor(age_code, levels = 1:13, labels = c("18 to 24", "25 to 29",
                                                     "30 to 34", "35 to 39",
                                                     "40 to 44", "45 to 49",
                                                     "50 to 54", "55 to 59",
                                                      "60 to 64", "65 to 69",
                                                     "70 to 74", "75 to 79",
                                                     "80+"))) |>
    mutate(bmi = as.numeric(bmi)) |>
    mutate(income = factor(income_code, levels = 1:8, labels = c("Less than $10k", "$10k to $15k",
                                                       "$15k to $20k", "$20k to $25k",
                                                       "$25k to $35k", "$35k to $50k",
                                                       "$50k to $75k", "$75k+")))

  #Capturing predictions and prediction probabilities
  class<-predict(rf_fit, new_data = new_obs, type = "class")$.pred_class
  prob<-predict(rf_fit, new_data = new_obs, type = "prob")$.pred_Yes
  
  #Capturing outputs
  list(predicted_class = class, probability_of_diabetes = prob)
}

#Try out a few API prediction calls! 
#Example Yes prediction
#http://127.0.0.1:8500/pred?phys=0&chol=1&bp=1&fruit=0&veg=0&smoker=1&alc=0&sex=1&age=62&bmi=40&income=12000
#More confident Yes prediction
#http://127.0.0.1:8500/predict?phys=0&chol=1&bp=1&fruit=1&veg=1&smoker=1&alc=0&sex=1&age=62&bmi=60&income=7000
#Example "hard" no prediction
#http://127.0.0.1:8500/pred?phys=1&chol=0&bp=0&fruit=1&veg=0&smoker=0&alc=1&sex=0&age=23&bmi=19&income=45000

#* Generate training set confusion matrix for the underlying model
#* @serializer png
#* @get /confusion
function() {
  #Printing training set confusion matrix
    print(autoplot(conf_mat(data |> mutate(prediction = predictions), diabetes_binary, 
                            prediction), type = "heatmap"))
}

#Query confusion matrix via
#http://127.0.0.1:8500/confusion
