# Final-Project
This repository holds my work developing a model to predict whether or not an individual has diabetes. To train and tune the model, I used this [dataset](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/data?select=diabetes_binary_health_indicators_BRFSS2015.csv). 

There are three main code files in this repository:
1. EDA.qmd is a quarto document, which has also been rendered as a web page, covering the data cleaning process and summarization of the data.
2. Modeling.qmd is a quarto document, which has also been rendered as a web page, covering the model fitting and tuning process. Two different model classes were explored (classification trees and random forests), with the final model being a random forest.
3. Within the modelAPI folder is API.R, which is a plumber API script. When run, it allows a user to use the final predictive model to predict whether or not an individual has diabetes (the `pred` endpoint). There are two other endpoints: an `info` endpoint giving a basic overview of the API and a `confusion` endpoint that produces the final training set confusion matrix for the model.
   - To deploy the API, I recommend running the runAPI.R file, which will ensure the API has a constant port number. 
