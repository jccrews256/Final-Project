#Basic script to run the API
#Simply click run to deploy the API locally with a consistent URL
#WARNING: It takes a while

#Loading needed package
library(plumber)

#Plumbing API
api<-plumb("modelAPI/API.R")

#Specifying a port for reproducibility and deploying
api$run(port = 8500)

################################################################################
#Example API calls
#These are also available in the API.R file itself

#Query the info endpoint via
#http://127.0.0.1:8500/info

#Try out a few API prediction calls! 
#Example Yes prediction
#http://127.0.0.1:8500/pred?phys=0&chol=1&bp=1&fruit=0&veg=0&smoker=1&alc=0&sex=1&age=62&bmi=40&income=12000
#More confident Yes prediction
#http://127.0.0.1:8500/pred?phys=0&chol=1&bp=1&fruit=1&veg=1&smoker=1&alc=0&sex=1&age=62&bmi=60&income=7000
#Example "hard" no prediction
#http://127.0.0.1:8500/pred?phys=1&chol=0&bp=0&fruit=1&veg=0&smoker=0&alc=1&sex=0&age=23&bmi=19&income=45000

#Query confusion matrix endpoint via
#http://127.0.0.1:8500/confusion