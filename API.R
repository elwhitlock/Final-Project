#API.R 
# used Dr. Post's as temp

# packages
library("tidyverse")
library("tidymodels")
library("ranger")
library("ggplot2")

# read in data
db_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv") |>
  mutate(
    Diabetes_binary = factor(Diabetes_binary,
                             levels = c(0,1),
                             labels = c("No diabetes", "Diabetes")),
    
    HighBP = factor(HighBP,
                    levels = c(0,1),
                    labels = c("No high BP", "High BP")),
    
    HighChol = factor(HighChol,
                      levels = c(0,1),
                      labels = c("No high cholesterol", "High cholesterol")),
    
    CholCheck = factor(CholCheck,
                       levels = c(0,1),
                       labels = c("No cholesterol check", "Checked cholesterol")),
    
    Smoker = factor(Smoker,
                    levels = c(0,1),
                    labels = c("Non-smoker", "Smoker")),
    
    Stroke = factor(Stroke,
                    levels = c(0,1),
                    labels = c("No stroke", "Stroke")),
    
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack,
                                  levels = c(0,1),
                                  labels = c("No CHD/MI", "CHD or MI")),
    
    PhysActivity = factor(PhysActivity,
                          levels = c(0,1),
                          labels = c("No physical activity", "Physically active")),
    
    Fruits = factor(Fruits,
                    levels = c(0,1),
                    labels = c("No fruit consumed", "Fruit consumed")),
    
    Veggies = factor(Veggies, 
                     levels = c(0,1),
                     labels = c("No veggies consumed", "Veggies consumed")),
    
    HvyAlcoholConsump = factor(HvyAlcoholConsump,
                               levels = c(0,1),
                               labels = c("Not heavy drinker", "Heavy drinker")),
    
    AnyHealthcare = factor(AnyHealthcare, 
                           levels = c(0,1),
                           labels = c("No healthcare", "Has healthcare")),
    
    NoDocbcCost = factor(NoDocbcCost, 
                         levels = c(0,1),
                         labels = c("Did not avoid care due to cost", "Avoided care due to cost")),
    
    DiffWalk = factor(DiffWalk, 
                      levels = c(0,1),
                      labels = c("No difficulty walking", "Difficulty walking")),
    
    Sex = factor(Sex, 
                 levels = c(0,1),
                 labels = c("Female", "Male")),
    
    GenHlth = factor(GenHlth, 
                     levels = c(1,2,3,4,5),
                     labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
                     ordered = TRUE),
    
    Age = factor(Age, 
                 levels = 1:13, 
                 labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                            "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
                 ordered = TRUE),
    
    Education = factor(Education, 
                       levels = 1:6, 
                       labels = c("No school", "Grades 1-8", "Grades 9-11","High school graduate",
                                  "Some collegel","College graduate"), 
                       ordered = TRUE),
    
    Income = factor(Income, 
                    levels = 1:8, 
                    labels = c("<10k", "<15k", "<20k", "<25k","<35k", "<50k","<75k","75k+"),
                    ordered = TRUE)
    
  )


# retrieve best model from modeling qmd
rf_final_fit <- readRDS("rf_final_model.rds")


#pred endpoint

#* @param BMI BMI
#* @param Age Age
#* @param GenHlth GenHlth
#* @param HighBP HighBP 
#* @param HighChol HighChol
#* @get /pred
function(
    #set defaults to mean for BMI and most prevalent class for categorical
  BMI = 28, 
  Age = "60-64", 
  GenHlth = "Very good", 
  HighBP = "No high BP",
  HighChol = "No high cholesterol"){
  
  #sorts/converts given values based on og data set
  new_data <- tibble(
    BMI = as.numeric(BMI),
    Age = factor(Age, levels = levels(db_data$Age), ordered = TRUE),
    GenHlth = factor(GenHlth, levels = levels(db_data$GenHlth), ordered = TRUE),
    HighBP = factor(HighBP, levels = levels(db_data$HighBP)),
    HighChol = factor(HighChol, levels = levels(db_data$HighChol))
  )
  
  # gives prediction
  pred_db <- predict(rf_final_fit, new_data)
  # give probability
  pred_prob  <- predict(rf_final_fit, new_data, type = "prob")
  
  # what is output from function
  list(
    input = new_data,
    class = pred_db$.pred_class,
    probs = pred_prob
  )
  
}

# Run the API with this command
# pr("api.R") |> pr_run(port = 8385)
# to get links that match mine!

#query with http://127.0.0.1:8385/pred?BMI=29&Age=60-64&GenHlth=Good&HighBP=No%20high%20BP&HighChol=High%20cholesterol
#query with http://127.0.0.1:8385/pred?BMI=35&Age=45-49&GenHlth=Fair&HighBP=High%20BP&HighChol=High%20cholesterol
#query with http://127.0.0.1:8385/pred?Age=18-24&GenHlth=Excellent&HighBP=No%20high%20BP&HighChol=No%20high%20cholesterol


#info endpoint
#* @get /readme
function(){
  list(name="Elle Whitlock",
       link="")
}


#confusion endpoint
#* @serializer png
#* @get /confusion
function() {
  
  # predictions on entire data set used for fitting
  preds <- predict(rf_final_fit, db_data)
  
  # based on resource link provided
  cm <- conf_mat(
    # bind the raw data and predicted
    data = bind_cols(db_data, preds),
    # outcome we know
    truth = Diabetes_binary,
    # outcome we predicted (name of column in preds)
    estimate = .pred_class
  )
  
  # we need ggplot2
  p <- autoplot(cm)  
  print(p)           

}


