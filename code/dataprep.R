getwd()


##### DATASET 1: FAST FOOD CALORIC AND NUTRITIONAL INFO
###Source: https://www.kaggle.com/datasets/joebeachcapital/fast-food

#Load
raw_fastfood <- read.csv("raw_data/FastFoodNutritionMenu_kaggle.csv")

#inspect
head(raw_fastfood)
dim(raw_fastfood)#1147 rows, 14 columns
names(raw_fastfood)
#[1] "Company"              "Item"                
#[3] "Calories"             "Calories.from.Fat"   
#[5] "Total.Fat..g."        "Saturated.Fat..g."   
#[7] "Trans.Fat..g."        "Cholesterol..mg."    
#[9] "Sodium...mg."         "Carbs..g."           
#[11] "Fiber..g."            "Sugars..g."          
#[13] "Protein..g."          "Weight.Watchers.Pnts"

str(raw_fastfood) #all are characters. 
#TODO: Switch 12/14 from char to numeric types (except Company and Item)

library(dplyr)
library(tidyr)
raw_fastfood <- raw_fastfood %>%
  mutate(across(-c(Company, Item), ~as.numeric(replace_na(.,0))))

#check
str(raw_fastfood) #all numeric except Company, Item
sapply(raw_fastfood, function(x) sum(is.na(x))) #no NA values

 