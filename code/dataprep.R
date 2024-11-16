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


##### DATASET 2: UK GOVERNMENT DIETARY RECOMMENCATIONS FOR ENERGY AND NUTRIENTS
### Source: https://assets.publishing.service.gov.uk/media/5a749fece5274a44083b82d8/government_dietary_recommendations.pdf

#Load
raw_guideline <- read.csv("raw_data/UK_Nutrition.csv")

#inspect
head(raw_guideline)
dim(raw_guideline) #19 rows, 13 columns
names(raw_guideline)
#[1]  "Age..years."                
#[2]  "Gender"                     
#[3]  "Energy..MJ.day."            
#[4]  "Energy..kcal.day."          
#[5]  "Protein..g.day."            
#[6]  "Fat..g.day."                
#[7]  "Saturated.fat..g.day."      
#[8]  "Polyunsaturated.fat..g.day."
#[9]  "Monounsaturated.fat..g.day."
#[10] "Carbohydrate..g.day."       
#[11] "Free.sugars..g.day."        
#[12] "Salt..g.day."               
#[13] "Fibre..g.day.."

str(raw_guideline) #mix of chr, num, int. 

#TODO: Switch 11/13 variables into numeric types

raw_guideline <- raw_guideline %>%
  mutate(across(-c(Age..years., Gender), ~as.numeric(replace_na(.,0))))

#check
str(raw_guideline) #all numeric except for age groups and Gender
sapply(raw_guideline, function(x) sum(is.na(x))) #no NAs
