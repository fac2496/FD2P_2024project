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


##### COMPARE DATASET 1 & 2 FOR COMMON VARIABLES

# Dataset 1 variables
#[1] "Company"              "Item"                
#[3] "Calories"             "Calories.from.Fat"   
#[5] "Total.Fat..g."        "Saturated.Fat..g."   
#[7] "Trans.Fat..g."        "Cholesterol..mg."    
#[9] "Sodium...mg."         "Carbs..g."           
#[11] "Fiber..g."            "Sugars..g."          
#[13] "Protein..g."          "Weight.Watchers.Pnts"

# Dataset 2 variables
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

# Similar variables (dataset 1/ dataset 2)
# "Calories"/ Energy..kcal.day 
# "Total.Fat..g."/ "Fat..g.day."
# "Saturated.Fat..g."/ "Saturated.fat..g.day."
# "Sodium...mg."/ "Salt..g.day."              --> to check 
# "Carbs..g."/ "Carbohydrate..g.day."
# "Fiber..g."/ "Fibre..g.day.."
# "Sugars..g."/ "Free.sugars..g.day."         --> to check
# "Protein..g."/ "Protein..g.day."


## Salt is sodium chloride, so it is not a like for like comparison 
## 1g of salt is 400 mg of sodium (source: https://www.heartfoundation.org.au/sodium-and-salt-converter)

#Create new column for salt in Dataset 1 (fast food)
raw_fastfood$Salt.g <- 
  raw_fastfood$Sodium...mg./400
#check
head(raw_fastfood[, c("Sodium...mg.", "Salt.g")])

## Free sugars are a subset of sugars. 
## Source: https://www.nutrition.org.uk/nutritional-information/sugar/#:~:text=Sugars%20can%20be%20naturally%20occurring,drinks%20with%20'free%20sugars'.
summary(raw_fastfood$Sugars..g.)
summary(raw_guideline$Free.sugars..g.day.)

# As fast food generally contains excessive amounts of sugar (fizzy drinks, desserts), it makes sense that the mean and median of Sugars in the fast food dataset is higher than the daily recommended intake


##### Prepare working dataset from Dataset 1

names(raw_fastfood)
#[1] "Company"              "Item"                
#[3] "Calories"             "Calories.from.Fat"   
#[5] "Total.Fat..g."        "Saturated.Fat..g."   
#[7] "Trans.Fat..g."        "Cholesterol..mg."    
#[9] "Sodium...mg."         "Carbs..g."           
#[11] "Fiber..g."           "Sugars..g."          
#[13] "Protein..g."         "Weight.Watchers.Pnts"
#[15] "Salt.g"    

#relevant variables:
# "Company"              "Item"                
# "Calories"                
# "Total.Fat..g."        "Saturated.Fat..g."   
# "Carbs..g."           
# "Fiber..g."            "Sugars..g."          
# "Protein..g."          
# "Salt.g"    

#create working dataset for fast food data
fastfood_workingset <- raw_fastfood %>%
  select(Company, Item, Calories, Total.Fat..g., Saturated.Fat..g., Carbs..g., Fiber..g., Sugars..g., Protein..g., Salt.g)
#check
head(fastfood_workingset)

#save working set in 'processed data' folder
getwd()
path_to_save <- "/Users/felixculas/shiny/fd2p2024/processed_data/fastfood_workingset.csv"
write.csv(fastfood_workingset, path_to_save, row.names = FALSE)

##### Prepare working dataset from Dataset 2

names(raw_guideline)
#[1] "Age..years."                 "Gender"                     
#[3] "Energy..MJ.day."             "Energy..kcal.day."          
#[5] "Protein..g.day."             "Fat..g.day."                
#[7] "Saturated.fat..g.day."       "Polyunsaturated.fat..g.day."
#[9] "Monounsaturated.fat..g.day." "Carbohydrate..g.day."       
#[11] "Free.sugars..g.day."         "Salt..g.day."               
#[13] "Fibre..g.day.." 

#relevant variables:
# "Age..years."                 "Gender"                     
# "Energy..kcal.day."          
# "Protein..g.day."             "Fat..g.day."                
# "Saturated.fat..g.day."       
# "Carbohydrate..g.day."       
# "Free.sugars..g.day."         "Salt..g.day."               
# "Fibre..g.day.." 

# Would prefer to standardise the names of the common variables between the two datasets.
#TODO: Convert the names of common variables in Dataset 2 to the names in Dataset 1

guideline_workingset <- raw_guideline %>%
  select(Age..years., Gender, Energy..kcal.day., Protein..g.day., Fat..g.day., Saturated.fat..g.day., Carbohydrate..g.day., Free.sugars..g.day., Salt..g.day., Fibre..g.day..)

#Reference
# "Calories"/ Energy..kcal.day 
# "Total.Fat..g."/ "Fat..g.day."
# "Saturated.Fat..g."/ "Saturated.fat..g.day."
# "Sodium...mg."/ "Salt..g.day."              --> to check 
# "Carbs..g."/ "Carbohydrate..g.day."
# "Fiber..g."/ "Fibre..g.day.."
# "Sugars..g."/ "Free.sugars..g.day."         --> to check
# "Protein..g."/ "Protein..g.day."


rename_vector <- c(
  "Age..years." = "Age.Range", 
  "Gender" = "Gender",
  "Energy..kcal.day" = "Calories", 
  "Fat..g.day." = "Total.Fat..g.",
  "Saturated.fat..g.day." = "Saturated.Fat..g.",
  "Salt..g.day." = "Salt.g",
  "Carbohydrate..g.day." = "Carbs..g.",
  "Fibre..g.day.." = "Fiber..g.",
  "Free.sugars..g.day." = "Sugars..g.",
  "Protein..g.day." = "Protein..g."
)

guideline_workingset <- guideline_workingset %>%
  rename("Age.Range" = "Age..years.", 
         "Gender" = "Gender",
         "Calories" = "Energy..kcal.day.", 
         "Total.Fat..g." = "Fat..g.day." ,
         "Saturated.Fat..g." = "Saturated.fat..g.day.",
         "Salt.g" = "Salt..g.day.",
         "Carbs..g." = "Carbohydrate..g.day.",
         "Fiber..g." = "Fibre..g.day..",
         "Sugars..g." = "Free.sugars..g.day.",
         "Protein..g." ="Protein..g.day.")

#check
colnames(guideline_workingset)
