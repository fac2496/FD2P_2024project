# Step 1: Read the original dataset
original_menu_data <- read.csv("processed_data/fastfood_workingset.csv", header = TRUE)

# Step 2: Create a copy of the dataset
test_menu_data <- original_menu_data

# Step 3: Define keywords for each category
categories <- list(
  Breakfast = c("oatmeal", "breakfast", "McMuffin", "biscuit", "McGriddles", "bagel"),
  Burgers = c("burger", "hamburger", "cheeseburger", "McDouble", "Quarter Pounder", "Big Mac", "Big N' Tasty ", "McChicken", "McRib", "Stacker King", "Chicken Jr.", "Baconator", "Dave's", "Double Stack", "BLT", "Tasty®", "Angus", "Dave’s Single", "Dave’s Double", "Dave’s Triple", "o-fish"),
  Burrito = c("burrito"),
  Chalupa = c("chalupa"),
  Chicken = c("breast", "wings", "drumstick", "thigh", "wing", "tender"),
  Coffee_drinks = c("Cappuccino", "Latte", "Mocha", "Coffee", "frappe"),
  Condiments = c("Dressing", "Ketchup", "salt packets", "barbecue sauce", "dipping sauce", "honey", "sauce", "jam", "preserves", "sugar packet", "jelly packet", "spread", "salt packet", "Mayonnaise", "Vinaigrette", "Margarine", "sweetener", "american cheese"),
  Desserts = c("ice cream", "cake", "pie", "Sundae", "McFlurry", "Cookie", "frosty", "cinnamon twist", "Cinnamon", "soft serve", "Cinnabon", "Cornbread muffin", "muffin", "cone", "parfait", "caramel dip"),
  Gordita = c("gordita"),
  Juice = c("juice", "lavaburst", "fruit punch", "fruit cup", "yumberry"),
  Milk = c("milk"),
  Nachos = c("nacho"),
  Pizza = c("pizza", "Hand Tossed"),
  Quesadilla = c("quesadilla"),
  Quesarito = ("quesarito"),
  Salads = c("salad"),
  Sandwiches = c("sandwich", "sub", "Club", "CROISSAN’WICH®", "chipotle melt"),
  Shakes = c("shake"),
  Sides = c("fries", "onion rings", "nuggets", "Hash Brown", "potatoes", "corn", "Potato", "Apple Bites", "crouton", "chilli", "gravy and sausage", "macaroni &", "littles", "turnover", "BBQ baked", "famous bowl", "crispy twister", "gizzards", "livers", "roll up", "power menu", "chili", "black beans"),
  Steak = c("steak"),
  Sweet_Drinks = c("Coca-cola", "coke", "sprite", "mountain", "Fanta", "Dr Pepper", "Lemonade", "7UP", "Mist Twst", "Pepsi", "Miranda", "Root Beer", "Tropicana", "Coca cola", "Dr. Pepper", "limeade", "berry freeze", "mtn dew", "smoothie", "7-UP", "hot chocolate", "manzanita", "mnt dew", "sierra", "fruit freeze"),
  Taco = c ("taco"),
  Tea = c("tea"),
  Veggie_sides = c("coleslaw", "corn on the cob", "green beans"),
  Water = c("water"),
  Wraps = c("wrap")
)

# Step 4: Function to categorize items based on keywords (case insensitive)
categorize_item <- function(item_name) {
  for (category in names(categories)) {
    if (any(sapply(categories[[category]], grepl, item_name, ignore.case = TRUE))) {
      return(category)
    }
  }
  return("Other")
}

# Step 5: Apply categorization to the copied dataset
test_menu_data$Category <- sapply(test_menu_data$Item, categorize_item)

# Step 6: Verify the results
head(test_menu_data)

# Step 7: Inspect 'Others' and add categorisation labels
other_items <- test_menu_data %>% filter(Category == "Other")

View(other_items) 
# 1st attempt : 867 entries unclassified
  # To add:
  ## Dessert    :  "Sundae", "McFlurry", "Cookie"
  ## Sweet_drinks:  "Coca-cola", "coke", "sprite", "mountain"

# 2nd attempt: 804 entries unclassified
  # To add: 
  ## Coffee_drinks: "Cappuccino", "Latte", "Mocha", "Coffee"
  ## Wraps: "wrap"
  ## Shakes: "shake"
  ## Condiments: "Dressing", "Ketchup"
  ## Sweet_drinks: "Fanta", "Dr Pepper, "Lemonade", "7UP", "Mist Twst", "Pepsi", "Miranda", "Root Beer", "Tropicana"

# 3rd attempt: 594 entries unclassified
  # To add: 
  ## Tea: "tea"
  ## Sweet_drinks: "Coca cola", "Dr. Pepper", "limeade", "berry freeze", "mtn dew"
  ## Dessert: "frosty", "cinnamon twist" 
  ## Veggie_sides: "coleslaw", "corn on the cob", "green beans"
  ## Pizza: "pizza"

# 4th attempt: 437 entries unclassified
  # To add: 
  ## Condiments:  "salt packets", "barbecue sauce", "dipping sauce", "honey", "sauce", "jam", "preserves", "sugar packet"
  ## Steak: "steak"
  ## Chicken: "breast", "wings", "drumstick", "thigh"
  ## Burgers: "McDouble", "Quarter Pounder", "Big Mac", "Big N' Tasty ", "McChicken", "McRib", "Stacker King", "Chicken Jr."
  ## Sides: "Hash Brown", "potatoes", 
  ## Desserts: "Cinnamon", "soft serve"
  ## Sweet_drinks: "smoothie", "7-UP"
  ## Breakfast: "oatmeal", "breakfast"
  ## Coffee_drinks: "frappe"
  ## Taco: "taco"
  ## Nachos" "nacho"
  ## Pizza: "Hand Tossed"


# 5th attempt: 258 entries unclassified
# To add: 
  ## Breakfast: "McMuffin", "biscuit"
  ## Chicken: "wing", "tender"
  ## Sides: "corn"
  ## Condiments: "jelly packet", "spread", "salt packet", "Mayonnaise"
  ## Desserts: "Cinnabon", "Cornbread muffin"
  ## Juice: "juice"
  ## Burger: "Baconator", "Dave's", "Double Stack", "BLT"
  ## Sides: "Potato", "Apple Bites", "chilli" 
  ## Milk: "milk"

# 6th attempt: 178 entries unclassified
# To add: 
  ## Breakfast: "McGriddles", "bagel"
  ## Burgers: "Tasty®", "Angus", "Dave's Single", "Dave's Double", "Dave's Triple"
  ## Condiments: "Vinaigrette", "Margarine"
  ## Burritos: "burrito" 
  ## Dessert: "parfait", "caramel dip"
  ## Juice: "lavaburst", "fruit punch", "fruit cup"
  ## Sides: "crouton", "chilli", "gravy and sausage", "macaroni &", "littles", "turnover"
  ## Sandwich: "Club", "CROISSAN’WICH®"


#7th attempt: 112 entries unclassified
# To add:
  ## Burger: "o-fish"
  ## condiments: "sweetener", "american cheese"
  ## sweet_drink: "hot chocolate", "manzanita", "mnt dew", "sierra", "fruit freeze"
  ## sides: "BBQ baked", "famous bowl", "crispy twister", "gizzards", "livers", "roll up", "power menu", "black beans"
  ## juice: "yumberry"
  ## Chalupa: "chalupa"
  ## gordita: "gordita"
  ## quesarito: "quesarito"
  ## quesadilla: "quesadilla
  ## Water: "water"
  ## Sandwich: "chipotle melt"

# After 8 attempts, 44 items entries left unclassified

getwd()


setwd("processed_data")
getwd()
write.csv(test_menu_data, "fastfood_categoryset.csv", row.names = FALSE)


updated_menu_data <- read.csv("fastfood_categoryset.csv")

View(updated_menu_data[updated_menu_data$Category == "Other", ])
