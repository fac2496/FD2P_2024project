library(shiny)
library(dplyr)
library(shinyjs)

# Read the data
fastfood_data <- read.csv("/Users/felixculas/shiny/fd2p2024/processed_data/fastfood_categoryset.csv", header = TRUE)
uk_guidelines <- read.csv("/Users/felixculas/shiny/fd2p2024/processed_data/guidelines_workingset.csv", header = TRUE)

# Get unique company names
unique_companies <- unique(fastfood_data$Company)

# Get unique age ranges and genders
unique_ages <- unique(uk_guidelines$Age.Range)
unique_genders <- unique(uk_guidelines$Gender)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .center-text {
        text-align: center;
      }
      .center-content {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
      }
      .center-radio {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
      }
      .radio-label {
        text-align: center;
        width: 100%;
      }
      .center-button {
        display: flex;
        justify-content: center;
        margin-top: 20px;
      }
      .action-button {
        background-color: #28a745; /* Bright green */
        color: white;
        border: none;
        padding: 10px 20px;
        font-size: 16px;
        cursor: pointer;
      }
      .action-button:hover {
        background-color: #218838; /* Darker green on hover */
      }
      .tile {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      .item-details {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        margin-top: 20px;
      }
      .detail-card {
        background-color: #ffffff;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        flex: 1 1 calc(25% - 20px);
        text-align: center;
      }
      .detail-card h4 {
        margin: 10px 0;
      }
      .detail-card p {
        margin: 0;
      }
      .legend {
        background-color: #f8f9fa;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 20px;
        text-align: center;
      }
      .legend span {
        display: inline-block;
        width: 20px;
        height: 20px;
        margin-right: 5px;
        border-radius: 3px;
      }
      .legend .green {
        background-color: #28a745;
      }
      .legend .yellow {
        background-color: #ffc107;
      }
      .legend .red {
        background-color: #dc3545;
      }
      .basket {
        margin-top: 20px;
      }
      .basket-item {
        padding: 10px;
        border-bottom: 1px solid #ddd;
      }
    "))
  ),
  div(style = "text-align: center;",
      uiOutput("dynamic_logo"),  # Placeholder for dynamic logo
      h1(class = "serif-font", "Better Bites: Know Your Food")
  ),
  navbarPage(id = "navbar", "",
             tabPanel("Welcome Page", value = "welcome",
                      fluidRow(
                        column(12,
                               div(class = "center-content",
                                   h3("What are you craving today?", class = "center-text"),
                                   selectInput("company", "Pick a restaurant to order from:", choices = unique_companies),
                                   h3("How old are you?", class = "center-text"),
                                   selectInput("age", "To understand your nutritional requirements", choices = unique_ages),
                                   h3("What is your gender?", class = "center-text"),
                                   div(class = "center-radio",
                                       div(class = "radio-label", "To understand your nutritional requirements"),
                                       radioButtons("gender", label = NULL, choices = unique_genders, inline = TRUE)
                                   ),
                                   div(class = "center-button",
                                       actionButton("next_page", "Let's get started!", class = "action-button")
                                   )
                               )
                        )
                      )
             ),
             tabPanel("Page 2", value = "page2",
                      fluidRow(
                        column(12,
                               div(class = "legend",
                                   span(class = "green"), "≤ 50% of daily recommendation",
                                   span(class = "yellow"), "51% - 100% of daily recommendation",
                                   span(class = "red"), "> 100% of daily recommendation"
                               )
                        )
                      ),
                      fluidRow(
                        column(3,
                               div(class = "tile",
                                   h3(uiOutput("dynamic_header"), class = "center-text"),
                                   selectInput("category", "Select a category:", choices = NULL),  # Placeholder for dynamic categories
                                   selectInput("item", "Select an item:", choices = NULL),  # Placeholder for dynamic items
                                   div(class = "center-button",
                                       actionButton("add_to_basket", "Add to Basket", class = "action-button")
                                   )
                               )
                        ),
                        column(9,
                               uiOutput("item_details"),  # Placeholder for item details
                               div(class = "basket",
                                   h3("Basket"),
                                   uiOutput("basket_contents")  # Placeholder for basket contents
                               )
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the selected restaurant
  selected_restaurant <- reactiveVal()
  
  # Reactive value to store the selected age and gender
  selected_age <- reactiveVal()
  selected_gender <- reactiveVal()
  
  # Reactive value to track if the button has been clicked
  button_clicked <- reactiveVal(FALSE)
  
  # Reactive value to store the basket contents
  basket <- reactiveVal(data.frame(Item = character(), Calories = numeric(), Total.Fat..g. = numeric(), Sugars..g. = numeric(), Salt.g = numeric(), stringsAsFactors = FALSE))
  
  # Dynamic logo rendering
  output$dynamic_logo <- renderUI({
    req(input$company)  # Ensure a company is selected
    
    # Map company to logo file
    logo_file <- switch(input$company,
                        "McDonald’s" = "mcdonalds_logo.jpg",
                        "Burger King" = "burgerking_logo.jpg",
                        "KFC" = "kfc_logo.jpg",
                        "Pizza Hut" = "pizzahut_logo.jpg",
                        "Taco Bell" = "tacobell_logo.jpg",
                        "Wendy’s" = "wendys_logo.jpg",
                        NULL)  # Default if no match
    
    # Render the logo dynamically
    if (!is.null(logo_file)) {
      img(src = logo_file, height = "75px", style = "margin-bottom: 10px;")
    }
  })
  
  # Disable Page 2 tab initially
  observe({
    shinyjs::disable(selector = "a[data-value='page2']")
  })
  
  # Store the selected restaurant, age, and gender, and navigate to the next page when the button is clicked
  observeEvent(input$next_page, {
    selected_restaurant(input$company)
    selected_age(input$age)
    selected_gender(input$gender)
    button_clicked(TRUE)
    shinyjs::enable(selector = "a[data-value='page2']")
    updateNavbarPage(session, "navbar", selected = "page2")
  })
  
  # Update category choices based on selected restaurant
  observe({
    req(selected_restaurant())
    categories <- unique(fastfood_data %>% filter(Company == selected_restaurant()) %>% pull(Category))
    updateSelectInput(session, "category", choices = categories)
  })
  
  # Update item choices based on selected category
  observe({
    req(input$category)
    items <- unique(fastfood_data %>% filter(Company == selected_restaurant(), Category == input$category) %>% pull(Item))
    updateSelectInput(session, "item", choices = items)
  })
  
  # Render the dynamic header
  output$dynamic_header <- renderUI({
    req(selected_restaurant())
    restaurant <- selected_restaurant()
    paste("What are you craving from", restaurant, "?")
  })
  
  # Filter the guidelines data based on selected age and gender
  guidelines <- reactive({
    req(selected_age(), selected_gender())
    uk_guidelines %>% filter(Age.Range == selected_age(), Gender == selected_gender())
  })
  
  # Render the item details with conditional formatting
  output$item_details <- renderUI({
    req(input$item, guidelines())
    item_data <- fastfood_data %>% filter(Item == input$item)
    guideline_data <- guidelines()
    
    if (nrow(item_data) > 0 && nrow(guideline_data) > 0) {
      item <- item_data[1, ]
      guideline <- guideline_data[1, ]
      
      # Function to determine the color based on the value and guideline
      get_color <- function(value, guideline_value) {
        if (value <= guideline_value * 0.5) {
          return("#28a745")  # Green for low
        } else if (value <= guideline_value) {
          return("#ffc107")  # Yellow for moderate
        } else {
          return("#dc3545")  # Red for high
        }
      }
      
      div(class = "item-details",
          div(class = "detail-card", style = paste("background-color:", get_color(item$Calories, guideline$Calories)),
              h4("Calories"),
              p(item$Calories)
          ),
          div(class = "detail-card", style = paste("background-color:", get_color(item$Total.Fat..g., guideline$Total.Fat..g.)),
              h4("Total Fat"),
              p(paste(item$Total.Fat..g., "g"))
          ),
          div(class = "detail-card", style = paste("background-color:", get_color(item$Sugars..g., guideline$Sugars..g.)),
              h4("Sugars"),
              p(paste(item$Sugars..g., "g"))
          ),
          div(class = "detail-card", style = paste("background-color:", get_color(item$Salt.g, guideline$Salt.g)),
              h4("Salt"),
              p(paste(item$Salt.g, "g"))
          ),
          div(class = "center-button",
              actionButton("add_to_basket", "Add to Basket", class = "action-button")
          )
      )
    } else {
      h4("No details available for this item.")
    }
  })
  
  # Add item to basket
  observeEvent(input$add_to_basket, {
    req(input$item)
    item_data <- fastfood_data %>% filter(Item == input$item)
    if (nrow(item_data) > 0) {
      item <- item_data[1, ]
      current_basket <- basket()
      new_basket <- rbind(current_basket, item)
      basket(new_basket)
    }
  })
  
  # Render the basket contents
  output$basket_contents <- renderUI({
    basket_data <- basket()
    if (nrow(basket_data) > 0) {
      lapply(1:nrow(basket_data), function(i) {
        item <- basket_data[i, ]
        div(class = "basket-item",
            h4(item$Item),
            p(paste("Calories:", item$Calories)),
            p(paste("Total Fat:", item$Total.Fat..g., "g")),
            p(paste("Sugars:", item$Sugars..g., "g")),
            p(paste("Salt:", item$Salt.g, "g"))
        )
      })
    } else {
      h4("Your basket is empty.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)