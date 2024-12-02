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
    ")),
    shinyjs::extendShinyjs(
      text = "
      shinyjs.resetRemoveItem = function() {
          Shiny.onInputChange('remove_item', null);
      }
      ",
      functions = c("resetRemoveItem")
    )
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
                               fluidRow(
                                 column(6,
                                        div(class = "basket",
                                            h3("Basket"),
                                            uiOutput("basket_contents")  # Placeholder for basket contents
                                        )
                                 ),
                                 column(6,
                                        div(class = "progress-circle",
                                            h3("Total Basket as % of recommended daily intake"),
                                            fluidRow(
                                              column(6, uiOutput("total_calories")),
                                              column(6, uiOutput("total_fat"))
                                            ),
                                            fluidRow(
                                              column(6, uiOutput("total_sugars")),
                                              column(6, uiOutput("total_salt"))
                                            )
                                        )
                                 )
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
          )
      )
    } else {
      h4("No details available for this item.")
    }
  })
  
  # Initialize basket with quantity column
  basket <- reactiveVal(data.frame(Item = character(), Quantity = numeric(), Calories = numeric(), Total.Fat..g. = numeric(), Sugars..g. = numeric(), Salt.g = numeric(), stringsAsFactors = FALSE))
  
  # Add item to basket
  observeEvent(input$add_to_basket, {
    req(input$item)
    item_data <- fastfood_data %>% filter(Item == input$item)
    if (nrow(item_data) > 0) {
      item <- item_data[1, ]
      current_basket <- basket()
      item_index <- which(current_basket$Item == item$Item)
      if (length(item_index) > 0) {
        current_basket$Quantity[item_index] <- current_basket$Quantity[item_index] + 1
      } else {
        new_item <- data.frame(Item = item$Item, Quantity = 1, Calories = item$Calories, Total.Fat..g. = item$Total.Fat..g., Sugars..g. = item$Sugars..g., Salt.g = item$Salt.g, stringsAsFactors = FALSE)
        current_basket <- rbind(current_basket, new_item)
      }
      basket(current_basket)
    }
  })
  
  # Remove item from basket
  observeEvent(input$remove_item, {
    req(input$remove_item)
    current_basket <- basket()
    item_index <- which(current_basket$Item == input$remove_item)
    if (length(item_index) > 0) {
      if (current_basket$Quantity[item_index] > 1) {
        current_basket$Quantity[item_index] <- current_basket$Quantity[item_index] - 1
      } else {
        current_basket <- current_basket[-item_index, ]
      }
      basket(current_basket)
      # Reset the input value to allow for subsequent clicks
      shinyjs::js$resetRemoveItem()
    }
  })
  
  
  # Render the basket contents (list of item names and quantities)
  output$basket_contents <- renderUI({
    basket_data <- basket()
    if (nrow(basket_data) > 0) {
      tags$ul(
        lapply(1:nrow(basket_data), function(i) {
          item <- basket_data[i, ]
          tags$li(
            paste(item$Item, "x", item$Quantity),
            actionButton(inputId = paste0("remove_", item$Item), label = "Remove", class = "btn btn-danger btn-sm", onclick = sprintf("Shiny.onInputChange('%s', '%s')", "remove_item", item$Item))
          )
        })
      )
    } else {
      h4("Your basket is empty.")
    }
  })
  
  # Function to get color based on percentage
  get_color <- function(percentage) {
    if (percentage <= 50) {
      return("#28a745")  # Green
    } else if (percentage <= 100) {
      return("#ffc107")  # Yellow
    } else {
      return("#dc3545")  # Red
    }
  }
  
  # Calculate and render the cumulative total with conditional formatting
  output$total_calories <- renderUI({
    basket_data <- basket()
    if (nrow(basket_data) > 0) {
      guideline <- guidelines()
      total_calories <- sum(basket_data$Calories * basket_data$Quantity)
      recommended_calories <- guideline$Calories[1]
      percentage <- (total_calories / recommended_calories) * 100
      color <- get_color(percentage)
      div(style = "text-align: center;",
          h4("Total Calories"),
          tags$svg(width = "100", height = "100",
                   tags$circle(cx = "50", cy = "50", r = "45", stroke = color, "stroke-width" = "10", fill = "none"),
                   tags$text(x = "50%", y = "50%", "text-anchor" = "middle", dy = ".3em", paste0(round(percentage), "%"))
          )
      )
    } else {
      h4("No items in the basket.")
    }
  })
  
  output$total_fat <- renderUI({
    basket_data <- basket()
    if (nrow(basket_data) > 0) {
      guideline <- guidelines()
      total_fat <- sum(basket_data$Total.Fat..g. * basket_data$Quantity)
      recommended_fat <- guideline$Total.Fat..g.[1]
      percentage <- (total_fat / recommended_fat) * 100
      color <- get_color(percentage)
      div(style = "text-align: center;",
          h4("Total Fat"),
          tags$svg(width = "100", height = "100",
                   tags$circle(cx = "50", cy = "50", r = "45", stroke = color, "stroke-width" = "10", fill = "none"),
                   tags$text(x = "50%", y = "50%", "text-anchor" = "middle", dy = ".3em", paste0(round(percentage), "%"))
          )
      )
    } else {
      h4("No items in the basket.")
    }
  })
  
  output$total_sugars <- renderUI({
    basket_data <- basket()
    if (nrow(basket_data) > 0) {
      guideline <- guidelines()
      total_sugars <- sum(basket_data$Sugars..g. * basket_data$Quantity)
      recommended_sugars <- guideline$Sugars..g.[1]
      percentage <- (total_sugars / recommended_sugars) * 100
      color <- get_color(percentage)
      div(style = "text-align: center;",
          h4("Total Sugars"),
          tags$svg(width = "100", height = "100",
                   tags$circle(cx = "50", cy = "50", r = "45", stroke = color, "stroke-width" = "10", fill = "none"),
                   tags$text(x = "50%", y = "50%", "text-anchor" = "middle", dy = ".3em", paste0(round(percentage), "%"))
          )
      )
    } else {
      h4("No items in the basket.")
    }
  })
  
  output$total_salt <- renderUI({
    basket_data <- basket()
    if (nrow(basket_data) > 0) {
      guideline <- guidelines()
      total_salt <- sum(basket_data$Salt.g * basket_data$Quantity)
      recommended_salt <- guideline$Salt.g[1]
      percentage <- (total_salt / recommended_salt) * 100
      color <- get_color(percentage)
      div(style = "text-align: center;",
          h4("Total Salt"),
          tags$svg(width = "100", height = "100",
                   tags$circle(cx = "50", cy = "50", r = "45", stroke = color, "stroke-width" = "10", fill = "none"),
                   tags$text(x = "50%", y = "50%", "text-anchor" = "middle", dy = ".3em", paste0(round(percentage), "%"))
          )
      )
    } else {
      h4("No items in the basket.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)