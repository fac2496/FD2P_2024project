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
            .modal-footer .btn {
                margin-right: 10px;
                padding: 5px 10px; /* Reduce button padding */
                font-size: 14px; /* Reduce font size */
            }
            .modal-footer {
                display: flex;
                justify-content: space-between;
            }
            .modal-dialog {
                max-width: 600px; /* Increase modal width */
            }
            .btn-remove {
                background-color: #dc3545 !important; /* Red */
                color: white !important;
                border: none !important;
                padding: 5px 10px !important; /* Smaller padding */
                font-size: 12px !important; /* Smaller font size */
                cursor: pointer !important;
            }
            .btn-remove:hover {
                background-color: #c82333 !important; /* Darker red on hover */
            }
        .shiny-input-container > label {
            font-style: italic;
            font-weight: normal;
            text-align: center;
            display: block;
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
                                   selectInput("age", "Age range to understand nutritional needs", choices = unique_ages, selected = "19 - 29"),
                                   h3("What is your gender?", class = "center-text"),
                                   div(class = "center-radio",
                                       div(class = "radio-label", style = "font-style: italic" ,"Gender to understand nutritional needs"),
                                       radioButtons("gender", label = NULL, choices = unique_genders, inline = TRUE)
                                   ),
                                   div(class = "center-button",
                                       actionButton("next_page", "Let's start your order!", class = "action-button")
                                   )
                               )
                        )
                      )
             ),
             tabPanel("Menu", value = "page2",
                      fluidRow(
                        h4("Understand how your meal contributes to your daily nutritional recommendation!", class = "center-text", style = "font-style: italic"),
                        column(12,
                               div(class = "legend",
                                   span(class = "green"), "≤ 50% of total daily nutrition",
                                   span(class = "yellow"), "51% - 100% of total daily nutrition",
                                   span(class = "red"), "> 100% of daily total daily nutrition"
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
                                            h3("Your Order"),
                                            uiOutput("basket_contents")  # Placeholder for basket contents
                                        )
                                 ),
                                 column(6,
                                        div(class = "progress-circle",
                                            h3("Total Basket as % of your daily nutrition"),
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
             
  ),
  # Modal pop-up
  tags$div(id = "warningModal", class = "modal", tabindex = "-1", role = "dialog",
           tags$div(class = "modal-dialog", role = "document",
                    tags$div(class = "modal-content",
                             tags$div(class = "modal-header",
                                      tags$h5(class = "modal-title", "Warning: Exceeding Daily Recommended Intake"),
                                      tags$button(type = "button", class = "close", `data-dismiss` = "modal", `aria-label` = "Close",
                                                  tags$span(`aria-hidden` = "true", "×")
                                      )
                             ),
                             tags$div(class = "modal-body",
                                      tags$p("Adding this item to the basket will exceed your daily recommended intake.")
                             ),
                             tags$div(class = "modal-footer",
                                      actionButton("suggest_healthier", "Suggest Healthier Alternative", class = "btn btn-success"),
                                      actionButton("remove_other_items", "Remove Other Items", class = "btn btn-warning"),
                                      actionButton("confirm_add", "Add to Basket", class = "btn btn-primary"),
                                      tags$button(type = "button", class = "btn btn-secondary", `data-dismiss` = "modal", "Close")
                             )
                    )
           )
  ),
  
  # Secondary pop-up for item removal
  tags$div(id = "removeItemsModal", class = "modal", tabindex = "-1", role = "dialog",
           tags$div(class = "modal-dialog", role = "document",
                    tags$div(class = "modal-content",
                             tags$div(class = "modal-header",
                                      tags$h5(class = "modal-title", "Remove Items from Basket"),
                                      tags$button(type = "button", class = "close", `data-dismiss` = "modal", `aria-label` = "Close",
                                                  tags$span(`aria-hidden` = "true", "×")
                                      )
                             ),
                             tags$div(class = "modal-body",
                                      tags$p("Select quantities to remove:"),
                                      uiOutput("basket_checklist")  # Placeholder for the checklist
                             ),
                             tags$div(class = "modal-footer",
                                      actionButton("confirm_removal", "Confirm Removal", class = "btn btn-primary"),
                                      tags$button(type = "button", class = "btn btn-secondary", `data-dismiss` = "modal", "Close")
                             )
                    )
           )
  ),
  
  # Healthier Alternatives Modal
  tags$div(id = "healthierAlternativesModal", class = "modal", tabindex = "-1", role = "dialog",
           tags$div(class = "modal-dialog", role = "document",
                    tags$div(class = "modal-content",
                             tags$div(class = "modal-header",
                                      tags$h5(class = "modal-title", "Healthier Alternatives"),
                                      tags$button(type = "button", class = "close", `data-dismiss` = "modal", `aria-label` = "Close",
                                                  tags$span(`aria-hidden` = "true", "×")
                                      )
                             ),
                             tags$div(class = "modal-body",
                                      uiOutput("healthier_alternatives")
                             ),
                             tags$div(class = "modal-footer",
                                      tags$button(type = "button", class = "btn btn-secondary", `data-dismiss` = "modal", "Close")
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
      
      get_color <- function(value, guideline_value) {
        if (value <= guideline_value * 0.5) {
          return("#28a745")  # Green
        } else if (value <= guideline_value) {
          return("#ffc107")  # Yellow
        } else {
          return("#dc3545")  # Red
        }
      }
      
      get_font_color <- function(bg_color) {
        if (bg_color == "#28a745" || bg_color == "#dc3545") {
          return("white")
        } else {
          return("black")
        }
      }
      
      div(
        h3(paste("What's in a", item$Item, "?"), class = "center-text"),
        div(class = "item-details",
            div(class = "detail-card", style = paste("background-color:", get_color(item$Calories, guideline$Calories), "; color:", get_font_color(get_color(item$Calories, guideline$Calories)), ";"),
                h4("Calories"),
                p(item$Calories, "kcal")
            ),
            div(class = "detail-card", style = paste("background-color:", get_color(item$Total.Fat..g., guideline$Total.Fat..g.), "; color:", get_font_color(get_color(item$Total.Fat..g., guideline$Total.Fat..g.)), ";"),
                h4("Total Fat"),
                p(paste(item$Total.Fat..g., "g"))
            ),
            div(class = "detail-card", style = paste("background-color:", get_color(item$Sugars..g., guideline$Sugars..g.), "; color:", get_font_color(get_color(item$Sugars..g., guideline$Sugars..g.)), ";"),
                h4("Sugars"),
                p(paste(item$Sugars..g., "g"))
            ),
            div(class = "detail-card", style = paste("background-color:", get_color(item$Salt.g, guideline$Salt.g), "; color:", get_font_color(get_color(item$Salt.g, guideline$Salt.g)), ";"),
                h4("Salt"),
                p(paste(item$Salt.g, "g"))
            )
        )
      )
    } else {
      h4("No details available for this item.")
    }
  })
  
  
  # Render the checklist of basket items with numeric inputs
  output$basket_checklist <- renderUI({
    basket_data <- basket()
    if (nrow(basket_data) > 0) {
      tags$ul(
        lapply(1:nrow(basket_data), function(i) {
          item <- basket_data[i, ]
          tags$li(
            paste(item$Item, "x", item$Quantity),
            numericInput(inputId = paste0("remove_qty_", item$Item), label = "Quantity to remove", value = 0, min = 0, max = item$Quantity)
          )
        })
      )
    } else {
      h4("Your basket is empty.")
    }
  })
  
  # Handle the confirmation of item removal
  observeEvent(input$confirm_removal, {
    basket_data <- basket()
    for (i in 1:nrow(basket_data)) {
      item <- basket_data[i, ]
      qty_to_remove <- input[[paste0("remove_qty_", item$Item)]]
      if (!is.null(qty_to_remove) && qty_to_remove > 0) {
        if (qty_to_remove >= item$Quantity) {
          basket_data <- basket_data[-i, ]
        } else {
          basket_data$Quantity[i] <- basket_data$Quantity[i] - qty_to_remove
        }
      }
    }
    basket(basket_data)
    shinyjs::runjs("$('#removeItemsModal').modal('hide')")
  })
  

  # Add item to basket with intake check
  observeEvent(input$add_to_basket, {
    req(input$item)
    item_data <- fastfood_data %>% filter(Item == input$item)
    guideline_data <- guidelines()
    
    if (nrow(item_data) > 0 && nrow(guideline_data) > 0) {
      item <- item_data[1, ]
      guideline <- guideline_data[1, ]
      
      exceeds_intake <- item$Calories > guideline$Calories ||
        item$Total.Fat..g. > guideline$Total.Fat..g. ||
        item$Sugars..g. > guideline$Sugars..g. ||
        item$Salt.g > guideline$Salt.g
      
      current_basket <- basket()
      total_calories <- sum(current_basket$Calories * current_basket$Quantity) + item$Calories
      total_fat <- sum(current_basket$Total.Fat..g. * current_basket$Quantity) + item$Total.Fat..g.
      total_sugars <- sum(current_basket$Sugars..g. * current_basket$Quantity) + item$Sugars..g.
      total_salt <- sum(current_basket$Salt.g * current_basket$Quantity) + item$Salt.g
      
      exceeds_cumulative_intake <- total_calories > guideline$Calories ||
        total_fat > guideline$Total.Fat..g. ||
        total_sugars > guideline$Sugars..g. ||
        total_salt > guideline$Salt.g
      
      if (exceeds_intake || exceeds_cumulative_intake) {
        shinyjs::runjs("$('#warningModal').modal('show')")
      } else {
        item_index <- which(current_basket$Item == item$Item)
        if (length(item_index) > 0) {
          current_basket$Quantity[item_index] <- current_basket$Quantity[item_index] + 1
        } else {
          new_item <- data.frame(Item = item$Item, Quantity = 1, Calories = item$Calories, Total.Fat..g. = item$Total.Fat..g., Sugars..g. = item$Sugars..g., Salt.g = item$Salt.g, stringsAsFactors = FALSE)
          current_basket <- rbind(current_basket, new_item)
        }
        basket(current_basket)
      }
    }
  })
  
  # Handle user choice to replace with a healthier alternative
  observeEvent(input$suggest_healthier, {
    req(input$item)
    item_data <- fastfood_data %>% filter(Item == input$item)
    guideline_data <- guidelines()
    
    if (nrow(item_data) > 0 && nrow(guideline_data) > 0) {
      item <- item_data[1, ]
      guideline <- guideline_data[1, ]
      
      # Find healthier alternatives
      healthier_alternatives <- fastfood_data %>%
        filter(Company == selected_restaurant(),
               Category == input$category,
               Calories <= guideline$Calories,
               Total.Fat..g. <= guideline$Total.Fat..g.,
               Sugars..g. <= guideline$Sugars..g.,
               Salt.g <= guideline$Salt.g) %>%
        arrange(Calories, Total.Fat..g., Sugars..g., Salt.g) %>%
        head(3)
      
      if (nrow(healthier_alternatives) > 0) {
        # Display healthier alternatives
        output$healthier_alternatives <- renderUI({
          tagList(
            h4("Healthier Alternatives:"),
            tags$table(class = "table table-striped",
                       tags$thead(
                         tags$tr(
                           tags$th("Item Name"),
                           tags$th("Calories"),
                           tags$th("Total Fat (g)"),
                           tags$th("Sugars (g)"),
                           tags$th("Salt (g)"),
                           tags$th("Actions")
                         )
                       ),
                       tags$tbody(
                         tags$tr(
                           tags$td(item$Item),
                           tags$td(item$Calories),
                           tags$td(item$Total.Fat..g.),
                           tags$td(item$Sugars..g.),
                           tags$td(item$Salt.g),
                           tags$td()
                         ),
                         lapply(1:nrow(healthier_alternatives), function(i) {
                           alt_item <- healthier_alternatives[i, ]
                           tags$tr(
                             tags$td(alt_item$Item),
                             tags$td(style = ifelse(alt_item$Calories < item$Calories, "color: green;", ""), alt_item$Calories),
                             tags$td(style = ifelse(alt_item$Total.Fat..g. < item$Total.Fat..g., "color: green;", ""), alt_item$Total.Fat..g.),
                             tags$td(style = ifelse(alt_item$Sugars..g. < item$Sugars..g., "color: green;", ""), alt_item$Sugars..g.),
                             tags$td(style = ifelse(alt_item$Salt.g < item$Salt.g, "color: green;", ""), alt_item$Salt.g),
                             tags$td(actionButton(inputId = paste0("add_", alt_item$Item), label = "Add to Basket", class = "btn btn-success btn-sm"))
                           )
                         })
                       )
            )
          )
        })
      } else {
        output$healthier_alternatives <- renderUI({
          h4("No healthier alternatives available.")
        })
      }
    }
    shinyjs::runjs("$('#warningModal').modal('hide')")
    shinyjs::runjs("$('#healthierAlternativesModal').modal('show')")
  })
  
  # Add healthier alternative to basket
  observe({
    lapply(1:nrow(fastfood_data), function(i) {
      alt_item <- fastfood_data[i, ]
      observeEvent(input[[paste0("add_", alt_item$Item)]], {
        current_basket <- basket()
        item_index <- which(current_basket$Item == alt_item$Item)
        if (length(item_index) > 0) {
          current_basket$Quantity[item_index] <- current_basket$Quantity[item_index] + 1
        } else {
          new_item <- data.frame(Item = alt_item$Item, Quantity = 1, Calories = alt_item$Calories, Total.Fat..g. = alt_item$Total.Fat..g., Sugars..g. = alt_item$Sugars..g., Salt.g = alt_item$Salt.g, stringsAsFactors = FALSE)
          current_basket <- rbind(current_basket, new_item)
        }
        basket(current_basket)
        shinyjs::runjs("$('#healthierAlternativesModal').modal('hide')")
      })
    })
  })
  
  # Handle user choice to remove the specific item
  observeEvent(input$remove_item, {
    shinyjs::runjs("$('#warningModal').modal('hide')")
  })
  
  # Handle user choice to remove other items
  observeEvent(input$remove_other_items, {
    # Show the secondary pop-up for removing items
    shinyjs::runjs("$('#removeItemsModal').modal('show')")
  })
  
  # Handle user choice to add to basket from modal
  observeEvent(input$confirm_add, {
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
    shinyjs::runjs("$('#warningModal').modal('hide')")
  })
  
  # Handle user choice to replace with a healthier alternative
  observeEvent(input$replace_item, {
    req(input$item)
    item_data <- fastfood_data %>% filter(Item == input$item)
    guideline_data <- guidelines()
    
    if (nrow(item_data) > 0 && nrow(guideline_data) > 0) {
      item <- item_data[1, ]
      guideline <- guideline_data[1, ]
      
      # Find a healthier alternative
      healthier_alternatives <- fastfood_data %>%
        filter(Company == selected_restaurant(),
               Category == input$category,
               Calories <= guideline$Calories,
               Total.Fat..g. <= guideline$Total.Fat..g.,
               Sugars..g. <= guideline$Sugars..g.,
               Salt.g <= guideline$Salt.g)
      
      if (nrow(healthier_alternatives) > 0) {
        healthier_item <- healthier_alternatives[1, ]
        
        # Add the healthier item to the basket
        current_basket <- basket()
        item_index <- which(current_basket$Item == healthier_item$Item)
        if (length(item_index) > 0) {
          current_basket$Quantity[item_index] <- current_basket$Quantity[item_index] + 1
        } else {
          new_item <- data.frame(Item = healthier_item$Item, Quantity = 1, Calories = healthier_item$Calories, Total.Fat..g. = healthier_item$Total.Fat..g., Sugars..g. = healthier_item$Sugars..g., Salt.g = healthier_item$Salt.g, stringsAsFactors = FALSE)
          current_basket <- rbind(current_basket, new_item)
        }
        basket(current_basket)
      }
    }
    shinyjs::runjs("$('#warningModal').modal('hide')")
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
            actionButton(
              inputId = paste0("remove_", item$Item),
              label = "Remove",
              style = "background-color: #dc3545; color: white; border: none; padding: 5px 10px; font-size: 12px; cursor: pointer;",
              class = "btn-remove",
              onclick = sprintf("Shiny.onInputChange('%s', '%s')", "remove_item", item$Item)
            )
          )
        })
      )
    } else {
      tags$h4(style = "font-style: italic; font-weight: normal; font-size: 14px;", "Your basket is empty.")
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
  
  # Function to generate SVG for progress circle with label
  generate_progress_circle <- function(percentage, color, label) {
    svg <- sprintf('
    <svg width="100" height="100">
      <circle cx="50" cy="50" r="45" stroke="%s" stroke-width="10" fill="none"
        stroke-dasharray="%f %f" transform="rotate(-90 50 50)" />
      <text x="50%%" y="40%%" text-anchor="middle" dy=".3em" font-size="15">%d%%</text>
      <text x="50%%" y="60%%" text-anchor="middle" dy=".3em" font-size="10">%s</text>
    </svg>
  ', color, percentage * 2.83, 283 - percentage * 2.83, round(percentage), label)
    return(svg)
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
      HTML(generate_progress_circle(percentage, color, "Calories"))
    } else {
      h4("")
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
      HTML(generate_progress_circle(percentage, color, "Fat"))
    } else {
      h4("")
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
      HTML(generate_progress_circle(percentage, color, "Sugars"))
    } else {
      h4("")
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
      HTML(generate_progress_circle(percentage, color, "Salt"))
    } else {
      h4("")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)