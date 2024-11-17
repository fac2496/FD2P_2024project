library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("FD2P Data Product"),
  sidebarLayout(
    sidebarPanel(
      selectInput("age", "Choose your age:", choices = NULL, selected = NULL),
      selectInput("gender", "Choose your gender:", choices = NULL, selected = NULL),
      selectInput("restaurant", "Choose a restaurant:", choices = NULL, selected = NULL),
      selectizeInput("item", "Choose an item:", choices = NULL, options = list(placeholder = 'Please scroll or type to select an item')),
      actionButton("add_to_basket", "Add to Basket"),
      h3("Basket"),
      uiOutput("basket_ui")
    ),
    mainPanel(
      actionButton("toggle_view", "Toggle View"),
      conditionalPanel(
        condition = "input.toggle_view % 2 == 0",
        fluidRow(
          column(3, plotOutput("caloriesPlot")),
          column(3, plotOutput("fatPlot")),
          column(3, plotOutput("saturatedFatPlot")),
          column(3, plotOutput("saltPlot"))
        ),
        fluidRow(
          column(3, plotOutput("carbsPlot")),
          column(3, plotOutput("fiberPlot")),
          column(3, plotOutput("sugarsPlot")),
          column(3, plotOutput("proteinPlot"))
        )
      ),
      conditionalPanel(
        condition = "input.toggle_view % 2 == 1",
        h3("Cumulative Nutritional Values"),
        fluidRow(
          column(3, plotOutput("cumulativeCaloriesPlot")),
          column(3, plotOutput("cumulativeFatPlot")),
          column(3, plotOutput("cumulativeSaturatedFatPlot")),
          column(3, plotOutput("cumulativeSaltPlot"))
        ),
        fluidRow(
          column(3, plotOutput("cumulativeCarbsPlot")),
          column(3, plotOutput("cumulativeFiberPlot")),
          column(3, plotOutput("cumulativeSugarsPlot")),
          column(3, plotOutput("cumulativeProteinPlot"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  menu_data <- read.csv("/Users/felixculas/shiny/fd2p2024/processed_data/fastfood_workingset.csv", header = TRUE)
  guideline_data <- read.csv("/Users/felixculas/shiny/fd2p2024/processed_data/guidelines_workingset.csv", header = TRUE)
  
  unique_ages <- unique(guideline_data$Age.Range)
  unique_genders <- unique(guideline_data$Gender)
  
  updateSelectInput(session, "age", choices = unique_ages, selected = "19-64")
  updateSelectInput(session, "gender", choices = unique_genders, selected = NULL)
  
  unique_restaurants <- unique(menu_data$Company)
  
  updateSelectInput(session, "restaurant", choices = unique_restaurants, selected = NULL)
  
  observeEvent(input$restaurant, {
    selected_restaurant <- input$restaurant
    if (!is.null(selected_restaurant) && selected_restaurant != "") {
      filtered_items <- menu_data$Item[menu_data$Company == selected_restaurant]
      updateSelectizeInput(session, "item", choices = filtered_items, selected = NULL, server = TRUE)
    } else {
      updateSelectizeInput(session, "item", choices = NULL, server = TRUE)
    }
  })
  
  basket <- reactiveVal(data.frame(Item = character(), Quantity = numeric(), stringsAsFactors = FALSE))
  
  observeEvent(input$add_to_basket, {
    selected_item <- input$item
    if (!is.null(selected_item) && selected_item != "") {
      current_basket <- basket()
      if (selected_item %in% current_basket$Item) {
        current_basket$Quantity[current_basket$Item == selected_item] <- current_basket$Quantity[current_basket$Item == selected_item] + 1
      } else {
        new_item <- data.frame(Item = selected_item, Quantity = 1, stringsAsFactors = FALSE)
        current_basket <- rbind(current_basket, new_item)
      }
      basket(current_basket)
    }
  })
  
  observe({
    current_basket <- basket()
    lapply(1:nrow(current_basket), function(i) {
      observeEvent(input[[paste0("remove_", current_basket$Item[i])]], {
        item_to_remove <- current_basket$Item[i]
        current_basket <- basket()
        current_basket <- current_basket[current_basket$Item != item_to_remove, ]
        basket(current_basket)
      })
    })
  })
  
  output$basket_ui <- renderUI({
    current_basket <- basket()
    if (nrow(current_basket) == 0) {
      return(NULL)
    }
    basket_ui <- lapply(1:nrow(current_basket), function(i) {
      fluidRow(
        column(8, current_basket$Item[i]),
        column(2, current_basket$Quantity[i]),
        column(2, actionButton(paste0("remove_", current_basket$Item[i]), "x", class = "btn-danger"))
      )
    })
    do.call(tagList, basket_ui)
  })
  
  cumulative_values <- reactive({
    current_basket <- basket()
    if (nrow(current_basket) == 0) {
      return(NULL)
    }
    cumulative_data <- menu_data %>%
      filter(Item %in% current_basket$Item) %>%
      inner_join(current_basket, by = "Item") %>%
      mutate(across(starts_with("Calories"), ~ . * Quantity)) %>%
      summarise(across(starts_with("Calories"), sum, na.rm = TRUE))
    
    recommended_intake <- guideline_data[guideline_data$Age.Range == input$age & guideline_data$Gender == input$gender, ]
    cumulative_data <- bind_rows(cumulative_data, recommended_intake)
    cumulative_data
  })
  
  output$cumulative_values <- renderTable({
    cumulative_values()
  })
  
  createPlot <- function(column, plotTitle, color) {
    renderPlot({
      if (is.null(input$age) || is.null(input$gender) || 
          is.null(input$restaurant) || input$restaurant == "" || 
          is.null(input$item) || input$item == "") {
        return(NULL)
      }
      
      selected_item <- input$item
      item_data <- menu_data[menu_data$Item == selected_item & menu_data$Company == input$restaurant, ]
      
      item_data[[column]] <- as.numeric(item_data[[column]])
      
      if (length(item_data[[column]]) == 0 || all(is.na(item_data[[column]]))) {
        return(NULL)
      }
      
      recommended_intake <- guideline_data[guideline_data$Age.Range == input$age & guideline_data$Gender == input$gender, column]
      
      if (length(recommended_intake) == 0 || is.na(recommended_intake)) {
        return(NULL)
      }
      
      ylim_max <- max(recommended_intake * 1.25, max(item_data[[column]], na.rm = TRUE) * 1.25, 0)
      
      if (max(item_data[[column]], na.rm = TRUE) > recommended_intake) {
        showModal(modalDialog(
          title = "Nutritional Alert",
          paste("The nutritional value of this item exceeds your recommended daily intake!"),
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      barplot(item_data[[column]], names.arg = NULL, main = plotTitle, col = color, ylim = c(0, ylim_max), las = 2)
      
      abline(h = recommended_intake, col = "red", lwd = 2, lty = 2)
      
      legend("topright", inset = c(-0.2, 0), legend = c(column, "Recommended Daily Intake"), col = c(color, "red"), lwd = 2, lty = c(1, 2), bty = "n")
    })
  }
  
  output$caloriesPlot <- createPlot("Calories", "Calories", "blue")
  output$fatPlot <- createPlot("Total.Fat..g.", "Total Fat", "green")
  output$saturatedFatPlot <- createPlot("Saturated.Fat..g.", "Saturated Fat", "purple")
  output$saltPlot <- createPlot("Salt.g", "Salt", "orange")
  output$carbsPlot <- createPlot("Carbs..g.", "Carbohydrates", "yellow")
  output$fiberPlot <- createPlot("Fiber..g.", "Fiber", "brown")
  output$sugarsPlot <- createPlot("Sugars..g.", "Sugars", "pink")
  output$proteinPlot <- createPlot("Protein..g.", "Protein", "cyan")
  
  createStackedPlot <- function(column, plotTitle, colors) {
    renderPlot({
      current_basket <- basket()
      if (nrow(current_basket) == 0) {
        return(NULL)
      }
      
      cumulative_data <- menu_data %>%
        filter(Item %in% current_basket$Item) %>%
        inner_join(current_basket, by = "Item") %>%
        mutate(across(starts_with(column), ~ . * Quantity)) %>%
        group_by(Item) %>%
        summarise(across(starts_with(column), sum, na.rm = TRUE))
      
      if (nrow(cumulative_data) == 0) {
        return(NULL)
      }
      
      barplot(as.matrix(cumulative_data[, -1]), beside = FALSE, col = colors, main = plotTitle, las = 2)
      legend("topright", legend = cumulative_data$Item, fill = colors, bty = "n")
    })
  }
  
  output$cumulativeCaloriesPlot <- createStackedPlot("Calories", "Cumulative Calories", rainbow(nrow(basket())))
  output$cumulativeFatPlot <- createStackedPlot("Total.Fat..g.", "Cumulative Total Fat", rainbow(nrow(basket())))
  output$cumulativeSaturatedFatPlot <- createStackedPlot("Saturated.Fat..g.", "Cumulative Saturated Fat", rainbow(nrow(basket())))
  output$cumulativeSaltPlot <- createStackedPlot("Salt.g", "Cumulative Salt", rainbow(nrow(basket())))
  output$cumulativeCarbsPlot <- createStackedPlot("Carbs..g.", "Cumulative Carbohydrates", rainbow(nrow(basket())))
  output$cumulativeFiberPlot <- createStackedPlot("Fiber..g.", "Cumulative Fiber", rainbow(nrow(basket())))
  output$cumulativeSugarsPlot <- createStackedPlot("Sugars..g.", "Cumulative Sugars", rainbow(nrow(basket())))
  output$cumulativeProteinPlot <- createStackedPlot("Protein..g.", "Cumulative Protein", rainbow(nrow(basket())))
}

shinyApp(ui = ui, server = server)