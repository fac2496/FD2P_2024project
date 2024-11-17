library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("FD2P Data Product"),
  sidebarLayout(
    sidebarPanel(
      selectInput("age", "Choose your age:", choices = NULL, selected = NULL),
      selectInput("gender", "Choose your gender:", choices = NULL, selected = NULL),
      selectInput("restaurant", "Choose a restaurant:", choices = NULL, selected = NULL),
      selectizeInput("item", "Choose an item:", choices = NULL, options = list(placeholder = 'Please scroll or type to select an item'))
    ),
    mainPanel(
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
    )
  )
)

server <- function(input, output, session) {
  # Read the prepped CSV files
  menu_data <- read.csv("/Users/felixculas/shiny/fd2p2024/processed_data/fastfood_workingset.csv", header = TRUE)
  guideline_data <- read.csv("/Users/felixculas/shiny/fd2p2024/processed_data/guidelines_workingset.csv", header = TRUE)
  
  # Extract unique values for age and gender
  unique_ages <- unique(guideline_data$Age.Range)
  unique_genders <- unique(guideline_data$Gender)
  
  # Dynamically update age and gender choices
  updateSelectInput(session, "age", choices = unique_ages, selected = "19-64")
  updateSelectInput(session, "gender", choices = unique_genders, selected = NULL)
  
  # Extract unique restaurants
  unique_restaurants <- unique(menu_data$Company)
  
  # Dynamically update restaurant choices with no selection on startup
  updateSelectInput(session, "restaurant", choices = unique_restaurants, selected = NULL)
  
  # Observe the restaurant selection and update item choices accordingly
  observeEvent(input$restaurant, {
    selected_restaurant <- input$restaurant
    if (!is.null(selected_restaurant) && selected_restaurant != "") {
      filtered_items <- menu_data$Item[menu_data$Company == selected_restaurant]
      updateSelectizeInput(session, "item", choices = filtered_items, selected = NULL, server = TRUE)
    } else {
      updateSelectizeInput(session, "item", choices = NULL, server = TRUE)
    }
  })
  
  # Function to create plots
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
      
      # Check if the nutritional value exceeds the recommended intake
      if (max(item_data[[column]], na.rm = TRUE) > recommended_intake) {
        showModal(modalDialog(
          title = "Nutritional Alert",
          paste("The", plotTitle, "value exceeds the recommended daily intake!"),
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
}

shinyApp(ui = ui, server = server)