library(shiny)

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
      plotOutput("barPlot")
    )
  )
)

server <- function(input, output, session) {
  # Read the prepped CSV files
  menu_data <- read.csv("/Users/felixculas/shiny/Project/fastfood_working_dataset.csv", header = TRUE)
  guideline_data <- read.csv("/Users/felixculas/shiny/Project/guideline_working_set.csv", header = TRUE)
  
  # Extract unique values for age and gender
  unique_ages <- unique(guideline_data$Age..years.)
  unique_genders <- unique(guideline_data$Gender)
  
  # Dynamically update age and gender choices
  updateSelectInput(session, "age", choices = unique_ages, selected = NULL)
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
  
  output$barPlot <- renderPlot({
    # Only render the plot if all inputs are selected
    if (is.null(input$age) || is.null(input$gender) || 
        is.null(input$restaurant) || input$restaurant == "" || 
        is.null(input$item) || input$item == "") {
      return(NULL)
    }
    
    # Sample plot using selected input
    selected_item <- input$item
    item_data <- menu_data[menu_data$Item == selected_item & menu_data$Company == input$restaurant, ]
    
    # Ensure Energy..kcal.day. column is numeric
    item_data$Energy..kcal.day. <- as.numeric(item_data$Energy..kcal.day.)
    
    # Check if item_data$Energy..kcal.day. is a vector
    if (length(item_data$Energy..kcal.day.) == 0 || all(is.na(item_data$Energy..kcal.day.))) {
      return(NULL)  # No valid data to plot
    }
    
    # Extract recommended daily intake based on age and gender
    recommended_intake <- guideline_data[guideline_data$Age..years. == input$age & guideline_data$Gender == input$gender, "Energy..kcal.day."]
    
    # Calculate the y-axis limit as 25% above the recommended intake
    ylim_max <- recommended_intake * 1.25
    
    # Plot the bar plot
    barplot(item_data$Energy..kcal.day., names.arg = selected_item, main = selected_item, col = "blue", ylim = c(0, ylim_max))
    
    # Add a dotted line for the recommended daily intake
    abline(h = recommended_intake, col = "red", lwd = 2, lty = 2)
    
    # Add a legend
    legend("topright", legend = c("Calories", "Recommended Daily Intake"), col = c("blue", "red"), lwd = 2, lty = c(1, 2), bty = "n")
  })
}

shinyApp(ui = ui, server = server)