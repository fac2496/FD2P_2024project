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
      
      recommended_intake <- guideline_data[guideline_data$Age..years. == input$age & guideline_data$Gender == input$gender, column]
      
      ylim_max <- max(recommended_intake * 1.25, max(item_data[[column]], na.rm = TRUE) * 1.25, 0)
      
      barplot(item_data[[column]], names.arg = selected_item, main = plotTitle, col = color, ylim = c(0, ylim_max))
      
      abline(h = recommended_intake, col = "red", lwd = 2, lty = 2)
      
      legend("topright", legend = c(column, "Recommended Daily Intake"), col = c(color, "red"), lwd = 2, lty = c(1, 2), bty = "n")
    })
  }
  
  output$caloriesPlot <- createPlot("Energy..kcal.day.", "Calories", "blue")
  output$fatPlot <- createPlot("Fat..g.day.", "Total Fat", "green")
  output$saturatedFatPlot <- createPlot("Saturated.fat..g.day.", "Saturated Fat", "purple")
  output$saltPlot <- createPlot("Salt..g.day.", "Salt", "orange")
  output$carbsPlot <- createPlot("Carbohydrate..g.day.", "Carbohydrates", "yellow")
  output$fiberPlot <- createPlot("Fibre..g.day..", "Fiber", "brown")
  output$sugarsPlot <- createPlot("Free.sugars..g.day.", "Sugars", "pink")
  output$proteinPlot <- createPlot("Protein..g.day.", "Protein", "cyan")
}

shinyApp(ui = ui, server = server)