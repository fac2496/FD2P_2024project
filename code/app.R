library(shiny)

# Set the working directory
setwd("/Users/felixculas/shiny/fd2p2024/code/")

# Load the data
menu_data <- read.csv("../processed_data/fastfood_categoryset.csv", header = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Restaurant Selector"),
  sidebarLayout(
    sidebarPanel(
      selectInput("restaurant", "Select a restaurant to start:", 
                  choices = c("McDonald’s", "Burger King", "Wendy’s", "KFC", "Taco Bell", "Pizza Hut")),
      selectInput("age", "How old are you?", choices = c("1", "2-3", "4-6", "7-10", "11-14", "15-18", "19 - 64", "65 - 74", "75+")),
      selectInput("gender", "What is your gender?", choices = c("Male", "Female")),
      uiOutput("category_ui"),  # Placeholder for the category dropdown
      uiOutput("item_ui"),  # Placeholder for the item dropdown
      uiOutput("category_image")  # Placeholder for the category image
    ),
    mainPanel(
      uiOutput("font_color")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$restaurant, {
    # Define font colors based on restaurant selection
    font_color <- switch(input$restaurant,
                         "McDonald’s" = "red",
                         "Burger King" = "orange",
                         "Wendy’s" = "blue",
                         "KFC" = "red",
                         "Taco Bell" = "purple",
                         "Pizza Hut" = "firebrick")
    
    # Update the font color
    output$font_color <- renderUI({
      tags$style(HTML(paste0(
        ".title-panel {color: ", font_color, ";}",
        "body {color: ", font_color, ";}"
      )))
    })
    
    # Update the category dropdown based on selected restaurant
    selected_restaurant <- input$restaurant
    categories <- unique(menu_data$Category[menu_data$Company == selected_restaurant])
    output$category_ui <- renderUI({
      selectInput("category", "Select a category:", choices = categories)
    })
  })
  
  observeEvent(input$category, {
    # Filter items based on selected restaurant and category
    selected_items <- menu_data$Item[menu_data$Company == input$restaurant & menu_data$Category == input$category]
    
    # Update the item dropdown
    output$item_ui <- renderUI({
      selectInput("item", "Select an item:", choices = selected_items)
    })
    
    # Display the corresponding category image
    output$category_image <- renderUI({
      img_src <- paste0("food_images/", input$category, ".jpg")
      if (file.exists(img_src)) {
        tags$img(src = img_src, alt = input$category, height = "200px")
      } else {
        tags$p("Image not found")
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
