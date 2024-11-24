library(shiny)
library(ggplot2)

# Load datasets
setwd("/Users/felixculas/shiny/fd2p2024/code/")
menu_data <- read.csv("../processed_data/fastfood_categoryset.csv", header = TRUE) 
rdi_data <- read.csv("../processed_data/guidelines_workingset.csv", header = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Fast Food Nutrition Visualizer"),
  
  tabsetPanel(
    # Tab 1: Main App Functionality
    tabPanel("Visualizer",
             sidebarLayout(
               sidebarPanel(
                 selectInput("restaurant", "Select a restaurant to start:", 
                             choices = unique(menu_data$Company)),
                 selectInput("age", "How old are you?", choices = unique(rdi_data$Age.Range)),
                 selectInput("gender", "What is your gender?", choices = unique(rdi_data$Gender)),
                 uiOutput("category_ui"),  # Dynamically updated category dropdown
                 uiOutput("item_ui"),      # Dynamically updated item dropdown
                 uiOutput("category_image")  # Image of selected category
               ),
               mainPanel(
                 h3("Nutritional Information"),
                 plotOutput("nutrition_plot")  # Nutritional visualization
               )
             )
    ),
    
    # Tab 2: About
    tabPanel("About",
             fluidRow(
               column(12,
                      h3("About This App"),
                      p("This app helps users explore the nutritional content of fast food items."),
                      p("Using the dropdowns, you can filter by restaurant, category, and food item."),
                      p("The main panel visualizes how the selected item compares to your recommended daily intake (RDI) based on your age and gender."),
                      p("Built with R and Shiny.")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update categories based on selected restaurant
  observeEvent(input$restaurant, {
    categories <- unique(menu_data$Category[menu_data$Company == input$restaurant])
    output$category_ui <- renderUI({
      selectInput("category", "Select a category:", choices = categories)
    })
  })
  
  # Update items based on selected category
  observeEvent(input$category, {
    items <- unique(menu_data$Item[menu_data$Company == input$restaurant & 
                                     menu_data$Category == input$category])
    output$item_ui <- renderUI({
      selectInput("item", "Select an item:", choices = items)
    })
    
    # Display category image
    output$category_image <- renderUI({
      img_src <- paste0(input$category, ".jpg")
      if (file.exists(file.path("www", img_src))) {
        tags$img(src = img_src, alt = input$category, height = "200px")
      } else {
        tags$p("Image not available")
      }
    })
  })
  
  # Reactive block to calculate nutrient data
  nutrient_data <- reactive({
    req(input$item, input$age, input$gender)  # Ensure all inputs are available
    
    # Filter data for the selected item
    selected_item_data <- menu_data[menu_data$Item == input$item, ]
    
    # Fetch RDI values for Calories, Total Fat, Sugars, and Salt
    rdi_row <- rdi_data[rdi_data$Age.Range == input$age & rdi_data$Gender == input$gender, ]
    rdi <- as.numeric(rdi_row[1, c("Calories", "Total.Fat..g.", "Sugars..g.", "Salt.g")])
    rdi_names <- c("Calories", "Total Fat", "Sugars", "Salt")
    
    # Extract selected item nutrient values
    nutrient_values <- as.numeric(selected_item_data[1, c("Calories", "Total.Fat..g.", "Sugars..g.", "Salt.g")])
    percent_of_rdi <- nutrient_values / rdi * 100
    
    # Create data frame for visualization
    data.frame(Nutrient = rdi_names, 
               Value = nutrient_values, 
               RDI = rdi, 
               Percent = percent_of_rdi)
  })
  
  # Render nutrition plot
  output$nutrition_plot <- renderPlot({
    nutrient_df <- nutrient_data()
    
    # Define custom colors for each nutrient
    nutrient_colors <- c("Calories" = "steelblue", 
                         "Total Fat" = "darkorange", 
                         "Sugars" = "forestgreen", 
                         "Salt" = "purple")
    
    ggplot(nutrient_df, aes(x = Percent, y = Nutrient, fill = Nutrient)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(Percent, 1), "%")), hjust = -0.2, color = "black") +
      scale_fill_manual(values = nutrient_colors) +
      geom_vline(aes(xintercept = 100, color = "Max Daily Intake"), linetype = "dashed", size = 0.8) +
      scale_color_manual(values = c("Max Daily Intake" = "red")) +
      labs(
        title = paste("What's in a", input$item, "?"),
        x = "% of Daily Nutritional Requirements",
        y = "Nutritional Components",
        fill = "Nutrient",
        color = ""  # Blank title for the line legend
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",  # Show legend
        panel.grid.major.y = element_blank()  # Remove horizontal gridlines
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)