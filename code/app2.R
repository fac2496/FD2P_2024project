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
                               uiOutput("page2_content")
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the selected restaurant
  selected_restaurant <- reactiveVal()
  
  # Reactive value to track if the button has been clicked
  button_clicked <- reactiveVal(FALSE)
  
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
  
  # Store the selected restaurant and navigate to the next page when the button is clicked
  observeEvent(input$next_page, {
    selected_restaurant(input$company)
    button_clicked(TRUE)
    shinyjs::enable(selector = "a[data-value='page2']")
    updateNavbarPage(session, "navbar", selected = "page2")
  })
  

  # Render the content for Page 2
  output$page2_content <- renderUI({
    req(selected_restaurant())
    restaurant <- selected_restaurant()
    categories <- unique(fastfood_data %>% filter(Company == restaurant) %>% pull(Category))
    
    fluidRow(
      column(12,
             h3(paste("What are you craving from", restaurant, "?"), class = "center-text"),
             selectInput("category", "", choices = categories)
      )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)