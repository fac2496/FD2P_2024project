library(shiny)
library(dplyr)
library(htmltools)

ui <- fluidPage(
  titlePanel("FD2P Data Product"),
  sidebarLayout(
    sidebarPanel(
      selectInput("age", "Choose your age:", choices = NULL, selected = "19-64"),
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
          column(12, uiOutput("nutrition_label"))
        )
      ),
      conditionalPanel(
        condition = "input.toggle_view % 2 == 1",
        h3("Cumulative Nutritional Values"),
        fluidRow(
          column(3, plotOutput("cumulativeCaloriesPlot")),
          column(3, plotOutput("cumulativeFatPlot")),
          column(3, plotOutput("cumulativeSaturatedFatPlot")),
          column(3, plotOutput("cumulativeSaltPlot")),
          column(3, plotOutput("cumulativeSugarsPlot"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  menu_data <- read.csv("/Users/felixculas/shiny/fd2p2024/processed_data/fastfood_workingset.csv", header = TRUE)
  guideline_data <- read.csv("/Users/felixculas/shiny/fd2p2024/processed_data/guidelines_workingset.csv", header = TRUE)
  
  updateSelectInput(session, "age", choices = unique(guideline_data$Age.Range), selected = "19-64")
  updateSelectInput(session, "gender", choices = unique(guideline_data$Gender))
  updateSelectInput(session, "restaurant", choices = unique(menu_data$Company))
  
  observeEvent(input$restaurant, {
    filtered_items <- menu_data$Item[menu_data$Company == input$restaurant]
    updateSelectizeInput(session, "item", choices = filtered_items, server = TRUE)
  })
  
  basket <- reactiveVal(data.frame(Item = character(), Quantity = numeric(), stringsAsFactors = FALSE))
  
  observeEvent(input$add_to_basket, {
    selected_item <- input$item
    if (!is.null(selected_item) && selected_item != "") {
      current_basket <- basket()
      if (selected_item %in% current_basket$Item) {
        current_basket$Quantity[current_basket$Item == selected_item] <- current_basket$Quantity[current_basket$Item == selected_item] + 1
      } else {
        current_basket <- rbind(current_basket, data.frame(Item = selected_item, Quantity = 1, stringsAsFactors = FALSE))
      }
      basket(current_basket)
    }
  })
  
  output$basket_ui <- renderUI({
    current_basket <- basket()
    if (nrow(current_basket) == 0) return(NULL)
    lapply(1:nrow(current_basket), function(i) {
      fluidRow(
        column(8, current_basket$Item[i]),
        column(2, current_basket$Quantity[i]),
        column(2, actionButton(paste0("remove_", current_basket$Item[i]), "x", class = "btn-danger"))
      )
    }) %>% tagList()
  })
  
  getColor <- function(value, recommended) {
    percentage <- (value / recommended) * 100
    if (percentage <= 50) {
      return("green")
    } else if (percentage <= 75) {
      return("yellow")
    } else if (percentage <= 100) {
      return("orange")
    } else {
      return("red")
    }
  }
  
  createNutritionLabel <- function(item_data, guideline_data) {
    age <- input$age
    gender <- input$gender
    
    recommended_calories <- guideline_data %>% filter(Age.Range == age & Gender == gender) %>% pull(Calories)
    recommended_fat <- guideline_data %>% filter(Age.Range == age & Gender == gender) %>% pull(Total.Fat..g.)
    recommended_saturated_fat <- guideline_data %>% filter(Age.Range == age & Gender == gender) %>% pull(Saturated.Fat..g.)
    recommended_salt <- guideline_data %>% filter(Age.Range == age & Gender == gender) %>% pull(Salt.g)
    recommended_sugars <- guideline_data %>% filter(Age.Range == age & Gender == gender) %>% pull(Sugars..g.)
    
    tags$div(
      class = "nutrition-label",
      tags$h3("Nutrition Facts"),
      tags$table(
        tags$tr(
          tags$td("Calories"),
          tags$td(style = paste("color:", getColor(item_data$Calories, recommended_calories)), item_data$Calories)
        ),
        tags$tr(
          tags$td("Total Fat"),
          tags$td(style = paste("color:", getColor(item_data$Total.Fat..g., recommended_fat)), paste(item_data$Total.Fat..g., "g"))
        ),
        tags$tr(
          tags$td("Saturated Fat"),
          tags$td(style = paste("color:", getColor(item_data$Saturated.Fat..g., recommended_saturated_fat)), paste(item_data$Saturated.Fat..g., "g"))
        ),
        tags$tr(
          tags$td("Salt"),
          tags$td(style = paste("color:", getColor(item_data$Salt.g, recommended_salt)), paste(item_data$Salt.g, "g"))
        ),
        tags$tr(
          tags$td("Sugars"),
          tags$td(style = paste("color:", getColor(item_data$Sugars..g., recommended_sugars)), paste(item_data$Sugars..g., "g"))
        )
      )
    )
  }
  
  output$nutrition_label <- renderUI({
    selected_item <- input$item
    if (is.null(selected_item) || selected_item == "") return(NULL)
    
    item_data <- menu_data %>% filter(Item == selected_item & Company == input$restaurant)
    if (nrow(item_data) == 0) return(NULL)
    
    createNutritionLabel(item_data, guideline_data)
  })
  
  createCumulativePlot <- function(column, plotTitle, color) {
    renderPlot({
      current_basket <- basket()
      if (nrow(current_basket) == 0) return(NULL)
      
      cumulative_data <- menu_data %>%
        filter(Item %in% current_basket$Item) %>%
        inner_join(current_basket, by = "Item") %>%
        mutate(Value = .[[column]] * Quantity) %>%
        summarise(Total = sum(Value, na.rm = TRUE))
      
      recommended_intake <- guideline_data[guideline_data$Age.Range == input$age & guideline_data$Gender == input$gender, column, drop = TRUE]
      barplot(cumulative_data$Total, main = plotTitle, col = color, ylim = c(0, max(c(recommended_intake, cumulative_data$Total) * 1.25, na.rm = TRUE)))
      abline(h = recommended_intake, col = "red", lwd = 2, lty = 2)
    })
  }
  
  output$cumulativeCaloriesPlot <- createCumulativePlot("Calories", "Cumulative Calories", "blue")
  output$cumulativeFatPlot <- createCumulativePlot("Total.Fat..g.", "Cumulative Fat", "green")
  output$cumulativeSaturatedFatPlot <- createCumulativePlot("Saturated.Fat..g.", "Cumulative Saturated Fat", "purple")
  output$cumulativeSaltPlot <- createCumulativePlot("Salt.g", "Cumulative Salt", "orange")
  output$cumulativeSugarsPlot <- createCumulativePlot("Sugars..g.", "Cumulative Sugars", "pink")
}

shinyApp(ui = ui, server = server)