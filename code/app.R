library(shiny)
library(ggplot2)
library(jsonlite)
library(htmltools)
library(scales)


# Load datasets
menu_data <- read.csv("../processed_data/fastfood_categoryset.csv", header = TRUE) 
rdi_data <- read.csv("../processed_data/guidelines_workingset.csv", header = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Better Bites: Know Your Food"),
  
  tabsetPanel(
    # Tab 1: Main App Functionality
    tabPanel("Visualizer",
             sidebarLayout(
               sidebarPanel(
                 selectInput("restaurant", "Select a restaurant to start:", 
                             choices = unique(menu_data$Company)),
                 selectInput("age", "How old are you?", choices = unique(rdi_data$Age.Range), selected = "19 - 64"),
                 selectInput("gender", "What is your gender?", choices = unique(rdi_data$Gender)),
                 uiOutput("category_ui"),  # Dynamically updated category dropdown
                 uiOutput("item_ui"),      # Dynamically updated item dropdown
                 uiOutput("category_image"),  # Image of selected category
                 actionButton("add_item", "Add item to basket"),
                 uiOutput("remove_item_ui"), # UI for remove item button
                 actionButton("remove_item", "Remove item from basket")  # Remove item button
               ),
               mainPanel(
                 h3("Nutritional Breakdown of Your Menu Choice!"),
                 plotOutput("nutrition_plot"),  # Nutritional visualization
                 h3("Basket Summary"),
                 tableOutput("basket_summary")  # Summary of items in basket
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
  observeEvent(input$category, {
    print(input$category)
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
  
  # Initialize basket
  basket <- reactiveVal(data.frame(Item = character(), Calories = numeric(), Total.Fat = numeric(), Sugars = numeric(), Salt = numeric(), Quantity = numeric(), stringsAsFactors = FALSE))
  
  # Add item to basket
  observeEvent(input$add_item, {
    req(input$item, input$age, input$gender)
    
    # Add selected item to the basket
    item_selected <- input$item
    restaurant_selected <- input$restaurant
    selected_item_data <- menu_data[menu_data$Item == item_selected & 
                                      menu_data$Company == restaurant_selected, ]
    
    if (nrow(selected_item_data) == 1) {
      # Prepare new item
      new_item <- data.frame(
        Item = item_selected,
        Calories = selected_item_data$Calories,
        Total.Fat = selected_item_data$Total.Fat..g.,
        Sugars = selected_item_data$Sugars..g.,
        Salt = selected_item_data$Salt.g
      )
      updated_basket <- rbind(basket(), new_item)
      basket(updated_basket)
      
      # Check if total basket nutrients exceed RDI
      total_nutrients <- colSums(updated_basket[, c("Calories", "Total.Fat", "Sugars", "Salt")])
      rdi_row <- rdi_data[rdi_data$Age.Range == input$age & rdi_data$Gender == input$gender, ]
      rdi <- as.numeric(rdi_row[1, c("Calories", "Total.Fat..g.", "Sugars..g.", "Salt.g")])
      
      # Identify nutrients exceeding RDI
      nutrients_exceeding <- which(total_nutrients > rdi)
      if (length(nutrients_exceeding) > 0) {
        nutrient_names <- c("Calories", "Total Fat", "Sugars", "Salt")
        exceeded <- nutrient_names[nutrients_exceeding]
        warning_message <- paste("Notice: Your basket exceeds the recommended daily intake for", 
                                 paste(exceeded, collapse = ", "), ". Please enjoy in moderation!")
        
        # Suggest alternatives for the most recent item
        alternatives <- menu_data[menu_data$Company == input$restaurant & 
                                    menu_data$Category == input$category, ]
        valid_alternatives <- alternatives[apply(alternatives[, c("Calories", "Total.Fat..g.", "Sugars..g.", "Salt.g")], 1, 
                                                 function(x) all(x <= rdi)), ]
        
        # Show modal with actionable options
        showModal(modalDialog(
          title = "Warning: Nutritional Intake Exceeded",
          tagList(
            tags$p(warning_message),
            if (nrow(valid_alternatives) > 0) {
              tagList(
                tags$p("Consider these alternatives instead:"),
                selectInput("alternative_select", "Choose an alternative item:", 
                            choices = valid_alternatives$Item),
                actionButton("add_alternative", "Add Selected Alternative")
              )
            } else {
              tags$p("Unfortunately, no suitable alternatives are available in this category.")
            },
            tags$hr(),
            actionButton("remove_offending", "Remove Last Added Item"),
            tags$hr(),
            actionButton("dismiss_modal", "Close", class = "btn-primary")
          ),
          easyClose = FALSE,
          footer = NULL
        ))
      }
    } else {
      print("Error: Multiple items or no matching items found. Check your filtering logic.")
    }
  })
  
  # Logic for 'Remove Offending' button
  observeEvent(input$remove_offending, {
    current_basket <- basket()
    
    # Remove the last added item
    if (nrow(current_basket) > 0) {
      updated_basket <- current_basket[-nrow(current_basket), ]
      basket(updated_basket)
      print("Removed the last added item.")
    } else {
      print("Basket is empty; nothing to remove.")
    }
  })
  
  # Logic for 'Add Alternative' button
  observeEvent(input$add_alternative, {
    # Get selected alternative item
    selected_alternative <- input$alternative_select
    restaurant <- input$restaurant
    
    # Find the corresponding item in the menu data
    alternative_data <- menu_data[menu_data$Item == selected_alternative & 
                                    menu_data$Company == restaurant, ]
    if (nrow(alternative_data) == 1) {
      # Prepare new item for the basket
      new_item <- data.frame(
        Item = selected_alternative,
        Calories = alternative_data$Calories,
        Total.Fat = alternative_data$Total.Fat..g.,
        Sugars = alternative_data$Sugars..g.,
        Salt = alternative_data$Salt.g
      )
      # Add the new item to the basket
      basket(rbind(basket(), new_item))
      
      # Provide feedback in the console (optional debugging)
      print(paste("Added alternative item:", selected_alternative))
    }
  })
  
  # Render dropdown for item removal when button is clicked
  observeEvent(input$remove_item, {
    current_basket <- basket()
    
    # Only show dropdown if basket is not empty
    if (nrow(current_basket) > 0) {
      output$remove_item_ui <- renderUI({
        tagList(
          selectInput("remove_item_select", "Select an item to remove", choices = current_basket$Item),
          actionButton("confirm_remove", "Confirm Removal")
        )
      })
    } else {
      output$remove_item_ui <- renderUI({
        tags$p("Basket is empty.", style = "color: gray;")
      })
    }
  })
  
  # Render dropdown for item removal when button is clicked
  observeEvent(input$remove_item, {
    current_basket <- basket()
    
    if (nrow(current_basket) > 0) {
      output$remove_item_ui <- renderUI({
        tagList(
          selectInput("remove_item_select", "Select an item to remove", choices = unique(current_basket$Item)),
          actionButton("confirm_remove", "Confirm Removal")
        )
      })
    } else {
      output$remove_item_ui <- renderUI({
        tags$p("Basket is empty.", style = "color: gray;")
      })
    }
  })
  
  # Remove the selected item when "Confirm Removal" is clicked
  observeEvent(input$confirm_remove, {
    req(input$remove_item_select) # Ensure an item is selected
    current_basket <- basket()
    item_to_remove <- input$remove_item_select
    
    # Find and remove the first instance of the selected item
    remove_index <- which(current_basket$Item == item_to_remove)[1] # Remove only one occurrence
    if (!is.na(remove_index)) {
      updated_basket <- current_basket[-remove_index, ]
      basket(updated_basket)
      
      # Update removal dropdown UI or show "empty" message
      if (nrow(updated_basket) > 0) {
        output$remove_item_ui <- renderUI({
          tagList(
            selectInput("remove_item_select", "Select an item to remove", choices = unique(updated_basket$Item)),
            actionButton("confirm_remove", "Confirm Removal")
          )
        })
      } else {
        output$remove_item_ui <- renderUI({
          tags$p("Basket is empty.", style = "color: gray;")
        })
      }
    }
  })
  
  # Remove the selected item when "Confirm Removal" is clicked
  observeEvent(input$confirm_remove, {
  req(input$remove_item_select) # Ensure an item is selected
  current_basket <- basket()
  item_to_remove <- input$remove_item_select
  
  # Find and remove the first instance of the selected item
  remove_index <- which(current_basket$Item == item_to_remove)[1]
  if (!is.na(remove_index)) {
    updated_basket <- current_basket[-remove_index, ]
    basket(updated_basket)
    
    # Update removal UI
    if (nrow(updated_basket) > 0) {
      output$remove_item_ui <- renderUI({
        tagList(
          selectInput("remove_item_select", "Select an item to remove", choices = unique(updated_basket$Item)),
          actionButton("confirm_remove", "Confirm Removal")
        )
      })
    } else {
      output$remove_item_ui <- renderUI({
        tags$p("Basket is empty.", style = "color: gray;")
      })
    }
  }
})
  
  
  # Enable manual dismissal of pop up button
  observeEvent(input$dismiss_modal, {
    removeModal()
  })
  
  # Render nutrition plot
  output$nutrition_plot <- renderPlot({
    nutrient_df <- nutrient_data()
    
    nutrient_colors <- c("Calories" = "steelblue", 
                         "Total Fat" = "darkorange", 
                         "Sugars" = "forestgreen", 
                         "Salt" = "purple")
    nutrient_df$Nutrient <- factor(nutrient_df$Nutrient, levels = c("Salt", "Sugars", "Total Fat", "Calories"))
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
        fill = "Components",
        color = ""  # Blank title for the line legend
      ) +
      theme_minimal() +
      theme(
        legend.position = "right", 
        panel.grid.major.y = element_blank()
      )
  })
  
  # Render basket summary with conditional formatting
  output$basket_summary <- renderUI({
    basket_data <- basket()
    if (nrow(basket_data) == 0) return(tags$p("Basket is empty.", style = "color: gray;"))
    
    # Calculate totals for the basket
    total_calories <- sum(basket_data$Calories)
    total_fat <- sum(basket_data$Total.Fat)
    total_sugars <- sum(basket_data$Sugars)
    total_salt <- sum(basket_data$Salt)
    
    # Fetch RDI values based on user input
    rdi_row <- rdi_data[rdi_data$Age.Range == input$age & rdi_data$Gender == input$gender, ]
    rdi <- as.numeric(rdi_row[1, c("Calories", "Total.Fat..g.", "Sugars..g.", "Salt.g")])
    
    # Prepare table rows with conditional formatting
    summary_table <- tags$table(
      style = "width: 100%; border-collapse: collapse;",
      tags$thead(
        tags$tr(
          tags$th("Nutrient"), tags$th("Total"), tags$th("RDI"), tags$th("% of RDI")
        )
      ),
      tags$tbody(
        tags$tr(
          style = ifelse(total_calories > rdi[1], "color: red;", ""),
          tags$td("Calories"), tags$td(total_calories), tags$td(rdi[1]), tags$td(sprintf("%.1f%%", total_calories / rdi[1] * 100))
        ),
        tags$tr(
          style = ifelse(total_fat > rdi[2], "color: red;", ""),
          tags$td("Total Fat"), tags$td(total_fat), tags$td(rdi[2]), tags$td(sprintf("%.1f%%", total_fat / rdi[2] * 100))
        ),
        tags$tr(
          style = ifelse(total_sugars > rdi[3], "color: red;", ""),
          tags$td("Sugars"), tags$td(total_sugars), tags$td(rdi[3]), tags$td(sprintf("%.1f%%", total_sugars / rdi[3] * 100))
        ),
        tags$tr(
          style = ifelse(total_salt > rdi[4], "color: red;", ""),
          tags$td("Salt"), tags$td(total_salt), tags$td(rdi[4]), tags$td(sprintf("%.1f%%", total_salt / rdi[4] * 100))
        )
      )
    )
    
    # Return the styled table
    summary_table
  })
}

# Run the application 
shinyApp(ui = ui, server = server)