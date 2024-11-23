# app3_image_test.R

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Display Breakfast Image"),
  mainPanel(
    img(src = "Breakfast.jpg", alt = "Delicious Breakfast", height = "400px")
  )
)

# Define server logic (none needed for static image)
server <- function(input, output, session) {}

# Run the app
shinyApp(ui = ui, server = server)