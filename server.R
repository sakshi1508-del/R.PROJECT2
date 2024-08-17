library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(shinymanager)

# Define credentials
credentials <- data.frame(
  user = c("admin", "user"),  # Define usernames
  password = c("admin_pass", "user_pass"),  # Define passwords
  stringsAsFactors = FALSE
)

# UI definition
ui <- fluidPage(
  titlePanel("MTCars Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X-axis Variable:", choices = names(mtcars)),
      selectInput("y_var", "Select Y-axis Variable:", choices = names(mtcars)),
      sliderInput("cyl", "Filter by Number of Cylinders:",
                  min = min(mtcars$cyl), max = max(mtcars$cyl), 
                  value = c(min(mtcars$cyl), max(mtcars$cyl))),
      selectInput("trans", "Transmission Type:", choices = c("All", "Automatic" = 0, "Manual" = 1)),
      sliderInput("hp", "Horsepower Range:", min = min(mtcars$hp), max = max(mtcars$hp), value = c(min(mtcars$hp), max(mtcars$hp))),
      checkboxInput("add_regression", "Add Regression Line", value = FALSE),
      selectInput("plot_type", "Plot Type:", choices = c("Scatter Plot", "Line Plot", "Bar Plot")),
      selectInput("color_var", "Select Color Variable:", choices = c("None", names(mtcars))),
      sliderInput("point_size", "Point Size:", min = 1, max = 10, value = 3),
      selectInput("transformation", "Transformation:", choices = c("None", "Log", "Square Root")),
      actionButton("stat_test", "Perform Statistical Test"),
      downloadButton("download_data", "Download Filtered Data"),
      downloadButton("download_plot", "Download Plot")
    ),
    mainPanel(
      plotlyOutput("plot"),
      tableOutput("summary_table"),
      verbatimTextOutput("stat_test_result")
    )
  )
)

# Secure the UI with secure_app
ui <- secure_app(ui)

# Server definition
server <- function(input, output, session) {
  
  # User authentication
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  filtered_data <- reactive({
    data <- mtcars %>%
      filter(cyl >= input$cyl[1] & cyl <= input$cyl[2])
    
    if (input$trans != "All") {
      data <- data %>%
        filter(am == as.numeric(input$trans))
    }
    
    data <- data %>%
      filter(hp >= input$hp[1] & hp <= input$hp[2])
    
    data
  })
  
  output$plot <- renderPlotly({
    data <- filtered_data()
    
    # Apply transformations safely
    x_var <- switch(input$transformation,
                    "Log" = ifelse(data[[input$x_var]] > 0, log(data[[input$x_var]]), NA),
                    "Square Root" = sqrt(data[[input$x_var]]),
                    data[[input$x_var]])
    
    y_var <- switch(input$transformation,
                    "Log" = ifelse(data[[input$y_var]] > 0, log(data[[input$y_var]]), NA),
                    "Square Root" = sqrt(data[[input$y_var]]),
                    data[[input$y_var]])
    
    p <- ggplot(data, aes_string(x = "x_var", y = "y_var")) +
      geom_point(aes_string(color = if(input$color_var != "None") input$color_var else NULL), size = input$point_size) +
      theme_minimal()
    
    if (input$add_regression) {
      p <- p + geom_smooth(method = "lm")
    }
    
    p <- switch(input$plot_type,
                "Scatter Plot" = p,
                "Line Plot" = p + geom_line(),
                "Bar Plot" = p + geom_bar(stat = "identity"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$summary_table <- renderTable({
    summary(filtered_data())
  })
  
  observeEvent(input$stat_test, {
    data <- filtered_data()
    if (is.numeric(data[[input$x_var]]) && is.numeric(data[[input$y_var]])) {
      test_result <- t.test(data[[input$x_var]], data[[input$y_var]])
      output$stat_test_result <- renderPrint({
        test_result
      })
    } else {
      output$stat_test_result <- renderPrint({
        "Selected variables must be numeric for t-test."
      })
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("filtered_data", ".csv", sep = "") },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("plot", ".png", sep = "") },
    content = function(file) {
      # Ensure the correct plot is saved
      p <- last_plot()
      ggsave(file, plot = p)
    }
  )
}


