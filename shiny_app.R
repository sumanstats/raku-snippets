library(shiny)
library(shinyFeedback)
library(scales)

# The function remains the same, capping at 5 for the calculation
calculate_probability <- function(texture, bmi, pd_size) {
  # assume pd_size is a valid numeric (not NA) when called
  pd_size <- min(pd_size, 5)
  log_odds <- -3.136 + 0.947 * texture + 0.0679 * bmi - 0.385 * pd_size
  probability <- exp(log_odds) / (1 + exp(log_odds))
  return(probability)
}

ui <- fluidPage(
  useShinyFeedback(),
  
  titlePanel("Department of GI Surgery, TUTH"),
  p(HTML("This App developed by Dr Suman Khanal, BSc (Stats), MS</br>for the needs of MCh GI Surgery Residents (always under stress !)."),
    style = "color: #7f8c8d; font-style: italic;"
  ),
  
  tabsetPanel(
    tabPanel(
      title = "aFRS Calculator",
      
      sidebarLayout(
        sidebarPanel(
          h4("Input Parameters"),
          
          numericInput(
            inputId = "texture",
            label = "Texture Score:",
            value = 1,
            min = 0,
            max = 5,
            step = 0.1
          ),
          p(tags$em("Note: Texture score is 1 for soft pancreas and 0 for not-soft pancreas.")),
          
          numericInput(
            inputId = "bmi",
            label = "Body Mass Index (BMI):",
            value = 25,
            min = 10,
            max = 50,
            step = 0.1
          ),
          
          numericInput(
            inputId = "pd_size",
            label = "PD Size (mm):",
            value = 2,
            step = 0.1
          ),
          
          p(tags$em("Note: PD Size needs to be between 1-30 mm. The calculation will cap any value > 5 mm at 5 mm."))
        ),
        
        mainPanel(
          h4("Calculation Result"),
          uiOutput("probability_text")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Watch pd_size and show appropriate feedback (handle NA explicitly)
  observeEvent(input$pd_size, {
    pd <- input$pd_size
    
    # If cleared, pd becomes NA — treat that as an error requiring user input
    if (is.na(pd)) {
      showFeedbackDanger(inputId = "pd_size", text = "PD Size is required.")
      return()
    }
    
    # Now pd is numeric (not NA)
    if (pd < 1) {
      showFeedbackDanger(inputId = "pd_size", text = "Error: PD Size must be at least 1 mm to calculate.")
    } else if (pd > 30) {
      showFeedbackWarning(inputId = "pd_size", text = "Warning: For best results, please enter a value up to 30 mm.")
    } else {
      hideFeedback("pd_size")
    }
  }, ignoreNULL = FALSE)
  
  output$probability_text <- renderUI({
    
    # Require texture and bmi to be non-missing
    req(!is.null(input$texture), !is.null(input$bmi))
    
    # Require pd_size to be present and numeric
    if (is.null(input$pd_size) || is.na(input$pd_size)) {
      # Provide a nice message instead of crashing
      return(
        tags$div(style = "padding:10px; border-left:5px solid #c0392b; background:#f9eaea;",
                 p("Please enter PD Size (mm) to calculate the probability."))
      )
    }
    
    # Now safe to compare
    if (input$pd_size < 1) {
      return(
        tags$div(style = "padding:10px; border-left:5px solid #c0392b; background:#f9eaea;",
                 p("PD Size must be at least 1 mm to perform the calculation."))
      )
    }
    
    # compute capped value safely
    pd_used <- min(input$pd_size, 5)
    
    # Call the function with the validated inputs
    probability <- calculate_probability(input$texture, input$bmi, pd_used)
    
    # If probability is NA for any reason, handle gracefully
    if (is.na(probability)) {
      return(
        tags$div(style = "padding:10px; border-left:5px solid #c0392b; background:#f9eaea;",
                 p("Could not compute probability. Please check inputs."))
      )
    }
    
    info_text <- ""
    if (input$pd_size > 5) {
      info_text <- p(tags$span(style="color: #e67e22; font-weight: bold;",
                               paste("Note: The entered PD Size of", input$pd_size, "mm was capped at 5 mm for this calculation.")))
    }
    
    probability_percent <- scales::percent(probability, accuracy = 0.01)
    
    tags$div(
      style = "font-size: 1.2em; padding: 10px; border-left: 5px solid #2c3e50; background-color: #ecf0f1;",
      p("The calculated probability is:"),
      tags$span(
        style = "font-weight: bold; color: #c0392b;",
        probability_percent
      ),
      info_text
    )
  })
}

shinyApp(ui = ui, server = server)
