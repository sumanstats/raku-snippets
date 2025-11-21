library(shiny)
library(bslib)

ui <- page_fluid(
  # Use a modern Bootstrap 5 theme
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  
  tags$head(
    tags$style(HTML("
      /* * CSS FIX FOR ALIGNMENT 
       * This forces the input and button to sit perfectly side-by-side
       */
      
      /* The container for the input + button */
      .input-group-custom {
        display: flex;
        align-items: stretch; /* Forces equal height */
        width: 100%;
        margin-bottom: 15px;
      }

      /* 1. Fix the Shiny Input Wrapper */
      /* numericInput creates a wrapper div; we need it to fill space and have no margins */
      .input-group-custom .shiny-input-container {
        flex-grow: 1;
        margin-bottom: 0 !important;
        width: auto; /* Let flex handle width */
      }

      /* 2. Fix the Actual Input Field */
      /* Remove right corners so it merges with the button */
      .input-group-custom input.form-control {
        border-top-right-radius: 0 !important;
        border-bottom-right-radius: 0 !important;
        height: 100%; /* Ensure it fills the vertical space */
      }

      /* 3. Fix the Button */
      /* Remove left corners, fix borders, and align text */
      .unit-toggle-btn {
        border-top-left-radius: 0 !important;
        border-bottom-left-radius: 0 !important;
        margin-left: -1px; /* Overlap borders slightly for 'merged' look */
        
        /* Visual styling */
        background-color: #f8f9fa;
        color: #007bc2;
        border-color: #ced4da;
        font-weight: 600;
        
        /* Layout */
        display: flex;
        align-items: center;
        justify-content: center;
        min-width: 70px; /* Consistent width for the unit label */
      }
      
      .unit-toggle-btn:hover {
        background-color: #e2e6ea;
        color: #0056b3;
      }
      
      .result-card {
        background-color: #f8f9fa;
        border-left: 5px solid #2c3e50;
      }
    "))
  ),
  
  div(class = "container-sm py-4", style = "max-width: 800px;",
      h2("Clinical Calculator", class = "mb-4"),
      
      div(class = "row",
          # --- Left Column: Inputs ---
          div(class = "col-md-7",
              card(
                card_header("Patient Data"),
                card_body(
                  
                  # Sex Input
                  div(class = "mb-3",
                      tags$label("Sex", class="form-label fw-bold me-3"),
                      radioButtons("sex", NULL, 
                                   choices = c("Male" = "male", "Female" = "female"), 
                                   inline = TRUE)
                  ),
                  
                  # Weight Row
                  tags$label("Weight", class="form-label"),
                  div(class = "input-group-custom",
                      numericInput("weight", NULL, value = 70),
                      actionButton("toggle_weight", "kg", class = "unit-toggle-btn")
                  ),
                  
                  # Height Row
                  tags$label("Height", class="form-label"),
                  div(class = "input-group-custom",
                      numericInput("height", NULL, value = 175),
                      actionButton("toggle_height", "cm", class = "unit-toggle-btn")
                  ),
                  
                  # Creatinine Row
                  tags$label("Serum Creatinine", class="form-label"),
                  div(class = "input-group-custom",
                      numericInput("creat", NULL, value = 1.0, step = 0.1),
                      actionButton("toggle_creat", "mg/dL", class = "unit-toggle-btn")
                  )
                )
              )
          ),
          
          # --- Right Column: Results ---
          div(class = "col-md-5",
              card(class = "result-card",
                   card_header("Results"),
                   card_body(
                     div(class="mb-3",
                         h6("Body Mass Index (BMI)", class="text-muted"),
                         h2(textOutput("bmi_result", inline=TRUE), style = "color: #2c3e50; font-weight: bold;")
                     ),
                     hr(),
                     div(
                       h6("Cockcroft-Gault CrCl", class="text-muted"),
                       h2(textOutput("crcl_result", inline=TRUE), style = "color: #2c3e50; font-weight: bold;"),
                       span("mL/min", class="ms-1")
                     )
                   )
              )
          )
      )
  )
)

server <- function(input, output, session) {
  
  # --- 1. State Management ---
  state <- reactiveValues(
    weight_unit = "kg",
    height_unit = "cm",
    creat_unit = "mg/dL"
  )
  
  # --- 2. Toggle Logic ---
  
  # Weight Toggle
  observeEvent(input$toggle_weight, {
    current_val <- input$weight
    if (state$weight_unit == "kg") {
      state$weight_unit <- "lbs"
      updateActionButton(session, "toggle_weight", label = "lbs")
      if (!is.na(current_val)) updateNumericInput(session, "weight", value = round(current_val * 2.20462, 1))
    } else {
      state$weight_unit <- "kg"
      updateActionButton(session, "toggle_weight", label = "kg")
      if (!is.na(current_val)) updateNumericInput(session, "weight", value = round(current_val / 2.20462, 1))
    }
  })
  
  # Height Toggle
  observeEvent(input$toggle_height, {
    current_val <- input$height
    if (state$height_unit == "cm") {
      state$height_unit <- "in"
      updateActionButton(session, "toggle_height", label = "in")
      if (!is.na(current_val)) updateNumericInput(session, "height", value = round(current_val * 0.393701, 1))
    } else {
      state$height_unit <- "cm"
      updateActionButton(session, "toggle_height", label = "cm")
      if (!is.na(current_val)) updateNumericInput(session, "height", value = round(current_val / 0.393701, 1))
    }
  })
  
  # Creatinine Toggle
  observeEvent(input$toggle_creat, {
    current_val <- input$creat
    if (state$creat_unit == "mg/dL") {
      state$creat_unit <- "µmol/L"
      updateActionButton(session, "toggle_creat", label = "µmol/L")
      if (!is.na(current_val)) updateNumericInput(session, "creat", value = round(current_val * 88.4, 1))
    } else {
      state$creat_unit <- "mg/dL"
      updateActionButton(session, "toggle_creat", label = "mg/dL")
      if (!is.na(current_val)) updateNumericInput(session, "creat", value = round(current_val / 88.4, 2))
    }
  })
  
  # --- 3. Calculations ---
  
  get_weight_kg <- reactive({
    req(input$weight)
    if (state$weight_unit == "lbs") return(input$weight / 2.20462)
    return(input$weight)
  })
  
  get_height_m <- reactive({
    req(input$height)
    if (state$height_unit == "in") return(input$height * 0.0254)
    return(input$height / 100)
  })
  
  get_creat_mgdl <- reactive({
    req(input$creat)
    if (state$creat_unit == "µmol/L") return(input$creat / 88.4)
    return(input$creat)
  })
  
  output$bmi_result <- renderText({
    req(get_weight_kg(), get_height_m())
    bmi <- get_weight_kg() / (get_height_m() ^ 2)
    sprintf("%.1f", bmi)
  })
  
  output$crcl_result <- renderText({
    req(get_weight_kg(), get_creat_mgdl(), input$sex)
    age <- 45 # default age
    w <- get_weight_kg()
    scr <- get_creat_mgdl()
    factor <- if (input$sex == "female") 0.85 else 1.0
    
    if(scr == 0) return("Error")
    crcl <- ((140 - age) * w) / (72 * scr) * factor
    sprintf("%.1f", crcl)
  })
}

shinyApp(ui, server)
