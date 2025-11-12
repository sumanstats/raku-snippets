# === Load libraries ===
library(shiny)
library(shinyFeedback)
library(scales)

# === Probability function (same as before) ===
calculate_probability <- function(texture, bmi, pd_size) {
  pd_size <- min(pd_size, 5)
  log_odds <- -3.136 + 0.947 * texture + 0.0679 * bmi - 0.385 * pd_size
  probability <- exp(log_odds) / (1 + exp(log_odds))
  return(probability)
}

calculate_bmi_bsa <- function(weight_kg, height_cm) {
  result <- list(bmi = NA_real_, bsa = NA_real_, category = NA_character_, valid = FALSE, message = NA_character_)
  if (weight_kg < 3 || weight_kg > 150) {
    result$message <- "Error: Weight must be between 3 and 150 kg."; return(result)
  }
  if (height_cm <= 30) {
    result$message <- "Error: Height must be greater than 30 cm."; return(result)
  }
  height_m <- height_cm / 100
  bmi <- weight_kg / (height_m^2)
  bsa <- sqrt((height_cm * weight_kg) / 3600)
  if (bmi < 18.5) category <- "Underweight"
  else if (bmi < 23.0) category <- "Normal Weight"
  else if (bmi < 25.0) category <- "Increased Risk/Overweight"
  else category <- "High Risk/Obese"
  result$bmi <- bmi; result$bsa <- bsa; result$category <- category
  result$valid <- TRUE; result$message <- NULL
  return(result)
}


calculate_slv <- function(weight_kg, height_cm) {
  result <- list(slv = NA_real_, bsa = NA_real_, valid = FALSE, message = NA_character_)
  
  if (!is.numeric(weight_kg) || !is.numeric(height_cm)) {
    result$message <- "Error: Weight and Height must be numbers."
    return(result)
  }
  if (weight_kg < 3 || weight_kg > 150) {
    result$message <- "Error: Weight must be between 3 and 150 kg."
    return(result)
  }
  if (height_cm <= 30) {
    result$message <- "Error: Height must be greater than 30 cm."
    return(result)
  }
  
  bsa <- sqrt((height_cm * weight_kg) / 3600.0)
  slv <- (1267 * bsa) - 794
  
  result$slv <- slv
  result$bsa <- bsa
  result$valid <- TRUE
  return(result)
}

# --- 3. NRI, BMI, and BSA Calculator Function ---
calculate_nri_bmi_bsa <- function(albumin_g_L, weight_kg, usual_weight_kg, height_cm) {
  result <- list(
    nri = NA_real_,
    bmi = NA_real_,
    bsa = NA_real_,
    nri_category = NA_character_,
    bmi_category = NA_character_,
    valid = FALSE,
    message = NA_character_
  )
  
  if (!is.numeric(albumin_g_L) || !is.numeric(weight_kg) ||
      !is.numeric(usual_weight_kg) || !is.numeric(height_cm)) {
    result$message <- "Error: All inputs (Albumin, Weight, Usual Weight, Height) must be numbers."
    return(result)
  }
  if (albumin_g_L <= 1) {
    result$message <- "Error: Serum albumin must be greater than 1 g/L."
    return(result)
  }
  if (weight_kg < 5 || weight_kg > 150) {
    result$message <- "Error: Present weight must be between 5 and 150 kg."
    return(result)
  }
  if (usual_weight_kg < 5 || usual_weight_kg > 150) {
    result$message <- "Error: Usual weight must be between 5 and 150 kg."
    return(result)
  }
  if (height_cm <= 30) {
    result$message <- "Error: Height must be greater than 30 cm."
    return(result)
  }
  
  height_m <- height_cm / 100.0
  bmi <- weight_kg / (height_m * height_m)
  bsa <- sqrt((height_cm * weight_kg) / 3600.0)
  nri <- (1.519 * albumin_g_L) + (41.7 * (weight_kg / usual_weight_kg))
  
  bmi_category <- if (bmi < 18.5) "Underweight" else if (bmi < 23.0) "Normal Weight" else if (bmi < 25.0) "Increased Risk/Overweight" else "High Risk/Obese"
  nri_category <- if (nri > 100.0) "No" else if (nri >= 97.6) "Mild" else if (nri >= 83.5) "Moderate" else "Severe"
  
  result$nri <- nri
  result$bmi <- bmi
  result$bsa <- bsa
  result$nri_category <- nri_category
  result$bmi_category <- bmi_category
  result$valid <- TRUE
  result$message <- NULL
  return(result)
}


# === UI ===
ui <- fluidPage(
  useShinyFeedback(),
  
  titlePanel("Department of GI Surgery, TUTH"),
  p(HTML("This App developed by Dr Suman Khanal, BSc (Stats), MS</br>for the needs of MCh GI Surgery Residents."),
    style = "color: #7f8c8d; font-style: italic;"
  ),
  
  # --- Multiple Tabs ---
  tabsetPanel(
    # ---------------- TAB 1 ----------------
    # ---------------- TAB 1 ----------------
    tabPanel(
      title = "aFRS Calculator",
      sidebarLayout(
        sidebarPanel(
          h4("Input Parameters"),
          
          # Texture: 0 or 1 only
          selectInput(
            inputId = "texture", 
            label = "Texture (0 = Not Soft, 1 = Soft):",
            choices = c("Select texture..." = "", "0 - Not Soft" = 0, "1 - Soft" = 1),
            selected = ""
          ),
          
          numericInput(
            inputId = "bmi", 
            label = "Body Mass Index (BMI):", 
           value = NA, 
            min = 10, 
            max = 50, 
            step = 0.1
          ),
          
          numericInput(
            inputId = "pd_size", 
            label = "PD Size (mm):", 
            value = NA, 
            min = 1, 
            max = 30, 
            step = 1
          ),
          p(tags$em("Note: PD Size needs to be between 1–30 mm. Values > 5 mm are capped at 5 mm.")),
          
          # ➕ Action Button to trigger calculation
          actionButton("calc_afrs", "Calculate", class = "btn-primary")
        ),
        
        mainPanel(
          h4("Risk of Pancreatic Fistula"),
          uiOutput("probability_text")
        )
      )
    ),
    
    # ---------------- TAB 2 ----------------
    tabPanel(
      title = "BMI–BSA Calculator",
      
      sidebarLayout(
        sidebarPanel(
          h4("Input Parameters"),
          
          numericInput("weight", "Weight (kg):", value = NA, min = 3, max = 150, step = 1),
          numericInput("height", "Height (cm):", value = NA, min = 30, max = 250, step = 1),
          actionButton("calc_bmi_bsa", "Calculate", class = "btn-primary")
        ),
        
        mainPanel(
          h4("BMI-BSA"),
          uiOutput("bmi_bsa_result")
        )
      )
    ),
    
    
    # ---------------- TAB 3 ----------------
    tabPanel("SLV Calculator",
             sidebarLayout(
               sidebarPanel(
                 h4("Input Parameters"),
                 numericInput("weight_slv", "Weight (kg):", value = NA, min = 3, max = 150, step = 1),
                 numericInput("height_slv", "Height (cm):", value = NA, min = 30, max = 250, step = 1),
                 actionButton("calc_slv", "Calculate", class = "btn-primary")
               ),
               mainPanel(
                 h4("SLV (Standard Liver Volume)"),
                 uiOutput("slv_result")
               )
             )
    ),
    
    # ---------------- TAB 4 ----------------
    tabPanel(
      title = "NRI–BMI–BSA Calculator",
      sidebarLayout(
        sidebarPanel(
          h4("Input Parameters"),
          numericInput("albumin", "Serum Albumin (g/L):", value = NA, min = 1, max = 60, step = 0.1),
          numericInput("weight_nri", "Present Weight (kg):", value = NA, min = 5, max = 150, step = 1),
          numericInput("usual_weight", "Usual Weight (kg):", value = NA, min = 5, max = 150, step = 1),
          numericInput("height_nri", "Height (cm):", value = NA, min = 30, max = 250, step = 0.1),
          actionButton("calc_nri", "Calculate", class = "btn-primary")
        ),
        mainPanel(
          h4("NRI–BMI–BSA Results"),
          uiOutput("nri_result")
        )
      )
    )
    
  )
)

# === SERVER ===
server <- function(input, output, session) {
  
  # ---------------- Tab 1 Logic (aFRS calculator) ----------------
  observeEvent(input$calc_afrs, {
    # Basic input validation
    hideFeedback("pd_size")
    
    if (is.na(input$texture) || is.na(input$bmi) || is.na(input$pd_size)) {
      output$probability_text <- renderUI({
        tags$div(
          style = "padding:10px; border-left:5px solid #c0392b; background:#f9eaea;",
          p("Please enter all input values before calculation.")
        )
      })
      return()
    }
    
    if (input$bmi < 12) {
      showFeedbackDanger("bmi", text = "Error: BMI must be at least 12.")
      output$probability_text <- renderUI(NULL)
      return()
    } else {
      hideFeedback("bmi")
    }
    
    if (input$pd_size < 1) {
      showFeedbackDanger("pd_size", text = "Error: PD Size must be at least 1 mm.")
      output$probability_text <- renderUI(NULL)
      return()
    }
    
    if (input$pd_size > 30) {
      showFeedbackWarning("pd_size", text = "Warning: PD Size > 30 mm; results may be unreliable.")
    }
    
    # Cap PD size at 5 mm for the calculation
    pd_used <- min(input$pd_size, 5)
    probability <- calculate_probability(as.numeric(input$texture), input$bmi, pd_used)
    
    # Render result
    if (is.na(probability)) {
      output$probability_text <- renderUI({
        tags$div(
          style = "padding:10px; border-left:5px solid #c0392b; background:#f9eaea;",
          p("Could not compute probability. Please check inputs.")
        )
      })
    } else {
      info_text <- NULL
      if (input$pd_size > 5) {
        info_text <- p(tags$span(style="color:#e67e22;font-weight:bold;",
                                 paste("Note: Entered PD Size", input$pd_size,
                                       "mm was capped at 5 mm for calculation.")))
      }
      
      probability_percent <- scales::percent(probability, accuracy = 0.01)
      output$probability_text <- renderUI({
        tags$div(
          style = "font-size:1.2em; padding:10px; border-left:5px solid #2c3e50; background-color:#ecf0f1;",
          p("The risk of pancreatic fistula is:"),
          tags$span(style = "font-weight:bold; color:#c0392b;", probability_percent),
          info_text
        )
      })
    }
  })
  
  
  # ---------------- Tab 2 Logic ----------------
  # ---------------- BMI–BSA Calculator Logic ----------------
  observeEvent(input$calc_bmi_bsa, {
    # Run your function
    result <- calculate_bmi_bsa(input$weight, input$height)
    
    # Store in reactive value so UI updates dynamically
    output$bmi_bsa_result <- renderUI({
      if (!result$valid) {
        # Error or invalid input
        tags$div(
          style = "padding:10px; border-left:5px solid #c0392b; background:#f9eaea;",
          p(result$message)
        )
      } else {
        # Valid result
        tags$div(
          style = "font-size: 1.1em; padding:10px; border-left:5px solid #2c3e50; background-color:#ecf0f1;",
          p(tags$b("BMI:"), sprintf("%.2f kg/m² (%s)", result$bmi, result$category)),
          p(tags$b("BSA (Mosteller):"), sprintf("%.2f m²", result$bsa)),
          tags$span(style="color: #27ae60;", result$message)
        )
      }
    })
  })
  
  
  # ---------------- Tab 3 Logic ----------------
  # --- SLV tab ---
  observeEvent(input$calc_slv, {
    res <- calculate_slv(input$weight_slv, input$height_slv)
    
    # Clear previous feedbacks
    hideFeedback("weight_slv")
    hideFeedback("height_slv")
    
    if (!res$valid) {
      # Choose which input to attach the feedback to
      if (grepl("Weight", res$message)) {
        showFeedbackDanger("weight_slv", text = res$message)
      } else if (grepl("Height", res$message)) {
        showFeedbackDanger("height_slv", text = res$message)
      } else {
        showFeedbackDanger("calc_slv", text = res$message)
      }
      
      output$slv_result <- renderUI(NULL)
      
    } else {
      output$slv_result <- renderUI({
        tags$div(
          style="font-size:1.1em;padding:10px;border-left:5px solid #27ae60;background-color:#ecf0f1;",
          p(tags$b("SLV (Standard Liver Volume):"), sprintf('%.2f cc', res$slv)),
          p(tags$b("BSA (Mosteller):"), sprintf('%.2f m²', res$bsa))
        )
      })
    }
  })
  
  # ---------------- Tab 4 Logic (NRI–BMI–BSA) ----------------
  observeEvent(input$calc_nri, {
    res <- calculate_nri_bmi_bsa(
      input$albumin,
      input$weight_nri,
      input$usual_weight,
      input$height_nri
    )
    
    # Clear previous feedback
    hideFeedback("albumin")
    hideFeedback("weight_nri")
    hideFeedback("usual_weight")
    hideFeedback("height_nri")
    
    if (!res$valid) {
      # Attach error to most relevant input
      if (grepl("Albumin", res$message)) {
        showFeedbackDanger("albumin", text = res$message)
      } else if (grepl("Present weight", res$message)) {
        showFeedbackDanger("weight_nri", text = res$message)
      } else if (grepl("Usual weight", res$message)) {
        showFeedbackDanger("usual_weight", text = res$message)
      } else if (grepl("Height", res$message)) {
        showFeedbackDanger("height_nri", text = res$message)
      } else {
        showFeedbackDanger("calc_nri", text = res$message)
      }
      output$nri_result <- renderUI(NULL)
      
    } else {
      output$nri_result <- renderUI({
        tags$div(
          style = "font-size:1.1em;padding:10px;border-left:5px solid #27ae60;background-color:#ecf0f1;",
          p(tags$b("Nutritional Risk Index (NRI):"),
          sprintf("%.2f (%s risk of malnutrition)", res$nri, res$nri_category)),
          p(tags$b("BMI:"), sprintf("%.2f kg/m² (%s)", res$bmi, res$bmi_category)),
          p(tags$b("BSA (Mosteller):"), sprintf("%.2f m²", res$bsa))
        )
      })
    }
  })
  
  
}

# === Run the App ===
shinyApp(ui = ui, server = server)
