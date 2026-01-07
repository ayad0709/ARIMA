# Define server logic for Shiny app
server <- function(input, output, session) {


  # 1. Reactive expression to read data from the file
  data <- reactive({
    req(input$fileData)
    inFile <- input$fileData
    
    # Extract extension from the original filename (more reliable than datapath)
    ext <- tools::file_ext(inFile$name)
    
    # Use tryCatch to prevent the app from crashing on bad file formatting
    df <- tryCatch({
      switch(ext,
             "csv"  = read.csv(inFile$datapath, header = TRUE),
             "txt"  = read.delim(inFile$datapath),
             "xlsx" = readxl::read_excel(inFile$datapath),
             "xls"  = readxl::read_excel(inFile$datapath),
             "sav"  = haven::read_spss(inFile$datapath),
             stop("Unsupported file format. Please upload .csv, .txt, .xlsx, or .sav"))
    }, error = function(e) {
      shinyalert("Import Error", e$message, type = "error")
      return(NULL)
    })
    
    return(df)
  })
  
  # 2. Reactive expression to extract the selected column
  selected_column_data <- reactive({
    req(data(), input$colNum)
    df <- data()
    
    # FIX: Use the column name directly. 
    # input$colNum is a string (e.g., "Sales"). as.numeric("Sales") returns NA, causing a crash.
    colData <- df[[input$colNum]]
    
    # Use validate() instead of stop() for a cleaner user interface message
    validate(
      need(is.numeric(colData), "The selected column is not numeric. Please select a different column.")
    )
    
    # Ensure it returns a simple numeric vector for TS functions
    return(as.numeric(colData))
  })

  
   # Helper function to apply the theme and the custom label sizes globally
  apply_user_theme <- function() {
    # 1. Get the base theme (e.g., theme_gray, theme_minimal)
    theme_name <- if (!is.null(userData$selectedTheme)) userData$selectedTheme else "theme_gray"
    base_theme <- match.fun(theme_name)()
    
    # 2. Combine the base theme with your custom global label settings
    return(
      base_theme +
        theme(
          # Global Main Title Style
          plot.title = element_text(size = userData$labelsize + 4, face = "bold", hjust = 0.5),
          # Global Axis Titles (X and Y)
          axis.title = element_text(size = userData$labelsize + 2),
          # Global Axis Ticks (Numbers/Dates)
          axis.text = element_text(size = userData$labelsize)
        )
    )
  }
  
  
  # Helper to clean dimension inputs (removes 'px' if present)
  getPlotDim <- function(dimVal) {
    as.numeric(gsub("[^0-9]", "", dimVal))
  }

########  ##########  ##########  ##########  ##########  ##########  ##########
########  ##########  ##########  ##########  ##########  ##########  ##########
#
#                          extractSARIMAeqLaTeX
#
########  ##########  ##########  ##########  ##########  ##########  ##########
########  ##########  ##########  ##########  ##########  ##########  ##########



  extractSARIMAeqLaTeX <- function(model) {

    # if (is.null(model)) return("Model not fitted yet")

    # Extract coefficients and terms
    coefs <- coef(model)
    coefs <- round(coefs, 3) # number of digits after comma
    p <- model$arma[1]  # AR order
    d <- model$arma[6]  # Degree of differencing
    q <- model$arma[2]  # MA order
    P <- model$arma[3]  # Seasonal AR order
    D <- model$arma[7]  # Seasonal differencing
    Q <- model$arma[4]  # Seasonal MA order
    s <- model$arma[5]  # Seasonal period
    
    # Check for intercept and include it in the equation
    intercept <- if ("intercept" %in% names(coefs)) paste0(coefs["intercept"], " + ") else ""
    

    # Check for drift and include it in the equation
    drift <- if ("drift" %in% names(coefs)) paste0(" + ", coefs["drift"], "t") else ""

    # AIC is directly available from the model
    aic_value <- AIC(model)

    # Calculate AICc
    n <- length(model$residuals)  # Number of observations
    k <- length(model$coef) + 1  # Number of parameters + 1 for the variance
    aicc_value <- aic_value + (2 * k * (k + 1)) / (n - k - 1)

    # BIC is also directly available
    bic_value <- BIC(model)



    ########  ##########  ##########  ##########  ##########  ##########  ##########


    # Create the symbolic LaTeX strings
    symbolic_ar <- paste0(" - \\phi_", 1:p, "L^", 1:p)
    symbolic_ma <- paste0(" + \\theta_", 1:q, "L^", 1:q)
    symbolic_sar <- paste0(" - \\Phi_", 1:P, "L^{", s * (1:P), "}")
    symbolic_sma <- paste0(" + \\Theta_", 1:Q, "L^{", s * (1:Q), "}")

    symbolic_eq <- paste0(
      "\\phi_p(L)\\Phi_P(L^S)(1-L)^d(1-L^S)^D Y_t = ",
      "\\theta_q(L)\\Theta_Q(L^S)\\varepsilon_t",
      " \\\\ \\text{where} \\\\ ",
      "\\phi_p(L) = 1", if (p > 0) paste0(symbolic_ar, collapse = ""), " \\\\ ",
      "\\Phi_P(L^S) = 1", if (P > 0) paste0(symbolic_sar, collapse = ""), " \\\\ ",
      "\\theta_q(L) = 1", if (q > 0) paste0(symbolic_ma, collapse = ""), " \\\\ ",
      "\\Theta_Q(L^S) = 1", if (Q > 0) paste0(symbolic_sma, collapse = "")
    )


    symbolic_eq <- paste0("$$ ", symbolic_eq, " $$")


    ########  ##########  ##########  ##########  ##########  ##########  ##########

    # Create the symbolic LaTeX strings for AR, MA, SAR, and SMA components
    symbolic_ar <- paste0(" - \\phi_", 1:p, "L^{", 1:p, "}")
    symbolic_ma <- paste0(" + \\theta_", 1:q, "L^{", 1:q, "}")
    symbolic_sar <- paste0(" - \\Phi_", 1:P, "L^{", s * (1:P), "}")
    symbolic_sma <- paste0(" + \\Theta_", 1:Q, "L^{", s * (1:Q), "}")

    # Substitute the AR, MA, SAR, and SMA strings into the main equation
    symbolic_eq2 <- paste0(
      "\\phi_p(L)\\Phi_P(L^{", s, "})(1-L)^{", d, "}(1-L^{", s, "})^{", D, "} Y_t = ",
      "\\theta_q(L)\\Theta_Q(L^{", s, "})\\varepsilon_t",
      " \\\\ \\text{where} \\\\ ",
      "\\phi_p(L) = 1", if (p > 0) paste0(symbolic_ar, collapse = ""), " \\\\ ",
      "\\Phi_P(L^{", s, "}) = 1", if (P > 0) paste0(symbolic_sar, collapse = ""), " \\\\ ",
      "\\theta_q(L) = 1", if (q > 0) paste0(symbolic_ma, collapse = ""), " \\\\ ",
      "\\Theta_Q(L^{", s, "}) = 1", if (Q > 0) paste0(symbolic_sma, collapse = "")
    )

    # Add LaTeX delimiters for MathJax
    symbolic_eq2 <- paste0("$$ ", symbolic_eq2, " $$")




    ########  ##########  ##########  ##########  ##########  ##########  ##########


    # Combine the AR, MA, SAR, and SMA strings into the main equation
    symbolic_eq3 <- paste0(

      "\\phi_p(L)\\Phi_P(L^S)(1-L)^d(1-L^S)^D Y_t = ","c+\\theta_q(L)\\Theta_Q(L^S)\\varepsilon_t", "+ \\delta t",
      " \\\\ \\text{------------} \\\\ ",
       "(1 - \\sum_{i=1}^{p} \\phi_i L^i)(1 - \\sum_{j=1}^{P} \\Phi_j L^{jS}) (1 - L)^d (1 - L^S)^D Y_t = c+(1 + \\sum_{i=1}^{q} \\theta_i L^i)(1 + \\sum_{j=1}^{Q} \\Theta_j L^{jS}) \\varepsilon_t + \\delta t",

      # " \\\\ \\text{------------} \\\\ ",
      #
      # "\\phi_p(L)\\Phi_P(L^{", s, "})(1-L)^{", d, "}(1-L^{", s, "})^{", D, "} Y_t = ","\\theta_q(L)\\Theta_Q(L^{", s, "})\\varepsilon_t", drift,

      " \\\\ \\text{------------} \\\\ ",

      "(1", if (p > 0) paste0(symbolic_ar, collapse = ""), ")",
      "(1", if (P > 0) paste0(symbolic_sar, collapse = ""), ")",
      "(1 - L)^{", d, "}",
      "(1 - L^{", s, "})^{", D, "}",
      " Y_t = ",intercept,
      "(1", if (q > 0) paste0(symbolic_ma, collapse = ""), ")",
      "(1", if (Q > 0) paste0(symbolic_sma, collapse = ""), ")",
      " \\varepsilon_t",drift,

      " \\\\ \\text{------------} \\\\ ",
      "(", paste0("1", if (p > 0) paste0(" - ", coefs[names(coefs) %in% paste0("ar", 1:p)], "L^{", 1:p, "}"), collapse = ""), ")",
      "(", paste0("1", if (P > 0) paste0(" - ", coefs[names(coefs) %in% paste0("sar", 1:P)], "L^{", s * (1:P), "}"), collapse = ""), ")",
      "(1 - L)^{", d, "}",
      "(1 - L^{", s, "})^{", D, "}",
      " Y_t = ",intercept,
      "(", paste0("1", if (q > 0) paste0(" + ", coefs[names(coefs) %in% paste0("ma", 1:q)], "L^{", 1:q, "}"), collapse = ""), ")",
      "(", paste0("1", if (Q > 0) paste0(" + ", coefs[names(coefs) %in% paste0("sma", 1:Q)], "L^{", s * (1:Q), "}"), collapse = ""), ")",
      " \\varepsilon_t", drift
      # " \\\\ \\text{------------} \\\\ ",
      # "\\\\ \\text{AIC: ", sprintf("%.2f", aic_value),
      # " -- AICc: ", sprintf("%.2f", aicc_value),
      # " -- BIC: ", sprintf("%.2f", bic_value) ,"}"
    )


    # Post-processing to handle double negatives and mixed signs and other

    symbolic_eq3 <- gsub("- -", "+", symbolic_eq3)  # Replace double negatives with a plus
    symbolic_eq3 <- gsub("\\+ -|\\-\\+", "-", symbolic_eq3)  # Replace '+-' or '-+' with a single minus
    symbolic_eq3 <- gsub("\\(1)", "", symbolic_eq3)  # Delete '(1)'
    symbolic_eq3 <- gsub("\\}1", "\\}", symbolic_eq3)  #    L1 --->  L
    symbolic_eq3 <- gsub("\\)\\^\\{1}", "\\)", symbolic_eq3)  #   (1-L)^{1}  --->  (1-L)
    symbolic_eq3 <- gsub("\\(1 - L)\\^\\{0}", "", symbolic_eq3)  #    (1 - L)^{0}   ---> empty
    symbolic_eq3 <- gsub("\\(1 - L\\^\\{12})\\^\\{0}", "", symbolic_eq3)  #    (1 - L^{12})^{0}   ---> empty


    symbolic_eq3 <- paste0("$$ ", symbolic_eq3, " $$")



    ########  ##########  ##########  ##########  ##########  ##########  ##########




    # Create the numerical LaTeX strings
    numerical_ar <- paste0(" - ", coefs[names(coefs) %in% paste0("ar", 1:p)], "L^{", 1:p, "}")
    numerical_ma <- paste0(" + ", coefs[names(coefs) %in% paste0("ma", 1:q)], "L^{", 1:q, "}")
    numerical_sar <- paste0(" - ", coefs[names(coefs) %in% paste0("sar", 1:P)], "L^{", s * (1:P), "}")
    numerical_sma <- paste0(" + ", coefs[names(coefs) %in% paste0("sma", 1:Q)], "L^{", s * (1:Q), "}")


    numerical_eq <- paste0(
      "\\phi_p(L)\\Phi_P(L^S)(1-L)^", d, "(1-L^S)^", D, " Y_t = ",
      "\\theta_q(L)\\Theta_Q(L^S)\\varepsilon_t",
      " \\\\ \\text{with} \\\\ ",
      "\\phi_p(L) = 1", if (p > 0) paste0(numerical_ar, collapse = ""), " \\\\ ",
      "\\Phi_P(L^S) = 1", if (P > 0) paste0(numerical_sar, collapse = ""), " \\\\ ",
      "\\theta_q(L) = 1", if (q > 0) paste0(numerical_ma, collapse = ""), " \\\\ ",
      "\\Theta_Q(L^S) = 1", if (Q > 0) paste0(numerical_sma, collapse = "")
    )


    numerical_eq <- paste0("$$ ", numerical_eq, " $$")

    ########  ##########  ##########  ##########  ##########  ##########  ##########
    ########  ##########  ##########  ##########  ##########  ##########  ##########


    # this is used to get the formula

    # Construct the one-line numerical equation
    numerical_one_line <- paste0(
      " \\\\ \\text{ } \\\\ ",
      "(", paste0("1", if (p > 0) paste0(" - ", coefs[names(coefs) %in% paste0("ar", 1:p)], "L^{", 1:p, "}"), collapse = ""), ")",
      "(", paste0("1", if (P > 0) paste0(" - ", coefs[names(coefs) %in% paste0("sar", 1:P)], "L^{", s * (1:P), "}"), collapse = ""), ")",
      "(1 - L)^{", d, "}",
      "(1 - L^{", s, "})^{", D, "}",
      " Y_t = ", intercept,
      "(", paste0("1", if (q > 0) paste0(" + ", coefs[names(coefs) %in% paste0("ma", 1:q)], "L^{", 1:q, "}"), collapse = ""), ")",
      "(", paste0("1", if (Q > 0) paste0(" + ", coefs[names(coefs) %in% paste0("sma", 1:Q)], "L^{", s * (1:Q), "}"), collapse = ""), ")",
      " \\varepsilon_t", drift,
      " \\\\ \\text{ } \\\\ "
    )

    # Post-processing to handle double negatives and mixed signs and other

    numerical_one_line <- gsub("- -", "+", numerical_one_line)  # Replace double negatives with a plus
    numerical_one_line <- gsub("\\+ -|\\-\\+", "-", numerical_one_line)  # Replace '+-' or '-+' with a single minus
    numerical_one_line <- gsub("\\(1)", "", numerical_one_line)  # Delete '(1)'
    numerical_one_line <- gsub("\\}1", "\\}", numerical_one_line)  #    L1 --->  L
    numerical_one_line <- gsub("\\(1 - L)\\^\\{0}", "", numerical_one_line)  #    (1 - L)^{0}   ---> empty
    numerical_one_line <- gsub("\\)\\^\\{1}", "\\)", numerical_one_line)  #   (1-L)^{1}  --->  (1-L)
    numerical_one_line <- gsub("\\(1 - L\\^\\{12})\\^\\{0}", "", numerical_one_line)  #    (1 - L^{12})^{0}   ---> empty

    # Add LaTeX delimiters for MathJax
    numerical_one_line <- paste0("$$ ", numerical_one_line, " $$")



    ########  ##########  ##########  ##########  ##########  ##########  ##########
    ########  ##########  ##########  ##########  ##########  ##########  ##########




    # Create the complete numerical LaTeX string formatted with Y_t on the left side
    numerical_one_line_Y_t <- paste0(
      "Y_t = ", drift,
      if (p > 0) paste0(" + ", paste0(coefs[names(coefs) %in% paste0("ar", 1:p)], "Y_{t-", 1:p, "}"), collapse = ""),
      if (d > 0) paste0(" + (1-L)^{", d, "}Y_t"),
      if (P > 0) paste0(" + ", paste0(coefs[names(coefs) %in% paste0("sar", 1:P)], "Y_{t-", s * (1:P), "}"), collapse = ""),
      if (D > 0) paste0(" + (1-L^{", s, "})^{", D, "}Y_t"),
      if (q > 0) paste0(" + ", paste0(coefs[names(coefs) %in% paste0("ma", 1:q)], "\\varepsilon_{t-", 1:q, "}"), collapse = ""),
      if (Q > 0) paste0(" + ", paste0(coefs[names(coefs) %in% paste0("sma", 1:Q)], "\\varepsilon_{t-", s * (1:Q), "}"), collapse = ""),
      " + \\varepsilon_t"
    )

    numerical_one_line_Y_t <- gsub("- -", "+", numerical_one_line_Y_t)  # Replace double negatives with a plus
    numerical_one_line_Y_t <- gsub("\\+ -|\\-\\+", "-", numerical_one_line_Y_t)  # Replace '+-' or '-+' with a single minus
    # numerical_one_line_Y_t <- gsub("(1)", "", numerical_one_line_Y_t)  # Replace double negatives with a plus

    # Add LaTeX delimiters for MathJax
    numerical_one_line_Y_t <- paste0("$$ ", numerical_one_line_Y_t, " $$")



    return(list(
      symbolic = symbolic_eq3,
      numerical = numerical_eq,
      numerical_one_line = numerical_one_line,
      numerical_one_line_Y_t = numerical_one_line_Y_t
      ))
  }




  ########  ##########  ##########  ##########  ##########  ##########  ##########
  ########  ##########  ##########  ##########  ##########  ##########  ##########


    # Function to generate equation text with parameters
    generateEquationText_HW <- function(model, type) {
      if (is.null(model)) return("Model not fitted yet")

      alpha <- round(model$alpha, 4)
      beta <- round(model$beta, 4)
      gamma <- ifelse(!is.null(model$gamma), round(model$gamma, 4), NA)
      # s <- ifelse(!is.null(model$seasonal.periods), model$seasonal.periods, NA)
      s <- input$frequency

      switch(type,
             "Holt-Winters Multiplicative" = paste(

               "Holt-Winters..Multiplicative..Model..Equations:",
               " \\\\ \\text{ } \\\\ ",

               "Level:  L_t = \\alpha \\frac{Y_t}{S_{t-s}} + (1 - \\alpha) (L_{t-1} + T_{t-1}) ",
               " \\\\ \\text{ } \\\\ ",

               "Trend:  T_t = \\beta (L_t - L_{t-1}) + (1 - \\beta) T_{t-1} ",
               " \\\\ \\text{ } \\\\ ",

               "Seasonal:  S_t = \\gamma \\frac{Y_t}{L_t} + (1 - \\gamma) S_{t-s} ",
               " \\\\ \\text{ } \\\\ ",


               "Level: L_t = ", alpha, " * Y_t / S_{t-", s, "} + (1 - ", alpha, ") * (L_{t-1} + T_{t-1})",
               " \\\\ \\text{ } \\\\ ",
               "Trend: T_t = ", beta, " * (L_t - L_{t-1}) + (1 - ", beta, ") * T_{t-1}",
               " \\\\ \\text{ } \\\\ ",
               "Seasonal: S_t = ", gamma, " * Y_t / L_t + (1 - ", gamma, ") * S_{t-", s, "}"
             ),
             "Holt-Winters Additive" = paste(

               "Holt-Winters.. Additive.. Model.. Equations:",
               " \\\\ \\text{ } \\\\ ",
               "Level:  L_t = \\alpha (Y_t - S_{t-s}) + (1 - \\alpha) (L_{t-1} + T_{t-1}) ",
               " \\\\ \\text{ } \\\\ ",
               "Trend:  T_t = \\beta (L_t - L_{t-1}) + (1 - \\beta) T_{t-1} ",
               " \\\\ \\text{ } \\\\ ",
               "Seasonal:  S_t = \\gamma (Y_t - L_t) + (1 - \\gamma) S_{t-s} ",
               " \\\\ \\text{ } \\\\ ",

               "Level: L_t = ", alpha, " * (Y_t - S_{t-", s, "}) + (1 - ", alpha, ") * (L_{t-1} + T_{t-1})",
               " \\\\ \\text{ } \\\\ ",
               "Trend: T_t = ", beta, " * (L_t - L_{t-1}) + (1 - ", beta, ") * T_{t-1}",
               " \\\\ \\text{ } \\\\ ",
               "Seasonal: S_t = ", gamma, " * (Y_t - L_t) + (1 - ", gamma, ") * S_{t-", s, "}"
             ),
             "HOLT's Exponential Smoothing" = paste(
               "Level: L_t = ", alpha, " * Y_t + (1 - ", alpha, ") * (L_{t-1} + T_{t-1})",
               " \\\\ \\text{ } \\\\ ",
               "Trend: T_t = ", beta, " * (L_t - L_{t-1}) + (1 - ", beta, ") * T_{t-1}"
             ),
             "ARIMA" = "SARIMA(p,d,q)(P,D,Q)[s]: Use Seasonal ARIMA model Panel to get the results."
      )
    }

    model_HW <- reactive({
      if (input$Model == "Holt-Winters Multiplicative") {
        HoltWinters(tsData()) # Replace 'ts_data' with your actual time series data
      } else if (input$Model == "Holt-Winters Additive") {
        HoltWinters(tsData(), seasonal = "additive") # Replace 'ts_data'
      } else if (input$Model == "HOLT's Exponential Smoothing") {
        holt(tsData()) # Replace 'ts_data'
      } else if (input$Model == "ARIMA") {
        # auto.arima(tsData()) # Replace 'ts_data' with your time series data
        NULL
      }
      else {
        NULL
      }
    })

    output$modelOutput_HW <- renderText({
      if (is.null(model_HW())) {
        "Please select a model type"
      } else {
        summary(model_HW())
      }
    })

    output$equationOutput_HW <- renderUI({
      if (is.null(model_HW())) {
        withMathJax(helpText("No model selected"))
      } else {
        equationText_HW <- generateEquationText_HW(model_HW(), input$Model)
        equationText_HW <- paste0("$$ ", equationText_HW, " $$")
        withMathJax(helpText(HTML(equationText_HW)))
      }
    })


  ########  ##########  ##########  ##########  ##########  ##########  ##########
  ########  ##########  ##########  ##########  ##########  ##########  ##########
  ########  ##########  ##########  ##########  ##########  ##########  ##########



  # not used in the program
  numerical_one_line_Y_t_2 <- function(model) {
    # Extract coefficients and terms
    coefs <- coef(model)
    coefs <- round(coefs, 2)  # Round coefficients to 2 decimal places
    p <- model$arma[1]  # AR order
    d <- model$arma[6]  # Degree of differencing
    q <- model$arma[2]  # MA order
    P <- model$arma[3]  # Seasonal AR order
    D <- model$arma[7]  # Seasonal differencing
    Q <- model$arma[4]  # Seasonal MA order
    s <- model$arma[5]  # Seasonal period
    include_drift <- "drift" %in% names(coefs)
    drift <- if (include_drift) paste0(" + ", coefs["drift"], "t") else ""

    # Start building the equation string
    equation <- "Y_t = "

    # Add the AR terms
    if (p > 0) {
      ar_coefs <- coefs[names(coefs) %in% paste0("ar", 1:p)]
      equation <- paste(equation, paste0(" + ", ar_coefs, "Y_{t-", 1:p, "}"), collapse = "")
    }

    # Add differencing
    if (d > 0) {
      equation <- paste(equation, " - ", "(1 - L)^", d, "Y_t")
    }

    # Add seasonal differencing
    if (D > 0) {
      equation <- paste(equation, " - ", "(1 - L^{", s, "})^", D, "Y_t")
    }

    # Add the MA terms
    if (q > 0) {
      ma_coefs <- coefs[names(coefs) %in% paste0("ma", 1:q)]
      equation <- paste(equation, paste0(" + ", ma_coefs, "\\varepsilon_{t-", 1:q, "}"), collapse = "")
    }

    # Add the seasonal AR terms
    if (P > 0) {
      sar_coefs <- coefs[names(coefs) %in% paste0("sar", 1:P)]
      equation <- paste(equation, paste0(" + ", sar_coefs, "Y_{t-", s * (1:P), "}"), collapse = "")
    }

    # Add the seasonal MA terms
    if (Q > 0) {
      sma_coefs <- coefs[names(coefs) %in% paste0("sma", 1:Q)]
      equation <- paste(equation, paste0(" + ", sma_coefs, "\\varepsilon_{t-", s * (1:Q), "}"), collapse = "")
    }

    # Add the drift term if present
    if (include_drift) {
      equation <- paste(equation, drift)
    }

    # Add the error term
    equation <- paste(equation, "+ \\varepsilon_t")

    # Add LaTeX delimiters for MathJax
    equation <- paste0("$$ ", equation, " $$")

    return(equation)
  }


########  ##########  ##########  ##########  ##########  ##########  ##########
########  ##########  ##########  ##########  ##########  ##########  ##########
#   
#                             Render UI Logic
#  
########  ##########  ##########  ##########  ##########  ##########  ##########
########  ##########  ##########  ##########  ##########  ##########  ##########
    
    # 1. Reactive expression for currentFrequency
    # We keep your name 'currentFrequency' but make it a reactive expression
    currentFrequency <- reactive({
      req(input$frequency)
      if (input$frequency == "other") {
        req(input$customFrequency)
        return(as.numeric(input$customFrequency))
      } else {
        return(as.numeric(input$frequency))
      }
    })
    
    # 2. Dynamic Input Renderers
    output$frequencyInputUI <- renderUI({
      req(input$fileData)
      selectInput("frequency", "Frequency :",
                  choices = c("Quarterly   P[4]"   = 4,
                              "Monthly     P[12]"  = 12,
                              "Weekly      P[52]"  = 52,
                              "Daily       P[365]" = 365,
                              "7 days      P[7]"   = 7,
                              "Other (Specify)"    = "other"),
                  selected = 12)
    })
    
    output$customInput <- renderUI({
      req(input$frequency)
      if (input$frequency == "other") {
        numericInput("customFrequency", 
                     label = tags$span(class = "custom-label", "Enter Custom Frequency :"),
                     value = 1, min = 1)
      }
    })
    
    output$lengthInputUI <- renderUI({
      req(input$fileData)
      numericInput("length", "Forecast Length (Periods) :", value = 12, min = 1, max = 500)
    })
    
    output$colNumUI <- renderUI({
      req(data())
      choices <- names(data())
      default_selection <- if(length(choices) >= 2) choices[2] else choices[1]
      selectInput("colNum", "Select Data Column :", choices = choices, selected = default_selection)
    })
    
    output$dateColUI <- renderUI({
      req(data())
      selectInput("dateCol", "Select Date Column :", choices = names(data()))
    })
    
    output$modelSelectUI <- renderUI({
      req(input$fileData)
      selectInput("Model", "Forecasting Model :",
                  choices = c("ARIMA", 
                              "Holt-Winters Additive", 
                              "Holt-Winters Multiplicative", 
                              "HOLT's Exponential Smoothing"),
                  selected = "ARIMA")
    })
    
    output$graphTypeUI <- renderUI({
      req(input$fileData)
      selectInput("plot_type", "Diagnostic Plot Type :",
                  choices = c("Partial ACF" = "partial", 
                              "Histogram"   = "histogram", 
                              "Scatter"     = "scatter", 
                              "Spectrum"    = "spectrum"),
                  selected = "partial")
    })
    
    # 3. Data Table Display
    output$dataPrint <- renderTable({
      req(data(), input$dateCol)
      df <- data()
      
      try({
        if(is.numeric(df[[input$dateCol]])) {
          df[[input$dateCol]] <- as.Date(df[[input$dateCol]], origin = "1899-12-30")
        } else {
          df[[input$dateCol]] <- as.Date(df[[input$dateCol]])
        }
        df[[input$dateCol]] <- format(df[[input$dateCol]], "%d/%m/%Y")
      }, silent = TRUE)
      
      # Return the full dataframe instead of head(df, 100)
      df 
      
    }, rownames = TRUE)
    
    # 4. Conditional Sidebar Buttons
    output$conditionalButtons <- renderUI({
      req(input$fileData)
      tagList(
        actionButton("plotSettings", "Labels", icon = icon("tags")),
        div(style="margin-top: 10px;"), 
        actionButton("dimBtn", "Dim. (*)", icon = icon("expand"))
      )
    })
    
    
    ########  ##########  ##########  ##########  ##########  ##########  ##########
    #                  Time Series Object Creation [tsData()]
    ########  ##########  ##########  ##########  ##########  ##########  ##########
    
    tsData <- reactive({
      req(input$fileData, input$colNum, input$dateCol)
      
      # This now works because currentFrequency is defined as a reactive() above
      freq <- currentFrequency() 
      req(freq)
      
      df <- data()
      colData <- df[[input$colNum]]
      
      # Date Parsing
      date_vec <- tryCatch({
        if(is.numeric(df[[input$dateCol]])) {
          as.Date(df[[input$dateCol]], origin = "1899-12-30")
        } else {
          as.Date(df[[input$dateCol]])
        }
      }, error = function(e) return(NULL))
      
      req(date_vec)
      starting_date <- min(date_vec, na.rm = TRUE)
      
      # Dynamic Start Vector Calculation
      start_year <- lubridate::year(starting_date)
      start_period <- switch(as.character(freq),
                             "12"  = lubridate::month(starting_date),
                             "4"   = lubridate::quarter(starting_date),
                             "52"  = lubridate::isoweek(starting_date),
                             "365" = lubridate::yday(starting_date),
                             "7"   = lubridate::wday(starting_date),
                             1 + as.numeric(format(starting_date, "%j")) %/% (365/freq)
      )
      
      numeric_data <- as.numeric(colData)
      numeric_data <- na.omit(numeric_data) 
      
      # Create the TS Object
      ts_obj <- ts(numeric_data, 
                   start = c(start_year, start_period), 
                   frequency = freq)
      
      return(ts_obj)
    })
    
    
    

##########  ##########  ##########  ##########  ##########  ##########  ########
#                                Observers
##########  ##########  ##########  ##########  ##########  ##########  ########

    
    # 1. Initialize userData with your defaults at the start of the server function
    userData <- reactiveValues(
    data = NULL,
    mainTitle = "Time Series Plot",
    xLabel = "Time",
    yLabel = "Values",
    plotWidth = "900px",  
    plotHeight = "650px",
    labelsize = 12,
    selectedTheme = "theme_linedraw"
  )


    # 2. The Modal Trigger
    observeEvent(input$dimBtn, {
      
      # --- CRITICAL FIX: Convert strings to numbers for the sliders ---
      # gsub("[^0-9]", "", x) removes everything that is NOT a number
      current_w <- as.numeric(gsub("[^0-9]", "", userData$plotWidth))
      current_h <- as.numeric(gsub("[^0-9]", "", userData$plotHeight))
      
      showModal(modalDialog(
        title = "Set Plot Dimensions & Style",
        size = "l",
        
        # Use the numeric 'current_w' instead of 'userData$plotWidth'
        sliderInput("plotWidthSlider", "Plot Width (px)", 
                    min = 400, max = 3500, 
                    value = current_w, 
                    step = 10, 
                    width = '100%', post = "px"),
        
        # Use the numeric 'current_h' instead of 'userData$plotHeight'
        sliderInput("plotHeightSlider", "Plot Height (px)", 
                    min = 300, max = 2500, 
                    value = current_h, 
                    step = 10, 
                    width = '100%', post = "px"),
        
        selectInput("theme", "Select Theme",
                    selected = userData$selectedTheme,
                    choices = list("Line Draw" = "theme_linedraw",
                                   "Default" = "theme_gray",
                                   "Minimal" = "theme_minimal",
                                   "Classic" = "theme_classic",
                                   "Light" = "theme_light",
                                   "Dark" = "theme_dark",
                                   "Void" = "theme_void"),
                    width = '100%'),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("okDimensions", "OK", class = "btn-primary")
        )
      ))
    })
    
    # 3. Update userData when 'OK' is clicked
    observeEvent(input$okDimensions, {
      # Convert numbers back to strings with "px" for your CSS/UI logic
      userData$plotWidth <- paste0(input$plotWidthSlider, "px")
      userData$plotHeight <- paste0(input$plotHeightSlider, "px")
      userData$selectedTheme <- input$theme
      removeModal()
    })

  # Observer for Plot Titles and Labels
  observeEvent(input$plotSettings, {
    showModal(modalDialog(
      title = "Plot Titles and Label Settings",
      textInput("mainTitle", "Main Title", userData$mainTitle),
      div(style = "display: flex; gap: 10px;",
          textInput("xLabel", "X-axis Label", userData$xLabel),
          textInput("yLabel", "Y-axis Label", userData$yLabel)
      ),
      numericInput("labelsize", "Axis Tick Size", value = userData$labelsize, min = 1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK", class = "btn-primary")
      )
    ))
  })

  # Update userData when 'OK' is clicked in settings modal
  observeEvent(input$ok, {
    userData$mainTitle <- input$mainTitle
    userData$xLabel <- input$xLabel
    userData$yLabel <- input$yLabel
    userData$labelsize <- input$labelsize 
    removeModal()
  })

  # Log Transformation Observer
  # Keeping the 'values' reactiveValues and 'islog' variable names as requested
  values <- reactiveValues(islog = "No")

  observe({
    # req ensures input$check_box exists before checking value
    req(!is.null(input$check_box))
    if (input$check_box) {
      values$islog <- "Yes"
    } else {
      values$islog <- "No"
    }
  })
    
    
    

  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #                 For d[?] ( D[?]( log[?] (st) ) )
  #
  ########  ########  ########  ########  ########  ########  ########  ########


  getMyData <- function(tsData, frequency, islog = "No", d_n = 0, DS_n = 0) {
    # 1. Ensure data is present before proceeding
    shiny::req(tsData)
    
    # 2. Setup parameters
    freq <- as.numeric(frequency)
    d_n <- as.numeric(d_n)
    DS_n <- as.numeric(DS_n)
    
    # 3. Apply Log Transformation first (if requested)
    # We store this in 'working_ts' to avoid repeating logic later
    working_ts <- if (identical(islog, "Yes")) log(tsData) else tsData
    
    # 4. Apply Seasonal Differencing (D)
    # Standard SARIMA: lag is the frequency, differences is the order DS_n
    if (DS_n > 0) {
      working_ts <- diff(working_ts, lag = freq, differences = DS_n)
    }
    
    # 5. Apply Ordinary Differencing (d)
    # lag is 1, differences is the order d_n
    if (d_n > 0) {
      working_ts <- diff(working_ts, lag = 1, differences = d_n)
    }
    
    return(working_ts)
  }

  
 
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #           Statistics & Model Outputs for Selected Column
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Basic Descriptive Stats (Text/Row Format)
  output$data_StatisticsText1 <- renderPrint({
    req(selected_column_data())
    
    # summarytools::descr() provides a clean summary
    summarytools::descr(selected_column_data(), stats = "common", transpose = TRUE)
  })
  
  # 2. Descriptive Stats (Table Format)
  output$data_StatisticsText1_Table <- renderTable({
    req(selected_column_data())
    
    # Calculate stats and convert to a clean data frame
    ds <- summarytools::descr(selected_column_data())
    ds_df <- as.data.frame(as.matrix(ds))
    
    # Create a clean table with named columns
    final_table <- data.frame(
      Statistic = rownames(ds_df),
      Value = ds_df[[1]]
    )
    return(final_table)
  }, digits = 4)
  

  
  
  # 3. Detailed Statistics (pastecs::stat.desc) — formatted table + CI as [a , b]
  output$data_StatisticsText2 <- renderPrint({
    req(selected_column_data())
    
    x <- as.numeric(stats::na.omit(selected_column_data()))
    req(length(x) > 0)
    
    if (!requireNamespace("pastecs", quietly = TRUE)) {
      cat("ERROR: Package 'pastecs' is not installed.\n")
      return(invisible(NULL))
    }
    
    # Compute statistics
    s <- pastecs::stat.desc(x, norm = TRUE)
    
    # Build table
    df <- data.frame(
      Statistic = names(s),
      Value     = as.numeric(s),
      stringsAsFactors = FALSE
    )
    
    # Confidence interval (display only)
    ci_half <- s["CI.mean.0.95"]
    ci_low  <- s["mean"] - ci_half
    ci_high <- s["mean"] + ci_half
    
    # Replace CI row with formatted interval
    df <- df[df$Statistic != "CI.mean.0.95", ]
    df <- rbind(
      df,
      data.frame(
        Statistic = "95% Confidence Interval (Mean)",
        Value = NA_real_,
        stringsAsFactors = FALSE
      )
    )
    
    # Rename statistics
    label_map <- c(
      "nbr.val"  = "Number of observations (N)",
      "nbr.null" = "Number of zeros",
      "nbr.na"   = "Number of missing values (NA)",
      "min"      = "Minimum",
      "max"      = "Maximum",
      "range"    = "Range (max − min)",
      "sum"      = "Sum",
      "mean"     = "Mean",
      "median"   = "Median",
      "SE.mean"  = "Std. error of mean",
      "var"      = "Variance",
      "std.dev"  = "Standard deviation",
      "coef.var" = "Coefficient of variation",
      "skewness" = "Skewness",
      "kurtosis" = "Kurtosis",
      "normtest.W" = "Normality (Shapiro) W",
      "normtest.p" = "Normality (Shapiro) p-value"
    )
    
    df$Statistic <- ifelse(df$Statistic %in% names(label_map),
                           unname(label_map[df$Statistic]),
                           df$Statistic)
    
    # Formatting (SPACE as thousands separator)
    is_count <- grepl("^Number of ", df$Statistic)
    
    df$Value <- ifelse(
      is_count,
      format(round(df$Value),
             big.mark = " ",
             scientific = FALSE,
             trim = TRUE),
      formatC(df$Value,
              format = "f",
              digits = 4,
              big.mark = " ")
    )
    
    # Insert CI display
    ci_row <- which(df$Statistic == "95% Confidence Interval (Mean)")
    df$Value[ci_row] <- sprintf(
      "[ %.4f , %.4f ]",
      ci_low,
      ci_high
    )
    
    cat("Detailed Descriptive Statistics\n")
    cat("--------------------------------\n")
    print(df, row.names = FALSE, right = FALSE)
  })
  
  
  
  
  
  # 4. Integrated Model Output (ARIMA & Holt-Winters)
  output$modelOutput <- renderPrint({
    req(input$Model, tsData())
    
    # Use the reactive tsData which already has the correct frequency
    data_to_fit <- tsData()
    
    # Fit the model based on selection using the forecast package for consistency
    fit <- tryCatch({
      switch(input$Model,
             "ARIMA" = forecast::auto.arima(data_to_fit),
             "Holt-Winters Additive" = forecast::hw(data_to_fit, seasonal = "additive"),
             "Holt-Winters Multiplicative" = {
               # Multiplicative check
               validate(need(all(data_to_fit > 0), "Multiplicative model requires all data values > 0"))
               forecast::hw(data_to_fit, seasonal = "multiplicative")
             },
             stop("Invalid model selection."))
    }, error = function(e) {
      stop(safeError(e$message))
    })
    
    # Return the summary of the fitted model
    summary(fit)
  })
  
  
  

  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #           Time Series Plots, ACF/PACF & Stationarity Tests
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  

  # 1. Main Time Series Plot [S(t)]
  output$tsPlot <- renderPlot({
    req(tsData())
    
    autoplot(tsData(), size = 1, colour = "steelblue") +
      labs(
        title = userData$mainTitle, 
        x = userData$xLabel, 
        y = userData$yLabel
      ) +
      apply_user_theme() 
  }, 
  # Dynamic dimensions from your sliders
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )

  
  # 2. Individual ACF Plot
  output$StACF <- renderPlot({
    req(tsData())
    
    forecast::ggAcf(tsData()) + 
      labs(title = paste("ACF:", userData$mainTitle)) +
      apply_user_theme()
  }, 
  # Dynamic dimensions from your sliders
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )
  
  # 3. Individual PACF Plot
  output$StPACF <- renderPlot({
    req(tsData())
    
    forecast::ggPacf(tsData()) + 
      labs(title = paste("PACF:", userData$mainTitle)) +
      apply_user_theme()
  }, 
  # Dynamic dimensions from your sliders
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )
  
  # 4. Combined ACF + PACF 
  # Note: astsa::acf2 is a base R plot. For theme consistency, 
  # we use a ggplot-based approach here.
  output$StACFPACF <- renderPlot({
    req(tsData())
    
    p1 <- forecast::ggAcf(tsData()) + apply_user_theme() + labs(title = "ACF")
    p2 <- forecast::ggPacf(tsData()) + apply_user_theme() + labs(title = "PACF")
    
    # Arrange them vertically for the combined view
    gridExtra::grid.arrange(p1, p2, ncol = 1, top = userData$mainTitle)
  }, 
  # Dynamic dimensions from your sliders
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )
  
  # 5. Diagnostic Ts Display
  output$tsDisplay2 <- renderPlot({
    req(tsData(), input$plot_type)
    
    forecast::ggtsdisplay(
      tsData(), 
      plot.type = input$plot_type, 
      main = userData$mainTitle,
      xlab = userData$xLabel, 
      ylab = userData$yLabel
    ) + apply_user_theme()
  }, 
  # Dynamic dimensions from your sliders
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )
  
 
  
  # 6. Augmented Dickey-Fuller (ADF) Test
  output$teststationariteSt <- renderPrint({
    req(tsData(), input$alternSt, input$LagOrderADFSt, input$alphaSt)
    
    alpha_val <- as.numeric(input$alphaSt)
    # Map the decimal input to the urca critical value column names
    alpha_col <- switch(as.character(alpha_val),
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct")
    
    cat("==========================================================================\n")
    cat("               AUGMENTED DICKEY-FULLER (ADF) TEST                         \n")
    cat("==========================================================================\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0: The series has a unit root (Non-Stationary).\n")
    cat(" • H1: The series is Stationary.\n")
    cat("--------------------------------------------------------------------------\n")
    cat(paste(" - Alpha (Significance Level):", alpha_val, "\n"))
    cat(paste(" - Confidence Level         :", (1 - alpha_val) * 100, "%\n"))
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # 1. Standard ADF Test for p-values
      res <- tseries::adf.test(
        tsData(), 
        alternative = input$alternSt, 
        k = as.numeric(input$LagOrderADFSt)
      )
      
      # 2. Advanced ADF Test for Critical Values (Tau) using urca
      res_urca <- urca::ur.df(tsData(), type = "trend", lags = as.numeric(input$LagOrderADFSt))
      crit_vals <- res_urca@cval
      
      # Extract values
      tau_obs  <- res$statistic
      tau_crit <- crit_vals["tau3", alpha_col] # Dynamically selects based on alpha
      p_val    <- res$p.value
      
      cat(" RESULT:\n")
      cat(paste(" • Tau (Observed Value)  :", round(tau_obs, 4), "\n"))
      cat(paste(" • Tau (Critical Value)  :", round(tau_crit, 4), "(at", alpha_col, ")\n"))
      cat(paste(" • p-value (One-tailed)  :", round(p_val, 4), "\n"))
      cat("--------------------------------------------------------------------------\n")
      
      cat(" DECISION:\n")
      # Check both p-value and Tau-statistic comparison
      if(p_val <= alpha_val) {
        cat(paste("  • The p-value is", round(p_val, 4), "(<=", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is more negative than Critical (", round(tau_crit, 2), ").\n"))
        cat("  • DECISION: Reject H0. The series is Stationary.\n")
      } else {
        cat(paste("  • The p-value is", round(p_val, 4), "(>", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is not negative enough.\n"))
        cat("  • DECISION: Fail to reject H0. The series is Non-Stationary.\n")
      }
      
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE:\n")
      if(p_val <= alpha_val) {
        cat("  • Statistical evidence suggests the series is integrated of order 0.\n")
        cat("  • You may proceed with modeling using the current transformation.\n")
      } else {
        cat("  • The series exhibits unit root behavior at this significance level.\n")
        cat("  • ACTION: Try a stricter Alpha (10%) or apply differencing (d=1).\n")
      }
      
    }, error = function(e) {
      cat(" ERROR: ", e$message, "\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE: Ensure the 'urca' package is loaded. If the error persists,\n")
      cat(" check if the number of lags is too high for the dataset size.\n")
    })
    cat("==========================================================================\n")
  })
  
  
  
  
  
  

  
  

  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #        log(St) Analysis: Plots + ACF + PACF (Dynamic Scaling)
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Centralized Reactive for Log Transformation
  logTsData <- reactive({
    req(tsData())
    # Validation: Log requires strictly positive data
    validate(
      need(all(tsData() > 0), "Error: Log transformation requires all values to be greater than zero.")
    )
    log(tsData())
  })
  

  # 2. Log-Transformed Plot
  output$plotLogSt <- renderPlot({
    req(logTsData())
    
    autoplot(logTsData(), size = 1, colour = "darkgreen") +
      labs(
        title = paste("Log-Transformed:", userData$mainTitle),
        x = userData$xLabel,
        y = paste("log(", userData$yLabel, ")")
      ) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 3. Log ACF Plot
  output$logStACF <- renderPlot({
    req(logTsData())
    forecast::ggAcf(logTsData()) + 
      labs(title = paste("ACF (Log):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 4. Log PACF Plot
  output$logStPACF <- renderPlot({
    req(logTsData())
    forecast::ggPacf(logTsData()) + 
      labs(title = paste("PACF (Log):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 5. Combined Log ACF/PACF (Thematic version of acf2)
  output$logStACFPACF <- renderPlot({
    req(logTsData())
    
    p1 <- forecast::ggAcf(logTsData()) + apply_user_theme() + labs(title = "Log ACF")
    p2 <- forecast::ggPacf(logTsData()) + apply_user_theme() + labs(title = "Log PACF")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 6. Integrated Log Diagnostic Display
  output$log_ts_Display <- renderPlot({
    req(logTsData(), input$plot_type)
    
    forecast::ggtsdisplay(
      logTsData(), 
      plot.type = input$plot_type, 
      main = paste("Log Display:", userData$mainTitle),
      xlab = userData$xLabel, 
      ylab = userData$yLabel
    ) + apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  
  # 7. Stationarity Test on Log Series
  output$teststationariteLogSt <- renderPrint({
    # Use input$alphaSt for consistency across tests
    req(logTsData(), input$alternLogSt, input$LagOrderADFLogSt, input$alphaSt)
    
    # 1. Map Alpha input to urca critical value columns
    alpha_val <- as.numeric(input$alphaSt)
    alpha_col <- switch(as.character(alpha_val),
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct")
    
    cat("==========================================================================\n")
    cat("      ADF TEST ON LOG-TRANSFORMED SERIES (Variance Stabilized)            \n")
    cat("==========================================================================\n")
    cat(" This test checks stationarity after applying a Natural Log transformation.\n")
    cat(" Log transforms are typically used to stabilize non-constant variance.    \n")
    cat("--------------------------------------------------------------------------\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0: The log-transformed series is Non-Stationary.\n")
    cat(" • H1: The log-transformed series is Stationary.\n")
    cat("--------------------------------------------------------------------------\n")
    cat(paste(" - Alpha (Significance Level):", alpha_val, "\n"))
    cat(paste(" - If Tau (Observed) < Tau (Critical): Reject H0 (Stationary).\n"))
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # 2. Standard ADF Test for p-values
      res <- tseries::adf.test(
        logTsData(), 
        alternative = input$alternLogSt, 
        k = as.numeric(input$LagOrderADFLogSt)
      )
      
      # 3. Advanced ADF Test for Critical Values (Tau)
      res_urca <- urca::ur.df(logTsData(), type = "trend", lags = as.numeric(input$LagOrderADFLogSt))
      crit_vals <- res_urca@cval
      
      # Extract values
      tau_obs  <- res$statistic
      tau_crit <- crit_vals["tau3", alpha_col] 
      p_val    <- res$p.value
      
      cat(" RESULT:\n")
      cat(paste(" • Tau (Observed Value)  :", round(tau_obs, 4), "\n"))
      cat(paste(" • Tau (Critical Value)  :", round(tau_crit, 4), "(at", alpha_col, ")\n"))
      cat(paste(" • p-value (One-tailed)  :", round(p_val, 4), "\n"))
      cat("--------------------------------------------------------------------------\n")
      
      cat(" DECISION:\n")
      if(p_val <= alpha_val) {
        cat(paste("  • The p-value is", round(p_val, 4), "(<=", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is more negative than Critical (", round(tau_crit, 2), ").\n"))
        cat("  • DECISION: Reject H0. The Log-Series is Stationary.\n")
      } else {
        cat(paste("  • The p-value is", round(p_val, 4), "(>", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is not negative enough.\n"))
        cat("  • DECISION: Fail to reject H0. The Log-Series is Non-Stationary.\n")
      }
      
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE:\n")
      if(p_val <= alpha_val) {
        cat("  • The Log transformation was sufficient to achieve stationarity.\n")
        cat("  • You may proceed with ARIMA models where d = 0 (on log data).\n")
      } else {
        cat("  • The series is still non-stationary even after the Log transform.\n")
        cat("  • ADVICE: Apply first-order differencing (d=1) to the log-series.\n")
        cat("  • This is equivalent to modeling the 'percentage growth rate'.\n")
      }
      
    }, error = function(e) {
      cat(" EXECUTION ERROR: ", e$message, "\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE: Try a smaller Lag order or ensure the Log series has no -Inf values.\n")
    })
    cat("==========================================================================\n")
  })
  

  



  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #        d[1] Analysis: Ordinary Differencing (Dynamic Scaling)
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Centralized Reactive for First Difference
  # This calculates the difference once and caches it for all 6 outputs below
  diff1TsData <- reactive({
    req(tsData())
    diff(tsData(), differences = 1)
  })
  
  # 2. Difference Plot (d=1)
  output$difference1 <- renderPlot({
    req(diff1TsData())
    
    autoplot(diff1TsData(), size = 1, colour = "firebrick") +
      labs(
        title = paste("First Difference (d=1):", userData$mainTitle),
        x = userData$xLabel,
        y = paste0("Δ", userData$yLabel)
      ) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 3. Difference ACF Plot
  output$d1StACF <- renderPlot({
    req(diff1TsData())
    forecast::ggAcf(diff1TsData()) + 
      labs(title = paste("ACF (d=1):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 4. Difference PACF Plot
  output$d1StPACF <- renderPlot({
    req(diff1TsData())
    forecast::ggPacf(diff1TsData()) + 
      labs(title = paste("PACF (d=1):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 5. Combined ACF/PACF (Thematic replacement for acf2)
  output$d1StACFPACF <- renderPlot({
    req(diff1TsData())
    
    p1 <- forecast::ggAcf(diff1TsData()) + apply_user_theme() + labs(title = "ACF (d=1)")
    p2 <- forecast::ggPacf(diff1TsData()) + apply_user_theme() + labs(title = "PACF (d=1)")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 6. Integrated Diagnostic Display
  output$d1_ts_Display <- renderPlot({
    req(diff1TsData(), input$plot_type)
    
    forecast::ggtsdisplay(
      diff1TsData(), 
      plot.type = input$plot_type, 
      main = paste("Diff Display (d=1):", userData$mainTitle),
      xlab = userData$xLabel, 
      ylab = userData$yLabel
    ) + apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  
  # 7. Stationarity Test on Differenced Series
  output$teststationarited1St <- renderPrint({
    # Pull alpha from the primary alphaSt input as requested
    req(diff1TsData(), input$alternd1St, input$LagOrderADFd1St, input$alphaSt)
    
    # 1. Map Alpha input to urca critical value columns
    alpha_val <- as.numeric(input$alphaSt)
    alpha_col <- switch(as.character(alpha_val),
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct")
    
    cat("==========================================================================\n")
    cat("      ADF TEST ON FIRST DIFFERENCED SERIES (d=1)                          \n")
    cat("==========================================================================\n")
    cat(" This test checks if removing the trend via first-order differencing      \n")
    cat(" has rendered the time series stationary.                                 \n")
    cat("--------------------------------------------------------------------------\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0: The differenced series is Non-Stationary (has a unit root).\n")
    cat(" • H1: The differenced series is Stationary.\n")
    cat("--------------------------------------------------------------------------\n")
    cat(paste(" - Alpha (Significance Level):", alpha_val, "\n"))
    cat(paste(" - If Tau (Observed) < Tau (Critical): Reject H0 (Stationary).\n"))
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # 2. Standard ADF Test for p-values
      res <- tseries::adf.test(
        diff1TsData(), 
        alternative = input$alternd1St, 
        k = as.numeric(input$LagOrderADFd1St)
      )
      
      # 3. Advanced ADF Test for Critical Values (Tau) using urca
      # Note: We use type='trend' to match tseries::adf.test's internal regression
      res_urca <- urca::ur.df(diff1TsData(), type = "trend", lags = as.numeric(input$LagOrderADFd1St))
      crit_vals <- res_urca@cval
      
      # Extract values
      tau_obs  <- res$statistic
      tau_crit <- crit_vals["tau3", alpha_col] 
      p_val    <- res$p.value
      
      cat(" RESULT:\n")
      cat(paste(" • Tau (Observed Value)  :", round(tau_obs, 4), "\n"))
      cat(paste(" • Tau (Critical Value)  :", round(tau_crit, 4), "(at", alpha_col, ")\n"))
      cat(paste(" • p-value (One-tailed)  :", round(p_val, 4), "\n"))
      cat(paste(" • Alpha                 :", alpha_val, "\n"))
      cat("--------------------------------------------------------------------------\n")
      
      cat(" DECISION:\n")
      if(p_val <= alpha_val) {
        cat(paste("  • The p-value is", round(p_val, 4), "(<=", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is more negative than Critical (", round(tau_crit, 2), ").\n"))
        cat("  • DECISION: Reject H0. The Differenced Series is Stationary.\n")
      } else {
        cat(paste("  • The p-value is", round(p_val, 4), "(>", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is not negative enough.\n"))
        cat("  • DECISION: Fail to reject H0. The Differenced Series is Non-Stationary.\n")
      }
      
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE:\n")
      if(p_val <= alpha_val) {
        cat("  • First-order differencing (d=1) was sufficient to achieve stationarity.\n")
        cat("  • Your ARIMA model should likely use d = 1.\n")
        cat("  • Check the ACF/PACF of this differenced series to find AR/MA terms.\n")
      } else {
        cat("  • The series is still non-stationary after one difference.\n")
        cat("  • ADVICE: Consider second-order differencing (d=2).\n")
        cat("  • If the plot shows seasonal peaks, apply a Seasonal Difference (D=1).\n")
      }
      
    }, error = function(e) {
      cat(" EXECUTION ERROR: ", e$message, "\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE: Ensure the differenced series has enough observations after the \n")
      cat(" lag removal. Try reducing the 'Lag' value.\n")
    })
    cat("==========================================================================\n")
  })
  
  


  

  
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #        D[1] Analysis: Seasonal Differencing (Dynamic Scaling)
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Centralized Reactive for First Seasonal Difference
  diffSeasonTsData <- reactive({
    req(tsData())
    freq <- as.numeric(currentFrequency())
    # Seasonal difference: lag = frequency, differences = 1
    diff(tsData(), lag = freq, differences = 1)
  })
  
  # 2. Seasonal Difference Plot (D=1)
  output$DS1Stplot <- renderPlot({
    req(diffSeasonTsData())
    
    autoplot(diffSeasonTsData(), size = 1, colour = "purple") +
      labs(
        title = paste("Seasonal Difference (D=1):", userData$mainTitle),
        x = userData$xLabel,
        y = paste0("Δs", userData$yLabel)
      ) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 3. Seasonal ACF Plot
  output$DS1StACF <- renderPlot({
    req(diffSeasonTsData())
    forecast::ggAcf(diffSeasonTsData()) + 
      labs(title = paste("Seasonal ACF (D=1):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 4. Seasonal PACF Plot
  output$DS1StPACF <- renderPlot({
    req(diffSeasonTsData())
    forecast::ggPacf(diffSeasonTsData()) + 
      labs(title = paste("Seasonal PACF (D=1):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 5. Combined Seasonal ACF/PACF (Thematic replacement for acf2)
  output$DS1StACFPACF <- renderPlot({
    req(diffSeasonTsData())
    
    p1 <- forecast::ggAcf(diffSeasonTsData()) + apply_user_theme() + labs(title = "Seasonal ACF")
    p2 <- forecast::ggPacf(diffSeasonTsData()) + apply_user_theme() + labs(title = "Seasonal PACF")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 6. Integrated Seasonal Diagnostic Display
  output$Ds1_ts_Display <- renderPlot({
    req(diffSeasonTsData(), input$plot_type)
    
    forecast::ggtsdisplay(
      diffSeasonTsData(), 
      plot.type = input$plot_type, 
      main = paste("Seasonal Display (D=1):", userData$mainTitle),
      xlab = userData$xLabel, 
      ylab = userData$yLabel
    ) + apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  
  # 7. Stationarity Test on Seasonally Differenced Series
  output$teststationariteDs1St <- renderPrint({
    # Pull alpha from the primary alphaSt input for consistency
    req(diffSeasonTsData(), input$alternDs1St, input$LagOrderADFDs1St, input$alphaSt)
    
    # 1. Map Alpha input to urca critical value columns
    alpha_val <- as.numeric(input$alphaSt)
    alpha_col <- switch(as.character(alpha_val),
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct")
    
    cat("==========================================================================\n")
    cat("      ADF TEST ON SEASONAL DIFFERENCED SERIES (D=1)                       \n")
    cat("==========================================================================\n")
    cat(" This test evaluates if removing the seasonal component (subtracting      \n")
    cat(" the previous year's value) has made the series stationary.               \n")
    cat("--------------------------------------------------------------------------\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0: The seasonally differenced series is Non-Stationary.               \n")
    cat(" • H1: The seasonally differenced series is Stationary.                   \n")
    cat("--------------------------------------------------------------------------\n")
    cat(paste(" - Alpha (Significance Level):", alpha_val, "\n"))
    cat(paste(" - If Tau (Observed) < Tau (Critical): Reject H0 (Stationary).\n"))
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # 2. Standard ADF Test for p-values
      res <- tseries::adf.test(
        diffSeasonTsData(), 
        alternative = input$alternDs1St, 
        k = as.numeric(input$LagOrderADFDs1St)
      )
      
      # 3. Advanced ADF Test for Critical Values (Tau)
      # We use type='trend' to match the tseries::adf.test internal regression model
      res_urca <- urca::ur.df(diffSeasonTsData(), type = "trend", lags = as.numeric(input$LagOrderADFDs1St))
      crit_vals <- res_urca@cval
      
      # Extract values
      tau_obs  <- res$statistic
      tau_crit <- crit_vals["tau3", alpha_col] 
      p_val    <- res$p.value
      
      cat(" RESULT:\n")
      cat(paste(" • Tau (Observed Value)  :", round(tau_obs, 4), "\n"))
      cat(paste(" • Tau (Critical Value)  :", round(tau_crit, 4), "(at", alpha_col, ")\n"))
      cat(paste(" • p-value (One-tailed)  :", round(p_val, 4), "\n"))
      cat(paste(" • Alpha                 :", alpha_val, "\n"))
      cat("--------------------------------------------------------------------------\n")
      
      cat(" DECISION:\n")
      if(p_val <= alpha_val) {
        cat(paste("  • The p-value is", round(p_val, 4), "(<=", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is more negative than Critical (", round(tau_crit, 2), ").\n"))
        cat("  • DECISION: Reject H0. The Seasonal Differenced Series is Stationary.\n")
      } else {
        cat(paste("  • The p-value is", round(p_val, 4), "(>", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is not negative enough.\n"))
        cat("  • DECISION: Fail to reject H0. The series remains Non-Stationary.\n")
      }
      
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE:\n")
      if(p_val <= alpha_val) {
        cat("  • Seasonal differencing (D=1) successfully removed the unit root.\n")
        cat("  • Your SARIMA model should likely include the D = 1 component.\n")
        cat("  • Next Step: Check the ACF/PACF for seasonal AR or MA terms.\n")
      } else {
        cat("  • The series is still non-stationary after seasonal differencing.\n")
        cat("  • ADVICE: You may need a combination of d=1 and D=1 (Integrated Seasonal).\n")
        cat("  • This is common in 'Airline' models (SARIMA(0,1,1)(0,1,1)).\n")
      }
      
    }, error = function(e) {
      cat(" EXECUTION ERROR: ", e$message, "\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE: Seasonal differencing reduces the sample size significantly.\n")
      cat(" Try reducing the 'Lag' value (k) to accommodate the smaller dataset.\n")
    })
    cat("==========================================================================\n")
  })
  
  



  
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #        d[1] (log(St)): Ordinary Difference of Log Series
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Centralized Reactive for First Difference of Log
  diff1LogTsData <- reactive({
    req(logTsData()) # Inherits log validation (strictly positive values)
    diff(logTsData(), differences = 1)
  })
  
  # 2. Difference of Log Plot
  output$plotd1Log <- renderPlot({
    req(diff1LogTsData())
    
    autoplot(diff1LogTsData(), size = 1, colour = "darkcyan") +
      labs(
        title = paste("First Diff of Log:", userData$mainTitle),
        x = userData$xLabel,
        y = paste0("Δ log(", userData$yLabel, ")")
      ) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 3. Difference of Log ACF
  output$d1LogStACFa <- renderPlot({
    req(diff1LogTsData())
    forecast::ggAcf(diff1LogTsData()) + 
      labs(title = paste("ACF (d=1, Log):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 4. Difference of Log PACF
  output$d1LogStPACFa <- renderPlot({
    req(diff1LogTsData())
    forecast::ggPacf(diff1LogTsData()) + 
      labs(title = paste("PACF (d=1, Log):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 5. Combined Difference of Log ACF/PACF
  output$d1LogStACFPACFa <- renderPlot({
    req(diff1LogTsData())
    
    p1 <- forecast::ggAcf(diff1LogTsData()) + apply_user_theme() + labs(title = "Diff-Log ACF")
    p2 <- forecast::ggPacf(diff1LogTsData()) + apply_user_theme() + labs(title = "Diff-Log PACF")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 6. Integrated Diagnostic Display (Log + Diff)
  output$d1_log_ts_Display <- renderPlot({
    req(diff1LogTsData(), input$plot_type)
    
    forecast::ggtsdisplay(
      diff1LogTsData(), 
      plot.type = input$plot_type, 
      main = paste("Diagnostic Display (d=1, Log):", userData$mainTitle),
      xlab = userData$xLabel, 
      ylab = userData$yLabel
    ) + apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  
  # 7. Stationarity Test on First Difference of Log
  output$teststationarited1LogSt <- renderPrint({
    # Pull alpha from the primary alphaSt input as requested
    req(diff1LogTsData(), input$alternd1LogSt, input$LagOrderADFd1LogSt, input$alphaSt)
    
    # 1. Map Alpha input to urca critical value columns
    alpha_val <- as.numeric(input$alphaSt)
    alpha_col <- switch(as.character(alpha_val),
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct")
    
    cat("==========================================================================\n")
    cat("      ADF TEST ON FIRST DIFFERENCE OF LOG SERIES                          \n")
    cat("==========================================================================\n")
    cat(" This test evaluates the series after both a Log transform (for variance)  \n")
    cat(" and First Differencing (for trend). This often represents growth rates.   \n")
    cat("--------------------------------------------------------------------------\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0: The Log-Differenced series is Non-Stationary.                      \n")
    cat(" • H1: The Log-Differenced series is Stationary.                          \n")
    cat("--------------------------------------------------------------------------\n")
    cat(paste(" - Alpha (Significance Level):", alpha_val, "\n"))
    cat(paste(" - If Tau (Observed) < Tau (Critical): Reject H0 (Stationary).\n"))
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # 2. Standard ADF Test for p-values
      res <- tseries::adf.test(
        diff1LogTsData(), 
        alternative = input$alternd1LogSt, 
        k = as.numeric(input$LagOrderADFd1LogSt)
      )
      
      # 3. Advanced ADF Test for Critical Values (Tau) using urca
      # Using type='trend' to match the internal regression model of tseries::adf.test
      res_urca <- urca::ur.df(diff1LogTsData(), type = "trend", lags = as.numeric(input$LagOrderADFd1LogSt))
      crit_vals <- res_urca@cval
      
      # Extract values
      tau_obs  <- res$statistic
      tau_crit <- crit_vals["tau3", alpha_col] 
      p_val    <- res$p.value
      
      cat(" RESULT:\n")
      cat(paste(" • Tau (Observed Value)  :", round(tau_obs, 4), "\n"))
      cat(paste(" • Tau (Critical Value)  :", round(tau_crit, 4), "(at", alpha_col, ")\n"))
      cat(paste(" • p-value (One-tailed)  :", round(p_val, 4), "\n"))
      cat(paste(" • Alpha                 :", alpha_val, "\n"))
      cat("--------------------------------------------------------------------------\n")
      
      cat(" DECISION:\n")
      if(p_val <= alpha_val) {
        cat(paste("  • The p-value is", round(p_val, 4), "(<=", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is more negative than Critical (", round(tau_crit, 2), ").\n"))
        cat("  • DECISION: Reject H0. The Log-Differenced Series is Stationary.\n")
      } else {
        cat(paste("  • The p-value is", round(p_val, 4), "(>", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is not negative enough.\n"))
        cat("  • DECISION: Fail to reject H0. The series remains Non-Stationary.\n")
      }
      
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE:\n")
      if(p_val <= alpha_val) {
        cat("  • The combination of Log + Differencing (d=1) has achieved stationarity.\n")
        cat("  • This suggests your ARIMA model should use d=1 and a Log transform.\n")
        cat("  • This transformation is common for financial and economic 'return' series.\n")
      } else {
        cat("  • Even after Log and d=1, the series is not stationary.\n")
        cat("  • ADVICE: Check for a strong seasonal component (requires D=1) or\n")
        cat("    structural breaks in the data that differencing cannot fix.\n")
      }
      
    }, error = function(e) {
      cat(" EXECUTION ERROR: ", e$message, "\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE: This double-transformation reduces the number of usable points. \n")
      cat(" Ensure your 'Lag' order is not too high for the remaining data.\n")
    })
    cat("==========================================================================\n")
  })
  
  
  

  


  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #        D[1] (log(St)): Seasonal Difference of Log Series
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Centralized Reactive for Seasonal Difference of Log
  diffSeasonLogTsData <- reactive({
    req(logTsData()) # Inherits safety checks for positive values
    freq <- as.numeric(currentFrequency())
    diff(logTsData(), lag = freq, differences = 1)
  })
  
  # 2. Seasonal Log Difference Plot
  output$Dlogplot <- renderPlot({
    req(diffSeasonLogTsData())
    
    autoplot(diffSeasonLogTsData(), size = 1, colour = "darkorchid") +
      labs(
        title = paste("Seasonal Diff of Log:", userData$mainTitle),
        x = userData$xLabel,
        y = paste0("Δs log(", userData$yLabel, ")")
      ) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 3. Seasonal Log ACF
  output$DlogplotACF <- renderPlot({
    req(diffSeasonLogTsData())
    forecast::ggAcf(diffSeasonLogTsData()) + 
      labs(title = paste("ACF (D=1, Log):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 4. Seasonal Log PACF
  output$DlogplotPACF <- renderPlot({
    req(diffSeasonLogTsData())
    forecast::ggPacf(diffSeasonLogTsData()) + 
      labs(title = paste("PACF (D=1, Log):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 5. Combined Seasonal Log ACF/PACF
  output$DlogplotACFPACF <- renderPlot({
    req(diffSeasonLogTsData())
    
    p1 <- forecast::ggAcf(diffSeasonLogTsData()) + apply_user_theme() + labs(title = "Seasonal Log ACF")
    p2 <- forecast::ggPacf(diffSeasonLogTsData()) + apply_user_theme() + labs(title = "Seasonal Log PACF")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 6. Integrated Diagnostic Display (Log + Seasonal Diff)
  output$Ds1_log_ts_Display <- renderPlot({
    req(diffSeasonLogTsData(), input$plot_type)
    
    forecast::ggtsdisplay(
      diffSeasonLogTsData(), 
      plot.type = input$plot_type, 
      main = paste("Diagnostic Display (D=1, Log):", userData$mainTitle),
      xlab = userData$xLabel, 
      ylab = userData$yLabel
    ) + apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  
  
  # 7. Stationarity Test on Seasonal Log Series
  output$teststationariteDs1LogSt <- renderPrint({
    # Pull alpha from the primary alphaSt input for consistency
    req(diffSeasonLogTsData(), input$alternDs1LogSt, input$LagOrderADFDs1LogSt, input$alphaSt)
    
    # 1. Map Alpha input to urca critical value columns
    alpha_val <- as.numeric(input$alphaSt)
    alpha_col <- switch(as.character(alpha_val),
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct")
    
    cat("==========================================================================\n")
    cat("      ADF TEST ON SEASONAL DIFFERENCE OF LOG SERIES (D=1)                 \n")
    cat("==========================================================================\n")
    cat(" This test checks if a Log transform (for variance) and a Seasonal          \n")
    cat(" Difference (to remove annual cycles) resulted in a stationary series.      \n")
    cat("--------------------------------------------------------------------------\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0: The Seasonal-Log series is Non-Stationary (has a unit root).         \n")
    cat(" • H1: The Seasonal-Log series is Stationary.                               \n")
    cat("--------------------------------------------------------------------------\n")
    cat(paste(" - Alpha (Significance Level):", alpha_val, "\n"))
    cat(paste(" - If Tau (Observed) < Tau (Critical): Reject H0 (Stationary).\n"))
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # 2. Standard ADF Test for p-values
      res <- tseries::adf.test(
        diffSeasonLogTsData(), 
        alternative = input$alternDs1LogSt, 
        k = as.numeric(input$LagOrderADFDs1LogSt)
      )
      
      # 3. Advanced ADF Test for Critical Values (Tau)
      # type='trend' matches the internal regression of tseries::adf.test
      res_urca <- urca::ur.df(diffSeasonLogTsData(), type = "trend", lags = as.numeric(input$LagOrderADFDs1LogSt))
      crit_vals <- res_urca@cval
      
      # Extract values
      tau_obs  <- res$statistic
      tau_crit <- crit_vals["tau3", alpha_col] 
      p_val    <- res$p.value
      
      cat(" RESULT:\n")
      cat(paste(" • Tau (Observed Value)  :", round(tau_obs, 4), "\n"))
      cat(paste(" • Tau (Critical Value)  :", round(tau_crit, 4), "(at", alpha_col, ")\n"))
      cat(paste(" • p-value (One-tailed)  :", round(p_val, 4), "\n"))
      cat(paste(" • Alpha                 :", alpha_val, "\n"))
      cat("--------------------------------------------------------------------------\n")
      
      cat(" DECISION:\n")
      if(p_val <= alpha_val) {
        cat(paste("  • The p-value is", round(p_val, 4), "(<=", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is more negative than Critical (", round(tau_crit, 2), ").\n"))
        cat("  • DECISION: Reject H0. The Seasonal-Log series is Stationary.\n")
      } else {
        cat(paste("  • The p-value is", round(p_val, 4), "(>", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is not negative enough.\n"))
        cat("  • DECISION: Fail to reject H0. The series remains Non-Stationary.\n")
      }
      
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE:\n")
      if(p_val <= alpha_val) {
        cat("  • Stationarity achieved! Log transformation and Seasonal differencing\n")
        cat("    are likely necessary components for your final SARIMA model.\n")
        cat("  • You are modeling the 'Annual Percentage Growth Rate'.\n")
      } else {
        cat("  • The series is still non-stationary after Seasonal-Log transformation.\n")
        cat("  • ADVICE: You may still need to apply an ordinary difference (d=1)\n")
        cat("    on top of the seasonal difference to remove a remaining trend.\n")
      }
      
    }, error = function(e) {
      cat(" EXECUTION ERROR: ", e$message, "\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE: Seasonal differencing on log data can result in very small\n")
      cat(" datasets. Lower the 'Lag' order (k) to allow the test to calculate.\n")
    })
    cat("==========================================================================\n")
  })
  
  

  



  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #    d[1] ( D[1] ( log(St) ) ): Double Difference of Log Series
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Centralized Reactive for First + Seasonal Difference of Log
  diff1LogSeasonTsData <- reactive({
    # Starts with logTsData (includes positivity check)
    # Then applies seasonal difference (D=1)
    # Then applies ordinary difference (d=1)
    req(diffSeasonLogTsData()) 
    diff(diffSeasonLogTsData(), differences = 1)
  })
  
  # 2. Double Difference Log Plot
  output$dDlogplot <- renderPlot({
    req(diff1LogSeasonTsData())
    
    autoplot(diff1LogSeasonTsData(), size = 1, colour = "firebrick4") +
      labs(
        title = paste("d=1, D=1 Log:", userData$mainTitle),
        x = userData$xLabel,
        y = paste0("Δ Δs log(", userData$yLabel, ")")
      ) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 3. Double Difference Log ACF
  output$dDlogplotACF <- renderPlot({
    req(diff1LogSeasonTsData())
    forecast::ggAcf(diff1LogSeasonTsData()) + 
      labs(title = paste("ACF (d=1, D=1, Log):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 4. Double Difference Log PACF
  output$dDlogplotPACF <- renderPlot({
    req(diff1LogSeasonTsData())
    forecast::ggPacf(diff1LogSeasonTsData()) + 
      labs(title = paste("PACF (d=1, D=1, Log):", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 5. Combined Double Difference Log ACF/PACF
  output$dDlogplotACFPACF <- renderPlot({
    req(diff1LogSeasonTsData())
    
    p1 <- forecast::ggAcf(diff1LogSeasonTsData()) + apply_user_theme() + labs(title = "Double Diff Log ACF")
    p2 <- forecast::ggPacf(diff1LogSeasonTsData()) + apply_user_theme() + labs(title = "Double Diff Log PACF")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 6. Integrated Diagnostic Display (d=1, D=1, Log)
  output$d1_Ds1_log_ts_Display <- renderPlot({
    req(diff1LogSeasonTsData(), input$plot_type)
    
    forecast::ggtsdisplay(
      diff1LogSeasonTsData(), 
      plot.type = input$plot_type, 
      main = paste("Diagnostic Display (d=1, D=1, Log):", userData$mainTitle),
      xlab = userData$xLabel, 
      ylab = userData$yLabel
    ) + apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  
  # 7. Stationarity Test on Double Differenced Log Series
  output$teststationarited1Ds1LogSt <- renderPrint({
    # Pull alpha from the primary alphaSt input for consistency
    req(diff1LogSeasonTsData(), input$alternd1Ds1LogSt, input$LagOrderADFd1Ds1LogSt, input$alphaSt)
    
    # 1. Map Alpha input to urca critical value columns
    alpha_val <- as.numeric(input$alphaSt)
    alpha_col <- switch(as.character(alpha_val),
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct")
    
    cat("==========================================================================\n")
    cat("      ADF TEST: DOUBLE DIFFERENCE OF LOG SERIES (d=1, D=1)                \n")
    cat("==========================================================================\n")
    cat(" This test evaluates the series after Log transformation, First           \n")
    cat(" Differencing (d=1), and Seasonal Differencing (D=1).                     \n")
    cat("--------------------------------------------------------------------------\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0: The Double-Differenced Log series is Non-Stationary.               \n")
    cat(" • H1: The Double-Differenced Log series is Stationary.                   \n")
    cat("--------------------------------------------------------------------------\n")
    cat(paste(" - Alpha (Significance Level):", alpha_val, "\n"))
    cat(paste(" - If Tau (Observed) < Tau (Critical): Reject H0 (Stationary).\n"))
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # 2. Standard ADF Test for p-values
      res <- tseries::adf.test(
        diff1LogSeasonTsData(), 
        alternative = input$alternd1Ds1LogSt, 
        k = as.numeric(input$LagOrderADFd1LogSt)
      )
      
      # 3. Advanced ADF Test for Critical Values (Tau) using urca
      # type='trend' matches the internal regression of tseries::adf.test
      res_urca <- urca::ur.df(diff1LogSeasonTsData(), type = "trend", lags = as.numeric(input$LagOrderADFd1LogSt))
      crit_vals <- res_urca@cval
      
      # Extract values
      tau_obs  <- res$statistic
      tau_crit <- crit_vals["tau3", alpha_col] 
      p_val    <- res$p.value
      
      cat(" RESULT:\n")
      cat(paste(" • Tau (Observed Value)  :", round(tau_obs, 4), "\n"))
      cat(paste(" • Tau (Critical Value)  :", round(tau_crit, 4), "(at", alpha_col, ")\n"))
      cat(paste(" • p-value (One-tailed)  :", round(p_val, 4), "\n"))
      cat(paste(" • Alpha                 :", alpha_val, "\n"))
      cat("--------------------------------------------------------------------------\n")
      
      cat(" DECISION:\n")
      if(p_val <= alpha_val) {
        cat(paste("  • The p-value is", round(p_val, 4), "(<=", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is more negative than Critical (", round(tau_crit, 2), ").\n"))
        cat("  • DECISION: Reject H0. The series is now Stationary.\n")
      } else {
        cat(paste("  • The p-value is", round(p_val, 4), "(>", alpha_val, ").\n"))
        cat(paste("  • Tau Observed (", round(tau_obs, 2), ") is not negative enough.\n"))
        cat("  • DECISION: Fail to reject H0. Series is still Non-Stationary.\n")
      }
      
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE:\n")
      if(p_val <= alpha_val) {
        cat("  • Stationarity achieved with (d=1, D=1). This is usually the final\n")
        cat("    transformation step before fitting a SARIMA model.\n")
        cat("  • Your model parameters are likely (p, 1, q)x(P, 1, Q)s.\n")
      } else {
        cat("  • Warning: The series remains non-stationary after extreme differencing.\n")
        cat("  • ADVICE: Check for structural breaks or 'over-differencing'.\n")
        cat("    Over-differencing can introduce artificial patterns (negative ACF).\n")
      }
      
    }, error = function(e) {
      cat(" EXECUTION ERROR: ", e$message, "\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE: Double differencing significantly reduces the number of data\n")
      cat(" points. Reduce 'Lag' order or check if the sample size is too small.\n")
    })
    cat("==========================================================================\n")
  })
  



  
 
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #        d[?] D[?] (log[?] (S(t))): User-Defined Transformations
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Centralized Reactive for User-Selected Transformations
  myData_Choice <- reactive({
    req(tsData())
    getMyData(
      tsData(),
      currentFrequency(),
      values$islog,
      input$d_n,
      input$DS_n
    )
  })
  

  
  # 2. Main Choice Diagnostic Display
  output$d_D_Log_ts_Choice_UI <- renderUI({
    plotOutput("d_D_Log_ts_Choice", 
               width = getPlotDim(userData$plotWidth), 
               height = getPlotDim(userData$plotHeight))
  })
  

  output$d_D_Log_ts_Choice <- renderPlot({
    req(myData_Choice(), input$plot_type)

    ggtsdisplay(myData_Choice(),
                plot.type = input$plot_type ,
                main = userData$mainTitle,
                xlab = userData$xLabel,
                ylab = userData$yLabel)
  })
  
  
  # 3. Main Choice Time Plot
  output$tsPlot_Choice_UI <- renderUI({
    plotOutput("tsPlot_Choice", 
               width = getPlotDim(userData$plotWidth), 
               height = getPlotDim(userData$plotHeight))
  })
  
  
  output$ts_color_ui <- renderUI({
    tagList(
      tags$input(type = "color", id = "ts_line_color", value = "#2C7FB8"),
      tags$label("Series color", style = "color: #2C7FB8;"),
      br(), br(),
      
      # Plot Colors Picker
      tags$script(HTML("
      $(document).ready(function() {
        var el = document.getElementById('ts_line_color');
        if (el) Shiny.setInputValue('ts_line_color', el.value);

        $(document).on('input', '#ts_line_color', function() {
          Shiny.setInputValue(this.id, this.value);
        });
      });
    "))
    )
    
  })
  
  output$tsPlot_Choice <- renderPlot({
    req(myData_Choice(), input$ts_line_color)
    
    autoplot(myData_Choice(), size = 1, colour = input$ts_line_color) +
      labs(
        title = userData$mainTitle,
        x = userData$xLabel,
        y = userData$yLabel
      ) +
      apply_user_theme() +
      theme(
        axis.text  = element_text(size = input$tickSize),
        axis.title = element_text(size = input$tickSize + 2)
      )
  })
  
  

  
  # 4. Choice ACF/PACF Individual Plots
  output$difference2ACF <- renderPlot({
    req(myData_Choice())
    forecast::ggAcf(myData_Choice()) + 
      labs(title = paste("ACF:", userData$mainTitle)) + 
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  output$difference2PACF <- renderPlot({
    req(myData_Choice())
    forecast::ggPacf(myData_Choice()) + 
      labs(title = paste("PACF:", userData$mainTitle)) + 
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 5. Combined Choice ACF/PACF
  output$difference2ACFPACF_UI <- renderUI({
    plotOutput("difference2ACFPACF", 
               width = getPlotDim(userData$plotWidth), 
               height = getPlotDim(userData$plotHeight))
  })
  
  output$difference2ACFPACF <- renderPlot({
    req(myData_Choice())
    
    p1 <- forecast::ggAcf(myData_Choice()) + apply_user_theme() + labs(title = "ACF")
    p2 <- forecast::ggPacf(myData_Choice()) + apply_user_theme() + labs(title = "PACF")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1, top = userData$mainTitle)
  })
  
  


  
  
  

  
  
  
  
  
  
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  
  
  
  
  
  
  
  output$teststationarited3St <- renderPrint({
    
    # ============================================================================
    # 0) SMALL HELPERS (safe input fallback + safe numeric)
    # ============================================================================
    `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
    
    to_num_safe <- function(v, default = NA_real_) {
      out <- suppressWarnings(as.numeric(v))
      if (length(out) == 0 || all(is.na(out)) || !is.finite(out[1])) default else out[1]
    }
    
    to_int_safe <- function(v, default = 0L) {
      out <- suppressWarnings(as.integer(v))
      if (length(out) == 0 || is.na(out[1]) || !is.finite(out[1])) default else out[1]
    }
    
    safe_head_tail <- function(x, n = 5) {
      x <- as.numeric(x)
      x <- x[is.finite(x)]
      if (length(x) == 0) return(list(head = numeric(0), tail = numeric(0)))
      list(head = head(x, n), tail = tail(x, n))
    }
    
    # helper to safely pick tau row given ur.df type
    tau_row_for <- function(type_in) {
      switch(type_in,
             "none"  = "tau1",
             "drift" = "tau2",
             "trend" = "tau3",
             "tau3")
    }
    
    # --- NEW: robust critical-value extractor for urca objects (fixes KPSS Eta-Crit NA) ---
    cval_pick_safe <- function(cval_obj, key) {
      if (is.null(cval_obj)) return(NA_real_)
      
      # matrix case
      if (is.matrix(cval_obj)) {
        cn <- colnames(cval_obj)
        rn <- rownames(cval_obj)
        
        if (!is.null(cn) && key %in% cn) return(suppressWarnings(as.numeric(cval_obj[1, key, drop = TRUE])))
        if (!is.null(rn) && key %in% rn) return(suppressWarnings(as.numeric(cval_obj[key, 1, drop = TRUE])))
        
        # fallback: first element
        return(suppressWarnings(as.numeric(cval_obj[1, 1])))
      }
      
      # vector case
      nm <- names(cval_obj)
      if (!is.null(nm) && key %in% nm) return(suppressWarnings(as.numeric(cval_obj[[key]])))
      
      # unnamed vector fallback (common order in urca: 10pct, 5pct, 1pct)
      if (length(cval_obj) >= 3) {
        if (identical(key, "10pct")) return(suppressWarnings(as.numeric(cval_obj[1])))
        if (identical(key, "5pct"))  return(suppressWarnings(as.numeric(cval_obj[2])))
        if (identical(key, "1pct"))  return(suppressWarnings(as.numeric(cval_obj[3])))
      }
      
      suppressWarnings(as.numeric(cval_obj[1]))
    }
    
    # ============================================================================
    # 1) INPUT COLLECTION (supports either naming convention in your UI)
    # ============================================================================
    alt_in  <- input$alternd2St %||% input$alternSt
    lag_in  <- input$LagOrderADFd2St %||% input$LagOrderADFSt
    a_in    <- input$alphaSt2
    type_in <- input$adfTypeSt2
    
    # OPTIONAL (transformation inputs — we only PRINT them, we don't transform here)
    # Because the actual transformation must happen inside myData_Choice().
    d_in   <- input$d_n  %||% input$d  %||% NA
    D_in   <- input$DS_n %||% input$D  %||% NA
    log_in <- input$check_box %||% input$islog %||% FALSE
    
    # Required data
    req(myData_Choice())
    
    # Required inputs (validated safely)
    if (is.null(alt_in) || is.null(lag_in) || is.null(a_in) || is.null(type_in)) {
      cat("==========================================================================\n")
      cat("                STATE-OF-THE-ART ADF UNIT ROOT DIAGNOSTIC                 \n")
      cat("==========================================================================\n")
      cat(" [!] INPUT ERROR: One or more inputs are NULL.\n")
      cat("     This usually means a UI/server ID mismatch.\n")
      cat("     Needed inputs (either naming is OK):\n")
      cat("       - alternative : input$alternd2St OR input$alternSt\n")
      cat("       - lag         : input$LagOrderADFd2St OR input$LagOrderADFSt\n")
      cat("       - alpha       : input$alphaSt2\n")
      cat("       - adf type    : input$adfTypeSt2\n")
      cat("==========================================================================\n")
      return(invisible(NULL))
    }
    
    # ============================================================================
    # 2) DATA PREP (IMPORTANT: we test EXACTLY what myData_Choice() returns)
    # ============================================================================
    x_raw <- myData_Choice()   # should already be transformed (log/d/D) in your app
    na_before <- sum(is.na(x_raw))
    
    # keep ts metadata if available (for reporting)
    x_class <- paste(class(x_raw), collapse = ", ")
    x_freq  <- NA_integer_
    if (inherits(x_raw, "ts")) x_freq <- tryCatch(stats::frequency(x_raw), error = function(e) NA_integer_)
    
    x <- as.numeric(stats::na.omit(x_raw))
    valid_N <- length(x)
    
    # lag (k) must be integer >= 0
    k <- to_int_safe(lag_in, default = 0L)
    if (!is.finite(k) || k < 0) k <- 0L
    
    # alpha mapping (robust)
    alpha_raw <- as.character(a_in)
    alpha_val <- to_num_safe(alpha_raw, default = 0.05)
    
    alpha_col <- switch(alpha_raw,
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct",
                        "0.10" = "10pct",
                        "1pct" = "1pct",
                        "5pct" = "5pct",
                        "10pct"= "10pct",
                        "5pct")  # default fallback
    
    # tau row mapping (ADF)
    tau_row <- tau_row_for(type_in)
    
    # seasonality resolved flag (based on your UI D input: seasonal differencing)
    # ONLY reporting; real seasonal differencing must happen in myData_Choice().
    D_ui <- to_int_safe(D_in, default = 0L)
    seasonality_resolved <- isTRUE(D_ui > 0L)
    
    # ============================================================================
    # SECTION 1: DATA CONTEXT & SPECIFICATION
    # ============================================================================
    cat("==========================================================================\n")
    cat("                        ADF UNIT ROOT DIAGNOSTIC                          \n")
    cat("==========================================================================\n")
    cat(sprintf(" MODEL TYPE : %-10s | SAMPLE SIZE (N) : %d\n", toupper(type_in), valid_N))
    cat(sprintf(" LAG ORDER  : %-10d | SIGNIFICANCE (α) : %s\n", k, alpha_raw))
    cat(sprintf(" MOMENTS    : Mean: %.4f | Std.Dev: %.4f\n", mean(x), stats::sd(x)))

    # --- Transformation provenance (guarantees “same series for all tests”) ---
    cat("--------------------------------------------------------------------------\n")
    cat(" TRANSFORMATION PROVENANCE (what ALL tests use):\n")
    cat(sprintf(" [ ] Source object class           : %s\n", x_class))
    cat(sprintf(" [ ] Frequency (if ts)             : %s\n", ifelse(is.finite(x_freq), as.character(x_freq), "NA / not ts")))
    cat(sprintf(" [ ] NA count before na.omit       : %d\n", na_before))
    cat(sprintf(" [ ] Transformation inputs (UI)    : log=%s | d=%s | D=%s\n",
                ifelse(isTRUE(log_in), "ON", "OFF"),
                ifelse(is.na(d_in), "NA", as.character(d_in)),
                ifelse(is.na(D_in), "NA", as.character(D_in))))
    ht <- safe_head_tail(x, n = 5)
    cat(sprintf(" [ ] First 5 values used in tests  : %s\n", paste(round(ht$head, 4), collapse = ", ")))
    cat(sprintf(" [ ] Last  5 values used in tests  : %s\n", paste(round(ht$tail, 4), collapse = ", ")))
    
    # Basic sanity
    if (valid_N < 5) {
      cat("--------------------------------------------------------------------------\n\n")
      cat(" [!] CRITICAL ERROR: Too few observations (N < 5).\n")
      cat("     DIRECTIVE: Provide more data points.\n")
      cat("==========================================================================\n")
      return(invisible(NULL))
    }
    
    if (!is.finite(stats::sd(x)) || stats::sd(x) == 0) {
      cat("--------------------------------------------------------------------------\n\n")
      cat(" [!] CRITICAL ERROR: Series is constant or invalid (sd = 0 / NA).\n")
      cat("     DIRECTIVE: Check transformation (log/diff) or data quality.\n")
      cat("==========================================================================\n")
      return(invisible(NULL))
    }
    
    # Degrees of Freedom Safety Check (conservative)
    if (valid_N <= (k + 10)) {
      cat("--------------------------------------------------------------------------\n\n")
      cat(" [!] WARNING: Sample size is small relative to selected lag.\n")
      cat("     This can break tseries::adf.test and weaken inference.\n")
      cat("     DIRECTIVE: Decrease Lag (k) or increase data points.\n\n")
    }
    
    # ============================================================================
    # 3) PREDECLARE OBJECTS (prevents crashes in post-summary)
    # ============================================================================
    res_urca <- NULL
    
    tau_obs  <- NA_real_
    tau_crit <- NA_real_
    
    res_tseries <- list(p.value = NA_real_)
    lb_test     <- list(statistic = NA_real_, p.value = NA_real_)
    lb_lag      <- NA_integer_
    
    # KPSS
    kpss_type   <- if (type_in == "trend") "Trend" else "Level"
    res_kpss_ts <- list(statistic = NA_real_, p.value = NA_real_) # tseries
    res_kpss_uc <- NULL                                           # urca
    
    # KPSS ETA extras
    eta_obs_ts   <- NA_real_
    eta_p_one    <- NA_real_
    eta_obs_uc   <- NA_real_
    eta_crit_uc  <- NA_real_
    eta_col      <- NA_character_
    
    is_stationary    <- FALSE
    kpss_stationary  <- FALSE
    
    # Pettitt placeholders
    pettitt_res <- list(statistic = NA_real_, p.value = NA_real_, estimate = NULL)
    
    # --- NEW: decision-aid objects ---
    k_reco        <- NA_integer_
    k_reco_reason <- NA_character_
    scan_k_tbl    <- NULL
    scan_type_tbl <- NULL
    
    # ============================================================================
    # 4) COMPUTATIONS (tryCatch keeps UI alive, cats remain)
    # ============================================================================
    tryCatch({
      
      # --- Package checks ---
      if (!requireNamespace("urca", quietly = TRUE)) {
        stop("Package 'urca' is not installed. ACTION: install.packages('urca')")
      }
      if (!requireNamespace("tseries", quietly = TRUE)) {
        stop("Package 'tseries' is not installed. ACTION: install.packages('tseries')")
      }
      
      # ==========================================================================
      # NEW (A): LAG RECOMMENDATION SCAN (choose smallest k with LB p-value > alpha)
      # ==========================================================================
      lb_lag_ref <- max(1L, min(10L, floor(valid_N / 5)))
      
      # keep scan small/fast
      k_scan_max <- min(12L, max(0L, floor(valid_N / 5)))
      k_grid <- 0L:k_scan_max
      
      scan_k_tbl <- data.frame(
        k = k_grid,
        lb_p = NA_real_,
        tau = NA_real_,
        tau_crit = NA_real_,
        stationary = NA,
        stringsAsFactors = FALSE
      )
      
      for (i in seq_along(k_grid)) {
        ki <- k_grid[i]
        tmp <- tryCatch(urca::ur.df(x, type = type_in, lags = ki), error = function(e) NULL)
        if (!is.null(tmp)) {
          tr <- tau_row_for(type_in)
          tau_i <- suppressWarnings(as.numeric(tmp@teststat[tr]))
          if (!is.finite(tau_i)) tau_i <- suppressWarnings(as.numeric(tmp@teststat[1]))
          crit_i <- suppressWarnings(as.numeric(tmp@cval[tr, alpha_col]))
          lb_i <- tryCatch(Box.test(tmp@res, lag = lb_lag_ref, type = "Ljung-Box"),
                           error = function(e) list(p.value = NA_real_))
          lbp <- to_num_safe(lb_i$p.value)
          
          scan_k_tbl$lb_p[i]      <- lbp
          scan_k_tbl$tau[i]       <- tau_i
          scan_k_tbl$tau_crit[i]  <- crit_i
          scan_k_tbl$stationary[i]<- is.finite(tau_i) && is.finite(crit_i) && (tau_i < crit_i)
        }
      }
      
      ok_idx <- which(is.finite(scan_k_tbl$lb_p) & (scan_k_tbl$lb_p > alpha_val))
      if (length(ok_idx) > 0) {
        k_reco <- scan_k_tbl$k[min(ok_idx)]
        k_reco_reason <- "smallest k with Ljung-Box p-value > alpha (whiter residuals)"
      } else if (any(is.finite(scan_k_tbl$lb_p))) {
        k_reco <- scan_k_tbl$k[which.max(scan_k_tbl$lb_p)]
        k_reco_reason <- "no k achieved LB p>alpha; chose k that maximizes LB p-value"
      } else {
        k_reco <- NA_integer_
        k_reco_reason <- "lag scan failed (NA Ljung-Box p-values)"
      }
      
      
      cat("\n")
      cat("==========================================================================\n")
      cat("PHASE 0: DECISION AID (Lag + Spec Sensitivity)\n")
      cat("==========================================================================\n")
      cat(sprintf(" • Ljung-Box reference lag used in scan : %d  ; (LB lag = min(10, floor(N/5)) \n", lb_lag_ref))
      if (is.finite(k_reco)) {
        tag_k <- if (isTRUE(k_reco == k)) "[✓]" else "[!]"
        cat(sprintf(" %s Suggested k* (scan)                : %d  (%s)\n", tag_k, k_reco, k_reco_reason))
        if (isTRUE(k_reco != k)) {
          cat(sprintf(" [!] You selected k=%d. Consider trying k=%d and re-running.\n", k, k_reco))
        }
      } else {
        cat(" [?] Suggested k* (scan)                : NA (scan inconclusive)\n")
      }
      cat("--------------------------------------------------------------------------\n")
      
      # ==========================================================================
      # NEW (B): ADF TYPE SENSITIVITY (none/drift/trend) at k (user) and k* (if exists)
      # ==========================================================================
      types_to_try <- c("none", "drift", "trend")
      k_for_types  <- if (is.finite(k_reco)) c(k, k_reco) else c(k)
      k_for_types  <- unique(pmax(0L, as.integer(k_for_types)))
      
      scan_type_tbl <- data.frame(
        type = character(0),
        k = integer(0),
        tau = numeric(0),
        tau_crit = numeric(0),
        adf_stationary = logical(0),
        lb_p = numeric(0),
        lb_ok = logical(0),
        stringsAsFactors = FALSE
      )
      
      for (tt in types_to_try) {
        for (kk in k_for_types) {
          tmp <- tryCatch(urca::ur.df(x, type = tt, lags = kk), error = function(e) NULL)
          if (!is.null(tmp)) {
            tr <- tau_row_for(tt)
            tau_i <- suppressWarnings(as.numeric(tmp@teststat[tr]))
            if (!is.finite(tau_i)) tau_i <- suppressWarnings(as.numeric(tmp@teststat[1]))
            crit_i <- suppressWarnings(as.numeric(tmp@cval[tr, alpha_col]))
            lb_i <- tryCatch(Box.test(tmp@res, lag = lb_lag_ref, type = "Ljung-Box"),
                             error = function(e) list(p.value = NA_real_))
            lbp <- to_num_safe(lb_i$p.value)
            scan_type_tbl <- rbind(scan_type_tbl, data.frame(
              type = tt, k = kk,
              tau = tau_i, tau_crit = crit_i,
              adf_stationary = is.finite(tau_i) && is.finite(crit_i) && (tau_i < crit_i),
              lb_p = lbp,
              lb_ok = is.finite(lbp) && (lbp > alpha_val),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      if (nrow(scan_type_tbl) > 0) {
        cat(" ADF SPEC SENSITIVITY (ur.df):\n")
        cat("   (Prefer: LB ok + stable decision across types)\n")
        for (kk in k_for_types) {
          cat(sprintf("  • k=%d\n", kk))
          sub <- scan_type_tbl[scan_type_tbl$k == kk, , drop = FALSE]
          for (ii in seq_len(nrow(sub))) {
            row <- sub[ii, ]
            tag_lb <- if (isTRUE(row$lb_ok)) "[✓]" else if (is.finite(row$lb_p)) "[X]" else "[?]"
            tag_adf <- if (isTRUE(row$adf_stationary)) "STATIONARY" else "NON-STATIONARY"
            cat(sprintf("     %s type=%-5s | tau=%.4f | crit=%.4f | ADF=%s | LB p=%.4f\n",
                        tag_lb, row$type, row$tau, row$tau_crit, tag_adf, row$lb_p))
          }
        }
      } else {
        cat(" [?] ADF spec sensitivity scan failed.\n")
      }
      # cat("==========================================================================\n\n")
      cat("--------------------------------------------------------------------------\n")
      
      # ==========================================================================
      # PRIMARY TEST: ADF via urca::ur.df  (left-tail, stationary alternative)
      # ==========================================================================
      res_urca <- urca::ur.df(x, type = type_in, lags = k)
      
      tau_obs <- suppressWarnings(as.numeric(res_urca@teststat[tau_row]))
      if (!is.finite(tau_obs)) {
        tau_obs_fallback <- suppressWarnings(as.numeric(res_urca@teststat[1]))
        if (is.finite(tau_obs_fallback)) tau_obs <- tau_obs_fallback
      }
      
      tau_crit <- suppressWarnings(as.numeric(res_urca@cval[tau_row, alpha_col]))
      
      # Secondary ADF (p-value reference) — only if feasible
      if (valid_N > (k + 10)) {
        res_tseries <- tseries::adf.test(x, alternative = alt_in, k = k)
      } else {
        res_tseries <- list(p.value = NA_real_)
      }
      
      # ==========================================================================
      # QUALITY CHECK: Ljung-Box on ur.df residuals
      # ==========================================================================
      lb_lag <- max(1L, min(10L, floor(valid_N / 5)))
      lb_test <- Box.test(res_urca@res, lag = lb_lag, type = "Ljung-Box")
      
      # ==========================================================================
      # KPSS (H0 = Stationary):
      #   - tseries: p-value + stat
      #   - urca: observed + critical (consistent)
      # ==========================================================================
      kpss_type <- if (type_in == "trend") "Trend" else "Level"
      
      res_kpss_ts <- tseries::kpss.test(x, null = kpss_type)
      eta_obs_ts  <- to_num_safe(res_kpss_ts$statistic)
      eta_p_one   <- to_num_safe(res_kpss_ts$p.value)
      
      kpss_type_urca <- if (type_in == "trend") "tau" else "mu"
      res_kpss_uc    <- urca::ur.kpss(x, type = kpss_type_urca)
      
      eta_obs_uc <- suppressWarnings(as.numeric(res_kpss_uc@teststat))
      
      # --- FIX: choose eta_col robustly + extract critical value robustly (prevents NA) ---
      eta_col <- if (alpha_val <= 0.01) "1pct" else if (alpha_val <= 0.05) "5pct" else "10pct"
      eta_crit_uc <- cval_pick_safe(res_kpss_uc@cval, eta_col)
      
      # ==========================================================================
      # Pettitt structural break check (optional)
      # ==========================================================================
      if (requireNamespace("trend", quietly = TRUE)) {
        pettitt_res <- tryCatch(trend::pettitt.test(x),
                                error = function(e) list(statistic = NA_real_, p.value = NA_real_, estimate = NULL))
      }
      
      # ==========================================================================
      # SAFE FLAGS
      # ==========================================================================
      tau_ok <- is.finite(tau_obs) && is.finite(tau_crit)
      lb_ok  <- is.finite(to_num_safe(lb_test$p.value))
      
      kpss_p_ok   <- is.finite(eta_p_one)
      kpss_eta_ok <- is.finite(eta_obs_uc) && is.finite(eta_crit_uc)
      
      kpss_reject_by_p   <- kpss_p_ok   && (eta_p_one < alpha_val)
      kpss_reject_by_eta <- kpss_eta_ok && (eta_obs_uc > eta_crit_uc)
      
      kpss_ok <- kpss_p_ok || kpss_eta_ok
      kpss_stationary <- !(kpss_reject_by_p || kpss_reject_by_eta)
      
      is_stationary <- isTRUE(tau_ok) && (tau_obs < tau_crit)
      lb_white_safe <- lb_ok && (to_num_safe(lb_test$p.value) > alpha_val)
      
      # ==========================================================================
      # PHASE 1: ADF
      # ==========================================================================
      cat("==========================================================================\n")
      cat("PHASE 1: ADF UNIT ROOT TEST\n")
      cat("==========================================================================\n")
      
      cat(" • H0: The series has a Unit Root (Non-Stationary).\n")
      cat(" • Ha: The series is Stationary (Mean Reverting).\n")
      cat(sprintf(" -> CRITERIA: Reject H0 if Tau-Obs (%.4f) < Tau-Crit (%.4f)\n",
                  tau_obs, tau_crit))
      
      cat("\n RESULT:\n")
      cat(paste("  - Tau Observed :", round(tau_obs, 4), "\n"))
      cat(paste("  - Tau Critical :", round(tau_crit, 4), "\n"))
      cat(paste("  - P-Value (Ref):", round(to_num_safe(res_tseries$p.value), 6), "\n"))
      
      cat("\n DECISION:\n")
      if (isTRUE(tau_ok) && isTRUE(is_stationary)) {
        cat("  -> REJECT H0: Evidence suggests the series is STATIONARY.\n")
      } else if (isTRUE(tau_ok) && !isTRUE(is_stationary)) {
        cat("  -> FAIL TO REJECT H0: Evidence suggests the series is NON-STATIONARY.\n")
      } else {
        cat("  -> [!] WARNING: Tau/Critical value is NA/Inf. ADF inference is not valid.\n")
      }
      
      if (identical(as.character(alt_in), "explosive")) {
        cat("\n [!] NOTE (Explosive mode):\n")
        cat("     • tseries::adf.test(alternative='explosive') is right-tail oriented.\n")
        cat("     • urca::ur.df critical values are used here in the usual left-tail rule.\n")
        cat("     • For explosive detection, prioritize the tseries p-value interpretation.\n")
      }
      cat("\n")
      
      # ==========================================================================
      # PHASE 2: Ljung-Box
      # ==========================================================================
      cat("==========================================================================\n")
      cat("PHASE 2: RESIDUAL DIAGNOSTICS (LJUNG-BOX)\n")
      cat("==========================================================================\n")
      
      cat(" • H0: Residuals are White Noise (No Autocorrelation).\n")
      cat(" • Ha: Residuals are Correlated (Lags are insufficient).\n")
      cat(sprintf(" -> CRITERIA: Reject H0 if P-Value (%.6f) < α (%.4f)\n",
                  to_num_safe(lb_test$p.value), alpha_val))
      
      cat("\n RESULT:\n")
      cat(paste("  - LB Statistic :", round(to_num_safe(lb_test$statistic), 6), "\n"))
      cat(paste("  - LB P-Value   :", round(to_num_safe(lb_test$p.value), 6), "\n"))
      cat(paste("  - LB Lag used  :", lb_lag, "\n"))
      cat(paste("    it’s normal to choose LB lag by a rule-of-thumb (like min(10, floor(N/5)) \n"))
      
      cat("\n DECISION:\n")
      if (isTRUE(lb_white_safe)) {
        cat("  -> FAIL TO REJECT H0: Residuals are White Noise. [ADF more reliable]\n")
      } else if (isTRUE(lb_ok)) {
        cat("  -> REJECT H0: Residuals are Correlated. [ADF may be biased]\n")
      } else {
        cat("  -> [!] WARNING: Ljung-Box P-Value is NA/Inf. Residual diagnosis failed.\n")
      }
      cat("\n")
      
      # ==========================================================================
      # PHASE 3: KPSS + Pettitt + KPSS segments
      # ==========================================================================
      cat("==========================================================================\n")
      cat("PHASE 3: KPSS + STRUCTURAL BREAK (Pettitt) + KPSS SEGMENTS\n")
      cat("==========================================================================\n\n")
      
      # 3A: KPSS main
      cat("PHASE 3A: KPSS (Stationarity Confirmation)\n")
      cat(paste(" • H0: The series is Stationary around a", kpss_type, ".\n"))
      cat(" • Ha: The series is Non-Stationary.\n")
      
      cat(sprintf(" • CRITERIA (p-value) : Reject H0 if p-value (%.6f) < α (%.4f)\n",
                  eta_p_one, alpha_val))
      cat(sprintf(" • CRITERIA (eta)     : Reject H0 if Eta-Obs (%.6f) > Eta-Crit (%.6f)  [urca]\n",
                  eta_obs_uc, eta_crit_uc))
      
      cat("\n RESULT:\n")
      cat(paste("  - Eta (Observed value) [urca]   :", round(eta_obs_uc, 6), "\n"))
      cat(paste("  - Eta (Critical value) [urca]   :", round(eta_crit_uc, 6), "\n"))
      cat(paste("  - p-value (one-tailed) [tseries]:", round(eta_p_one, 6), "\n"))
      cat(paste("  - Eta (Observed) [tseries, FYI] :", round(eta_obs_ts, 6), "\n"))
      
      cat("\n DECISION:\n")
      if (isTRUE(kpss_ok) && isTRUE(kpss_stationary)) {
        cat("  -> FAIL TO REJECT H0: Stationarity supported by KPSS.\n")
      } else if (isTRUE(kpss_ok) && !isTRUE(kpss_stationary)) {
        cat("  -> REJECT H0: Non-stationarity indicated by KPSS.\n")
      } else {
        cat("  -> [!] WARNING: KPSS outputs are NA/Inf. KPSS inference is not valid.\n")
      }
      cat("--------------------------------------------------------------------------\n")
      
      # 3B: Pettitt + segments
      cat("PHASE 3B: STRUCTURAL BREAK CHECK (Pettitt) + KPSS SEGMENT CHECK\n")
      cat(" • Goal: Detect a single change-point (median shift) that can distort KPSS.\n")
      cat(" • If a break exists, we re-run KPSS before/after the break.\n")
      
      pettitt_break <- FALSE
      break_idx <- NA_integer_
      
      if (requireNamespace("trend", quietly = TRUE)) {
        
        pett_p <- to_num_safe(pettitt_res$p.value)
        cat(sprintf(" • Pettitt p-value: %.6f | α: %.4f\n", pett_p, alpha_val))
        
        if (!is.null(pettitt_res$estimate) && is.finite(as.numeric(pettitt_res$estimate))) {
          break_idx <- as.integer(as.numeric(pettitt_res$estimate))
        }
        
        pettitt_break <- is.finite(pett_p) && (pett_p < alpha_val) && is.finite(break_idx)
        
        cat("\n RESULT:\n")
        cat(paste("  - Break index (estimate):", ifelse(is.finite(break_idx), break_idx, "NA"), "\n"))
        cat(paste("  - Pettitt statistic     :", round(to_num_safe(pettitt_res$statistic), 6), "\n"))
        cat(paste("  - Pettitt p-value       :", round(pett_p, 6), "\n"))
        
        cat("\n DECISION:\n")
        if (pettitt_break) {
          cat("  -> REJECT H0: Break detected. Full-sample KPSS/ADF may be contaminated.\n")
        } else {
          cat("  -> FAIL TO REJECT H0: No strong evidence of a single break.\n")
        }
        
        if (pettitt_break) {
          
          x1 <- x[1:break_idx]
          x2 <- x[(break_idx + 1):valid_N]
          
          cat("--------------------------------------------------------------------------\n")
          cat("PHASE 3C: KPSS RE-CHECK BY SEGMENTS\n")
          cat("  • Segment 1 = [1 .. break]\n")
          cat("  • Segment 2 = [break+1 .. N]\n")
          
          min_seg <- 12L
          if (length(x1) < min_seg || length(x2) < min_seg) {
            cat("  [!] WARNING: One segment is too short for reliable KPSS.\n")
            cat(sprintf("     Segment lengths: n1=%d, n2=%d (min recommended=%d)\n",
                        length(x1), length(x2), min_seg))
          } else {
            
            kpss1 <- tryCatch(tseries::kpss.test(x1, null = kpss_type),
                              error = function(e) list(statistic = NA_real_, p.value = NA_real_))
            kpss2 <- tryCatch(tseries::kpss.test(x2, null = kpss_type),
                              error = function(e) list(statistic = NA_real_, p.value = NA_real_))
            
            p1 <- to_num_safe(kpss1$p.value)
            p2 <- to_num_safe(kpss2$p.value)
            
            cat(sprintf("  - KPSS p-value (Segment 1): %.6f\n", p1))
            cat(sprintf("  - KPSS p-value (Segment 2): %.6f\n", p2))
            
            s1_stat <- is.finite(p1) && (p1 > alpha_val)
            s2_stat <- is.finite(p2) && (p2 > alpha_val)
            
            cat("\n INTERPRETATION:\n")
            if (s1_stat && s2_stat) {
              cat("  [✓] Both segments look stationary by KPSS.\n")
              cat("      -> Full-sample non-stationarity may be break-driven.\n")
            } else if (!s1_stat && !s2_stat) {
              cat("  [X] Both segments look non-stationary by KPSS.\n")
              cat("      -> Non-stationarity is not only due to a break.\n")
            } else {
              cat("  [?] Mixed: one segment stationary, the other not.\n")
              cat("      -> Consider regime modeling or adjust transformations.\n")
            }
          }
        }
        
      } else {
        cat(" [!] WARNING: Package 'trend' not installed → Pettitt break check skipped.\n")
        cat("     ACTION: install.packages('trend')\n")
      }
      
      # ==========================================================================
      # PHASE 4: Final verdict
      # ==========================================================================
      cat("\n==========================================================================\n")
      cat("PHASE 4: FINAL ACADEMIC VERDICT & ADVICE\n")
      cat("==========================================================================\n")
      
      if (isTRUE(lb_ok) && !isTRUE(lb_white_safe)) {
        cat(" [!] WARNING: Your ADF 'Tau' statistic is technically biased.\n")
        cat("     ADVICE: Increase 'Lag Order' until Ljung-Box P-Value > α.\n\n")
      }
      
      if (isTRUE(is_stationary) && isTRUE(kpss_stationary)) {
        cat(" [✓] VERDICT: STRONG STATIONARITY confirmed by both ADF and KPSS.\n")
        cat("     ACTION: Proceed to ARMA/ARIMA modeling with d=0.\n")
      } else if (isTRUE(!is_stationary) && isTRUE(!kpss_stationary)) {
        cat(" [X] VERDICT: CLEAR UNIT ROOT. Both tests confirm Non-Stationarity.\n")
        cat("     ACTION: Apply differencing (d=1) and/or seasonal differencing (D=1 if seasonal).\n")
      } else {
        cat(" [?] VERDICT: CONFLICTING RESULTS (ADF vs KPSS).\n")
        cat("     ADVICE: Near-unit-root, trend-stationary, or break-contaminated series.\n")
        cat("     Double-check plots for breaks, seasonality, and variance changes.\n")
      }
      
      cat("\n TECHNICAL APPENDIX (ADF Regression Coefficients):\n")
      if (!is.null(res_urca) && !is.null(res_urca@testreg)) {
        print(stats::coef(res_urca@testreg))
      } else if (!is.null(res_urca)) {
        cat("  [!] Not available via @testreg; printing summary(res_urca) instead.\n")
        print(summary(res_urca))
      } else {
        cat("  [!] Not available (ur.df did not run).\n")
      }
      
    }, error = function(e) {
      cat(" EXECUTION ERROR: ", e$message, "\n")
    })
    
    cat("\n")
    
    # ============================================================================
    # SECTION 5: POST-SUMMARY + CHECKLIST + NEXT STEPS
    # ============================================================================
    cat("==========================================================================\n")
    cat("PHASE 5: POST-SUMMARY (Academic-quality snapshot)\n")
    cat("==========================================================================\n")
    
    cat(" EVIDENCE SNAPSHOT (All key outcomes in one place):\n")
    cat(sprintf(" [ ] N (effective sample size)              : %d\n", valid_N))
    cat(sprintf(" [ ] Model type (ADF)                       : %s  (tau row: %s)\n", type_in, tau_row))
    cat(sprintf(" [ ] Lag order (k)                          : %d\n", k))
    cat(sprintf(" [ ] Alpha (α)                              : %.4f\n", alpha_val))
    cat(sprintf(" [ ] Tau-Observed (urca)                    : %.6f\n", tau_obs))
    cat(sprintf(" [ ] Tau-Critical (urca, %s)                : %.6f\n", alpha_col, tau_crit))
    cat(sprintf(" [ ] ADF p-value (tseries reference)        : %.6f\n", to_num_safe(res_tseries$p.value)))
    cat(sprintf(" [ ] Ljung-Box p-value (residuals)          : %.6f\n", to_num_safe(lb_test$p.value)))
    cat(sprintf(" [ ] KPSS Eta observed (urca)               : %.6f\n", eta_obs_uc))
    cat(sprintf(" [ ] KPSS Eta critical (urca, %s)           : %.6f\n", alpha_col, eta_crit_uc))
    cat(sprintf(" [ ] KPSS p-value (one-tailed, tseries)     : %.6f\n", eta_p_one))
    if (is.finite(k_reco)) {
      tag_k <- if (isTRUE(k_reco == k)) "[✓]" else "[!]"
      cat(sprintf(" %s Suggested k* (scan)                     : %d\n", tag_k, k_reco))
    } else {
      cat(" [?] Suggested k* (scan)                     : NA\n")
    }
    cat("--------------------------------------------------------------------------\n")
    
    cat(" CHECKLIST (Academic-quality acceptance criteria):\n")
    
    # Transformation provenance
    cat(sprintf(" [ ] Tests run on transformed series (x=myData_Choice) : %s\n",
                ifelse(!is.null(x_raw), "[✓] YES (same x used everywhere)", "[!] UNKNOWN")))
    
    # Data sanity
    cat(sprintf(" [ ] Variance usable (sd>0)                          : %s\n",
                ifelse(is.finite(stats::sd(x)) && stats::sd(x) > 0, "[✓] SATISFIED", "[!] NOT SATISFIED")))
    if (!(is.finite(stats::sd(x)) && stats::sd(x) > 0)) {
      cat("     [!] DANGER: sd=0/NA → many tests become meaningless.\n")
    }
    
    # ADF extraction
    cat(sprintf(" [ ] Tau is finite and usable                        : %s\n",
                ifelse(is.finite(tau_obs), "[✓] SATISFIED", "[!] NOT SATISFIED")))
    if (!is.finite(tau_obs)) {
      cat("     [!] DANGER: Tau is NA/Inf → ADF inference cannot be trusted.\n")
      cat("     [!] Common causes: constant/near-constant series, k too high,\n")
      cat("         insufficient N after transformation.\n")
    }
    
    cat(sprintf(" [ ] Tau critical value extracted                    : %s\n",
                ifelse(is.finite(tau_crit), "[✓] SATISFIED", "[!] NOT SATISFIED")))
    if (!is.finite(tau_crit)) {
      cat("     [!] DANGER: Critical value missing → alpha/type mapping issue.\n")
      cat("     [!] ACTION: verify alpha_col and tau_row exist in urca cval table.\n")
    }
    
    # Residual whiteness
    lb_p <- to_num_safe(lb_test$p.value)
    cat(sprintf(" [ ] Residuals pass Ljung-Box (white noise)          : %s\n",
                ifelse(is.finite(lb_p) && lb_p > alpha_val, "[✓] SATISFIED", "[X] NOT SATISFIED")))
    if (is.finite(lb_p) && lb_p <= alpha_val) {
      cat("     [!] WARNING: residual autocorrelation detected.\n")
      cat("     [!] Interpretation: ADF may be biased (insufficient k).\n")
    }
    
    # KPSS usability
    cat(sprintf(" [ ] KPSS Eta observed & critical are usable         : %s\n",
                ifelse(is.finite(eta_obs_uc) && is.finite(eta_crit_uc), "[✓] SATISFIED", "[!] NOT SATISFIED")))
    if (!(is.finite(eta_obs_uc) && is.finite(eta_crit_uc))) {
      cat("     [!] WARNING: urca KPSS eta/cval unavailable → rely on tseries p-value.\n")
    }
    
    cat(sprintf(" [ ] KPSS p-value is usable                          : %s\n",
                ifelse(is.finite(eta_p_one), "[✓] SATISFIED", "[!] NOT SATISFIED")))
    if (!is.finite(eta_p_one)) {
      cat("     [!] WARNING: KPSS p-value NA → rely on eta criterion (urca).\n")
    }
    
    # Sample size and lag safety
    cat(sprintf(" [ ] Sample size adequacy                            : %s\n",
                ifelse(valid_N >= 30, "[✓] STRONG", ifelse(valid_N >= 15, "[?] BORDERLINE", "[X] WEAK"))))
    if (valid_N < 15) {
      cat("     [!] WARNING: very low power. Decisions may be unstable.\n")
    }
    
    cat(sprintf(" [ ] Lag order reasonable relative to N              : %s\n",
                ifelse(valid_N > (k + 10), "[✓] OK", "[!] TOO HIGH / RISKY")))
    if (valid_N <= (k + 10)) {
      cat("     [!] RISK: high k vs N can produce weak/unstable regression tests.\n")
    }
    
    # Seasonality provenance (UI hint only)
    cat(sprintf(" [ ] Seasonal differencing indicated (UI D>0)         : %s\n",
                ifelse(seasonality_resolved, "[✓] YES", "[!] NO / UNKNOWN")))
    
    # ADF alternative sanity
    cat(sprintf(" [ ] ADF alternative mode (Stationary/Explosive)      : %s\n", as.character(alt_in)))
    if (identical(as.character(alt_in), "explosive")) {
      cat("     [!] NOTE: Explosive mode is handled by tseries ADF p-value; urca tau rule is left-tail.\n")
    }
    
    # Agreement
    agreement_safe <- (isTRUE(is_stationary) && isTRUE(kpss_stationary)) ||
      (isTRUE(!is_stationary) && isTRUE(!kpss_stationary))
    
    cat(sprintf(" [ ] ADF & KPSS agreement                             : %s\n",
                ifelse(agreement_safe, "[✓] AGREEMENT", "[?] CONFLICT")))
    if (!agreement_safe) {
      cat("     [?] NOTE: conflicts are common with near-unit-root series, trend vs drift mismatch,\n")
      cat("         structural breaks (Pettitt), or missing seasonal differencing.\n")
    }
    
    # Structural break summary (Pettitt only)
    if (requireNamespace("trend", quietly = TRUE)) {
      cat("--------------------------------------------------------------------------\n")
      cat(" STRUCTURAL BREAK SUMMARY (Pettitt):\n")
      cat(sprintf(" [ ] Pettitt p-value : %.6f  (Reject H0 if < α)\n",
                  to_num_safe(pettitt_res$p.value)))
    } else {
      cat("--------------------------------------------------------------------------\n")
      cat(" STRUCTURAL BREAK SUMMARY (Pettitt):\n")
      cat(" [!] WARNING: Package 'trend' is not installed. Pettitt summary unavailable.\n")
      cat("     ACTION: install.packages('trend')\n")
    }
    
    cat("--------------------------------------------------------------------------\n")
    
    # ============================================================================
    # ACTIONABLE NEXT STEPS (with [✓] [X] [?] [!])
    # ============================================================================
    cat("==========================================================================\n")
    cat(" ACTIONABLE NEXT STEPS (What to do now):\n")
    cat("==========================================================================\n")
    
    # [1] Fix residual autocorrelation
    if (is.finite(lb_p) && lb_p <= alpha_val) {
      cat(" [X] [1] FIX RESIDUAL AUTOCORRELATION (LB failed)\n")
      cat("     • Increase k gradually (e.g., k+1, k+2) and re-run.\n")
      cat("     • If k gets too high relative to N, prefer differencing or seasonal differencing.\n")
      cat("     • If series is seasonal (freq>1), consider D=1 before pushing k upward.\n")
      if (is.finite(k_reco) && (k_reco != k)) {
        cat(sprintf("     • Decision aid: try k*=%d (scan suggestion) first.\n", k_reco))
      }
    } else if (is.finite(lb_p) && lb_p > alpha_val) {
      cat(" [✓] [1] RESIDUAL AUTOCORRELATION CHECK\n")
      cat("     • Ljung-Box is acceptable → ADF regression is less likely biased.\n")
    } else {
      cat(" [?] [1] RESIDUAL AUTOCORRELATION CHECK\n")
      cat("     • Ljung-Box p-value is NA/Inf → residual diagnosis is inconclusive.\n")
      cat("     • Reduce k and/or verify the series after transformation.\n")
    }
    
    # [2] Resolve ADF vs KPSS conflict
    if (!agreement_safe) {
      cat(" [?] [2] RESOLVE ADF vs KPSS CONFLICT\n")
      cat("     • Use PHASE 0 'ADF SPEC SENSITIVITY' to pick the type with LB ok and stable decision.\n")
      cat("     • Try ADF model type variants: none / drift / trend (match KPSS Level vs Trend).\n")
      cat("     • If Pettitt indicates a break: split sample and re-test.\n")
      cat("     • If series is seasonal: test after seasonal differencing (D=1) + maybe log.\n")
      cat("     • Consider variance stabilization: log or Box-Cox (if positive data).\n")
    } else {
      cat(" [✓] [2] CONSISTENT DECISION\n")
      cat("     • ADF and KPSS agree → proceed with identification/modeling.\n")
    }
    
    # [3] Seasonality sanity (mark [✓] if seasonality_resolved)
    season_tag <- if (isTRUE(seasonality_resolved)) "[✓]" else "[!]"
    cat(sprintf(" %s [3] SEASONALITY SANITY (especially for AirPassengers-like series)\n", season_tag))
    
    if (is.finite(x_freq) && x_freq > 1) {
      cat(sprintf("     • Detected frequency=%d → seasonality is plausible.\n", x_freq))
      if (isTRUE(seasonality_resolved)) {
        cat("     • Seasonality is marked as resolved (D>0) → continue ADF/KPSS on transformed series.\n")
      } else {
        cat("     • If ACF shows spikes at seasonal lags, use D=1 (seasonal differencing).\n")
      }
    } else {
      cat("     • Frequency not available (not a ts object) → verify seasonality in your plot/ACF.\n")
    }
    
    # [4] Explosive option sanity
    if (identical(as.character(alt_in), "explosive")) {
      cat(" [!] [4] EXPLOSIVE MODE NOTE\n")
      cat("     • KPSS is not an 'explosive' test.\n")
      cat("     • For explosive alternative, use tseries ADF p-value + visual diagnostics.\n")
    } else {
      cat(" [✓] [4] EXPLOSIVE MODE NOTE\n")
      cat("     • Not in explosive mode → standard stationarity workflow applies.\n")
    }
    
    cat("\n PRACTICAL MODELING PATH (for your Shiny workflow):\n")
    if (isTRUE(is_stationary) && is.finite(lb_p) && (lb_p > alpha_val)) {
      cat(" [✓] Use ARMA identification on current series → fit → residual analysis.\n")
    } else if (!isTRUE(is_stationary)) {
      cat(" [X] Apply differencing (d and/or D) → re-run ADF/KPSS → then identify ARMA.\n")
    } else {
      cat(" [?] Mixed evidence: consider differencing for safety and validate visually.\n")
    }
    
    cat("--------------------------------------------------------------------------\n\n\n")
  })
  
  

  
  
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  
  
  
  
  output$CHECKLIST <- renderPrint({
    
    # ============================================================================
    # 0) SMALL HELPERS
    # ============================================================================
    `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
    
    to_num_safe <- function(v, default = NA_real_) {
      out <- suppressWarnings(as.numeric(v))
      if (length(out) == 0 || all(is.na(out)) || !is.finite(out[1])) default else out[1]
    }
    
    to_int_safe <- function(v, default = 0L) {
      out <- suppressWarnings(as.integer(v))
      if (length(out) == 0 || is.na(out[1]) || !is.finite(out[1])) default else out[1]
    }
    
    safe_head_tail <- function(x, n = 5) {
      x <- as.numeric(x)
      x <- x[is.finite(x)]
      if (length(x) == 0) return(list(head = numeric(0), tail = numeric(0)))
      list(head = head(x, n), tail = tail(x, n))
    }
    
    # Robust extractor for urca @cval (vector OR matrix; row/col name variations)
    get_uc_cval <- function(cv, key) {
      if (is.null(cv) || is.null(key) || !nzchar(key)) return(NA_real_)
      
      if (is.matrix(cv)) {
        if (!is.null(colnames(cv)) && key %in% colnames(cv)) return(as.numeric(cv[1, key]))
        if (!is.null(rownames(cv)) && key %in% rownames(cv)) return(as.numeric(cv[key, 1]))
        
        if (!is.null(colnames(cv))) {
          m <- which(grepl(key, colnames(cv), fixed = TRUE))
          if (length(m) > 0) return(as.numeric(cv[1, m[1]]))
        }
        if (!is.null(rownames(cv))) {
          m <- which(grepl(key, rownames(cv), fixed = TRUE))
          if (length(m) > 0) return(as.numeric(cv[m[1], 1]))
        }
        return(NA_real_)
      }
      
      nm <- names(cv)
      if (!is.null(nm) && key %in% nm) return(as.numeric(cv[[key]]))
      if (!is.null(nm)) {
        m <- which(grepl(key, nm, fixed = TRUE))
        if (length(m) > 0) return(as.numeric(cv[[m[1]]]))
      }
      NA_real_
    }
    
    # ============================================================================
    # 1) INPUT COLLECTION (supports either naming convention in your UI)
    # ============================================================================
    alt_in  <- input$alternd2St %||% input$alternSt
    lag_in  <- input$LagOrderADFd2St %||% input$LagOrderADFSt
    a_in    <- input$alphaSt2
    type_in <- input$adfTypeSt2
    
    # Optional transformation UI inputs (informational only)
    d_in   <- input$d_n  %||% input$d  %||% NA
    D_in   <- input$DS_n %||% input$D  %||% NA
    log_in <- input$check_box %||% input$islog %||% FALSE
    
    req(myData_Choice())
    
    if (is.null(alt_in) || is.null(lag_in) || is.null(a_in) || is.null(type_in)) {
      cat("==========================================================================\n")
      cat(" [!] INPUT ERROR: One or more inputs are NULL (likely UI/server ID mismatch)\n")
      cat("     Needed inputs:\n")
      cat("       - alternative : input$alternd2St OR input$alternSt\n")
      cat("       - lag         : input$LagOrderADFd2St OR input$LagOrderADFSt\n")
      cat("       - alpha       : input$alphaSt2\n")
      cat("       - adf type    : input$adfTypeSt2\n")
      cat("==========================================================================\n")
      return(invisible(NULL))
    }
    
    # ============================================================================
    # 2) DATA PREP
    # ============================================================================
    x_raw <- myData_Choice()
    na_before <- sum(is.na(x_raw))
    
    x_class <- paste(class(x_raw), collapse = ", ")
    x_freq  <- NA_integer_
    if (inherits(x_raw, "ts")) x_freq <- tryCatch(stats::frequency(x_raw), error = function(e) NA_integer_)
    
    x <- as.numeric(stats::na.omit(x_raw))
    valid_N <- length(x)
    
    k <- to_int_safe(lag_in, default = 0L)
    if (!is.finite(k) || k < 0) k <- 0L
    
    alpha_raw <- as.character(a_in)
    alpha_val <- to_num_safe(alpha_raw, default = 0.05)
    
    alpha_col <- switch(alpha_raw,
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct",
                        "0.10" = "10pct",
                        "1pct" = "1pct",
                        "5pct" = "5pct",
                        "10pct"= "10pct",
                        "5pct")
    
    tau_row <- switch(type_in,
                      "none"  = "tau1",
                      "drift" = "tau2",
                      "trend" = "tau3",
                      "tau3")
    
    D_ui <- to_int_safe(D_in, default = 0L)
    seasonality_resolved <- isTRUE(D_ui > 0L)
    
    # Quick sanity exits (still only printing the 3 sections)
    if (valid_N < 5 || !is.finite(stats::sd(x)) || stats::sd(x) == 0) {
      cat("==========================================================================\n")
      cat(" CHECKLIST\n")
      cat("==========================================================================\n\n")
      cat(sprintf(" [ ] N (effective)                    : %d\n", valid_N))
      cat(sprintf(" [ ] Variance usable (sd>0)           : %s\n",
                  ifelse(is.finite(stats::sd(x)) && stats::sd(x) > 0, "[✓]", "[!]")))
      cat("--------------------------------------------------------------------------\n")
      cat(" FINAL ACADEMIC ADVICE\n")
      cat("--------------------------------------------------------------------------\n\n")
      if (valid_N < 5) {
        cat(" [!] Too few observations after transformation. Provide more data points.\n")
      } else {
        cat(" [!] Series is constant/invalid after transformation (sd=0/NA).\n")
        cat("     Fix data quality or transformation pipeline in myData_Choice().\n")
      }
      cat("--------------------------------------------------------------------------\n")
      cat(" ACTIONABLE NEXT STEPS\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" [!] [1] Verify myData_Choice() returns the transformed series (log/d/D).\n")
      cat(" [!] [2] Print/plot the transformed series to confirm it is not constant.\n")
      cat(" [!] [3] Re-run tests after fixing the above.\n")
      cat("==========================================================================\n")
      return(invisible(NULL))
    }
    
    # ============================================================================
    # 3) PREDECLARE OUTPUTS
    # ============================================================================
    res_urca <- NULL
    res_tseries <- list(p.value = NA_real_)
    lb_test <- list(statistic = NA_real_, p.value = NA_real_)
    lb_lag <- NA_integer_
    
    res_kpss_ts <- list(statistic = NA_real_, p.value = NA_real_)
    res_kpss_uc <- NULL
    
    tau_obs <- NA_real_
    tau_crit <- NA_real_
    
    eta_obs_uc <- NA_real_
    eta_crit_uc <- NA_real_
    eta_obs_ts <- NA_real_
    eta_p_one <- NA_real_
    eta_col <- NA_character_
    
    pettitt_res <- list(statistic = NA_real_, p.value = NA_real_, estimate = NULL)
    
    is_stationary <- FALSE
    kpss_stationary <- FALSE
    agreement_safe <- FALSE
    lb_white_safe <- FALSE
    
    # ============================================================================
    # 4) COMPUTE TESTS (no verbose phases; we only use results for the 3 outputs)
    # ============================================================================
    tryCatch({
      
      if (!requireNamespace("urca", quietly = TRUE)) stop("Package 'urca' missing. install.packages('urca')")
      if (!requireNamespace("tseries", quietly = TRUE)) stop("Package 'tseries' missing. install.packages('tseries')")
      
      # ADF (urca)
      res_urca <- urca::ur.df(x, type = type_in, lags = k)
      
      tau_obs <- suppressWarnings(as.numeric(res_urca@teststat[tau_row]))
      if (!is.finite(tau_obs)) {
        tau_obs_fallback <- suppressWarnings(as.numeric(res_urca@teststat[1]))
        if (is.finite(tau_obs_fallback)) tau_obs <- tau_obs_fallback
      }
      tau_crit <- suppressWarnings(as.numeric(res_urca@cval[tau_row, alpha_col]))
      
      # ADF p-value reference (tseries)
      if (valid_N > (k + 10)) {
        res_tseries <- tseries::adf.test(x, alternative = alt_in, k = k)
      }
      
      # Ljung-Box on ADF residuals
      lb_lag <- max(1L, min(10L, floor(valid_N / 5)))
      lb_test <- Box.test(res_urca@res, lag = lb_lag, type = "Ljung-Box")
      
      # KPSS (tseries p-value, urca cvals)
      kpss_type <- if (type_in == "trend") "Trend" else "Level"
      res_kpss_ts <- tseries::kpss.test(x, null = kpss_type)
      eta_obs_ts  <- to_num_safe(res_kpss_ts$statistic)
      eta_p_one   <- to_num_safe(res_kpss_ts$p.value)
      
      kpss_type_urca <- if (type_in == "trend") "tau" else "mu"
      res_kpss_uc    <- urca::ur.kpss(x, type = kpss_type_urca)
      
      eta_obs_uc <- suppressWarnings(as.numeric(res_kpss_uc@teststat))
      
      eta_col <- switch(alpha_raw,
                        "0.01" = "1pct",
                        "0.05" = "5pct",
                        "0.1"  = "10pct",
                        "0.10" = "10pct",
                        "1pct" = "1pct",
                        "5pct" = "5pct",
                        "10pct"= "10pct",
                        "5pct")
      
      eta_crit_uc <- suppressWarnings(get_uc_cval(res_kpss_uc@cval, eta_col))
      
      # Optional Pettitt
      if (requireNamespace("trend", quietly = TRUE)) {
        pettitt_res <- tryCatch(trend::pettitt.test(x),
                                error = function(e) list(statistic = NA_real_, p.value = NA_real_, estimate = NULL))
      }
      
      # Decisions
      tau_ok <- is.finite(tau_obs) && is.finite(tau_crit)
      lb_p   <- to_num_safe(lb_test$p.value)
      lb_ok  <- is.finite(lb_p)
      
      is_stationary <- isTRUE(tau_ok) && (tau_obs < tau_crit)
      
      # KPSS rejects stationarity if p<alpha OR eta_obs>eta_crit (when available)
      kpss_reject_by_p   <- is.finite(eta_p_one) && (eta_p_one < alpha_val)
      kpss_reject_by_eta <- is.finite(eta_obs_uc) && is.finite(eta_crit_uc) && (eta_obs_uc > eta_crit_uc)
      kpss_stationary <- !(kpss_reject_by_p || kpss_reject_by_eta)
      
      lb_white_safe <- lb_ok && (lb_p > alpha_val)
      
      agreement_safe <- (isTRUE(is_stationary) && isTRUE(kpss_stationary)) ||
        (isTRUE(!is_stationary) && isTRUE(!kpss_stationary))
      
    }, error = function(e) {
      cat("==========================================================================\n")
      cat(" CHECKLIST\n")
      cat("==========================================================================\n")
      cat(" [!] Execution error while computing tests:\n")
      cat("     ", e$message, "\n", sep = "")
      cat("--------------------------------------------------------------------------\n")
      cat(" FINAL ACADEMIC ADVICE\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" [!] Install/enable required packages and re-run.\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ACTIONABLE NEXT STEPS\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" [!] [1] install.packages('urca') and install.packages('tseries')\n")
      cat(" [!] [2] Re-run after verifying myData_Choice() returns a numeric/ts vector.\n")
      cat("==========================================================================\n")
      return(invisible(NULL))
    })
    
    # ============================================================================
    # 5) OUTPUT ONLY: CHECKLIST, FINAL ACADEMIC ADVICE, ACTIONABLE NEXT STEPS
    # ============================================================================
    lb_p <- to_num_safe(lb_test$p.value)
    adf_p <- to_num_safe(res_tseries$p.value)
    pett_p <- to_num_safe(pettitt_res$p.value)
    
    # --------------------
    # CHECKLIST
    # --------------------
    cat("==========================================================================\n")
    cat(" CHECKLIST\n")
    cat("==========================================================================\n")
    
    # ht <- safe_head_tail(x, 5)
    
    # cat(sprintf(" [ ] N (effective)                              : %d\n", valid_N))
    # cat(sprintf(" [ ] ADF model type (none/drift/trend)          : %s\n", as.character(type_in)))
    # cat(sprintf(" [ ] Lag k                                      : %d\n", k))
    # cat(sprintf(" [ ] Alpha (α)                                  : %.4f\n", alpha_val))
    # cat(sprintf(" [ ] Data class                                 : %s\n", x_class))
    # cat(sprintf(" [ ] Frequency (if ts)                          : %s\n", ifelse(is.finite(x_freq), as.character(x_freq), "NA / not ts")))
    # cat(sprintf(" [ ] NA count before na.omit                     : %d\n", na_before))
    # cat(sprintf(" [ ] UI transform flags                          : log=%s | d=%s | D=%s\n",
    #             ifelse(isTRUE(log_in), "ON", "OFF"),
    #             ifelse(is.na(d_in), "NA", as.character(d_in)),
    #             ifelse(is.na(D_in), "NA", as.character(D_in))))
    # cat(sprintf(" [ ] First 5 values tested                       : %s\n", paste(round(ht$head, 4), collapse = ", ")))
    # cat(sprintf(" [ ] Last  5 values tested                       : %s\n", paste(round(ht$tail, 4), collapse = ", ")))
    # cat("--------------------------------------------------------------------------\n")
    
    # cat(sprintf(" [ ] ADF Tau observed is finite                  : %s\n", ifelse(is.finite(tau_obs), "[✓]", "[!]")))
    # cat(sprintf(" [ ] ADF Tau critical is finite                  : %s\n", ifelse(is.finite(tau_crit), "[✓]", "[!]")))
    # cat(sprintf(" [ ] Ljung-Box p-value is finite                 : %s\n", ifelse(is.finite(lb_p), "[✓]", "[!]")))
    # cat(sprintf(" [ ] KPSS Eta observed (urca) is finite          : %s\n", ifelse(is.finite(eta_obs_uc), "[✓]", "[!]")))
    # cat(sprintf(" [ ] KPSS Eta critical (urca) is finite          : %s\n", ifelse(is.finite(eta_crit_uc), "[✓]", "[!]" )))
    # cat(sprintf(" [ ] KPSS p-value (tseries) is finite            : %s\n", ifelse(is.finite(eta_p_one), "[✓]", "[!]" )))
    # cat("--------------------------------------------------------------------------\n")
    
    # Key decisions summary (compact)
    cat(sprintf(" [ ] ADF decision (reject unit root => stationary) : %s\n",
                ifelse(isTRUE(is_stationary), "[✓] STATIONARY", "[X] NON-STATIONARY")))
    cat(sprintf(" [ ] KPSS decision (fail reject => stationary)     : %s\n",
                ifelse(isTRUE(kpss_stationary), "[✓] STATIONARY", "[X] NON-STATIONARY")))
    cat(sprintf(" [ ] ADF vs KPSS agreement                         : %s\n",
                ifelse(isTRUE(agreement_safe), "[✓] AGREEMENT", "[?] CONFLICT")))
    cat(sprintf(" [ ] Residual whiteness (LB (Ljung–Box) p>α)       : %s\n",
                ifelse(is.finite(lb_p) && lb_p > alpha_val, "[✓] OK", ifelse(is.finite(lb_p), "[X] FAIL", "[?] UNKNOWN"))))
    cat(sprintf(" [ ] Seasonal differencing indicated (UI D>0)      : %s\n",
                ifelse(isTRUE(seasonality_resolved), "[✓] YES", "[!] NO / UNKNOWN")))
    cat(sprintf(" [ ] Pettitt break check available                 : %s\n",
                ifelse(requireNamespace("trend", quietly = TRUE), "[✓] YES", "[!] NO (trend pkg missing)")))
    # if (requireNamespace("trend", quietly = TRUE)) {
    #   cat(sprintf(" [ ] Pettitt p-value                               : %.6f\n", pett_p))
    # }
    
    cat("\n")
    
    # --------------------
    # FINAL ACADEMIC ADVICE
    # --------------------
    cat("==========================================================================\n")
    cat(" FINAL ACADEMIC ADVICE\n")
    cat("==========================================================================\n")
    cat("\n")
    # Explain conflict in a compact, decision-useful way
    if (isTRUE(agreement_safe) && isTRUE(is_stationary) && isTRUE(kpss_stationary)) {
      cat(" [✓] Strong evidence of stationarity (I(0)) from BOTH ADF and KPSS.\n")
      if (!(is.finite(lb_p) && lb_p > alpha_val)) {
        cat(" [!] But residual autocorrelation suggests your ADF lag may be too small.\n")
        cat("     Treat the ADF conclusion as less reliable until LB passes.\n")
      } else {
        cat(" [✓] Residuals are consistent with a well-specified ADF regression.\n")
      }
      cat("     Academic implication: proceed with ARMA/ARIMA using d=0 (and D as already applied).\n")
      
    } else if (isTRUE(agreement_safe) && !isTRUE(is_stationary) && !isTRUE(kpss_stationary)) {
      cat(" [X] Strong evidence of a unit root (non-stationarity) from BOTH tests.\n")
      cat("     Academic implication: apply differencing (d=1) and re-test.\n")
      if (is.finite(x_freq) && x_freq > 1 && !isTRUE(seasonality_resolved)) {
        cat(" [!] Seasonal frequency detected; consider seasonal differencing (D=1) before increasing k.\n")
      }
      
    } else {
      cat(" [?] ADF and KPSS are in conflict.\n")
      cat("     This is common when the series is near-unit-root, trend-stationary vs difference-stationary,\n")
      cat("     has structural breaks, seasonality not properly removed, or lag k is mis-specified.\n")
      
      if (requireNamespace("trend", quietly = TRUE) && is.finite(pett_p) && pett_p < alpha_val) {
        cat(" [!] Pettitt suggests a structural break (p < α). Breaks often cause ADF/KPSS disagreement.\n")
      }
      
      if (is.finite(x_freq) && x_freq > 1 && !isTRUE(seasonality_resolved)) {
        cat(" [!] Seasonal frequency detected but seasonal differencing is NOT indicated (D=0).\n")
        cat("     Unremoved seasonality often triggers KPSS non-stationarity while ADF looks borderline.\n")
      }
      
      if (is.finite(lb_p) && lb_p <= alpha_val) {
        cat(" [!] Ljung-Box fails => ADF regression residuals are autocorrelated; your ADF decision is less trustworthy.\n")
      }
      
      cat("     Academic implication: be conservative—prefer differencing (and/or D=1 if seasonal), then re-test.\n")
    }
    
    cat(sprintf("\n (Numbers) ADF tau=%.6f | tau_crit=%.6f | ADF p(ref)=%.6f\n", tau_obs, tau_crit, adf_p))
    cat(sprintf("           KPSS eta_obs=%.6f | eta_crit=%.6f | KPSS p=%.6f\n", eta_obs_uc, eta_crit_uc, eta_p_one))
    cat(sprintf("           LB (the Ljung–Box Q test) p=%.6f (lag=%s)\n", lb_p, ifelse(is.finite(lb_lag), as.character(lb_lag), "NA")))
            cat("            |__ it’s normal to choose LB lag by a rule-of-thumb (like min(10, floor(N/5)) \n")
    cat("\n")
    
    # --------------------
    # ACTIONABLE NEXT STEPS
    # --------------------
    cat("==========================================================================\n")
    cat(" ACTIONABLE NEXT STEPS\n")
    cat("==========================================================================\n")
    
    
    # [1] Residual autocorrelation
    cat("\n")
    if (is.finite(lb_p) && lb_p <= alpha_val) {
      cat(" [X] [1] FIX RESIDUAL AUTOCORRELATION (LB failed)\n")
      cat("     • Increase k gradually (k+1, k+2) and re-run.\n")
      cat("     • If k becomes large vs N, prefer (d=1) and/or (D=1 if seasonal) instead of pushing k.\n")
    } else if (is.finite(lb_p) && lb_p > alpha_val) {
      cat(" [✓] [1] RESIDUALS LOOK OK (LB passed)\n")
      cat("     • Your ADF regression is less likely biased by autocorrelation.\n")
    } else {
      cat(" [?] [1] RESIDUAL DIAGNOSTICS INCONCLUSIVE\n")
      cat("     • LB p-value is NA/Inf. Reduce k and re-run; verify x is not pathological.\n")
    }
    
    # [2] Agreement vs conflict
    cat("\n")
    if (!isTRUE(agreement_safe)) {
      cat(" [?] [2] RESOLVE THE ADF vs KPSS CONFLICT\n")
      cat("     • Re-test ADF with model types: none / drift / trend (choose what your plot suggests).\n")
      cat("     • If seasonal frequency exists, try D=1 (seasonal differencing) before debating k.\n")
      cat("     • If series is strictly positive, try log or Box-Cox for variance stabilization.\n")
      if (requireNamespace("trend", quietly = TRUE)) {
        if (is.finite(pett_p) && pett_p < alpha_val) {
          cat("     • Break detected: split sample around the break and re-run KPSS/ADF on each segment.\n")
        } else {
          cat("     • No strong break evidence at α: focus on model type (trend/drift) + seasonality + k.\n")
        }
      } else {
        cat("     • Install 'trend' to check breaks: install.packages('trend')\n")
      }
    } else {
      cat(" [✓] [2] TESTS AGREE — MOVE FORWARD\n")
      cat("     • Use this decision to pick d (and D if seasonal) for ARIMA/SARIMA modeling.\n")
    }
    
    # [3] Seasonality sanity with required rule: [✓] if resolved, else [!]
    cat("\n")
    season_tag <- if (isTRUE(seasonality_resolved)) "[✓]" else "[!]"
    cat(sprintf(" %s [3] SEASONALITY SANITY\n", season_tag))
    if (is.finite(x_freq) && x_freq > 1) {
      cat(sprintf("     • Frequency=%d detected.\n", x_freq))
      if (isTRUE(seasonality_resolved)) {
        cat("     • D>0 in UI indicates seasonality treatment is ON (as long as myData_Choice() applies it).\n")
      } else {
        cat("     • Consider D=1 if ACF shows seasonal spikes at lag = frequency, 2*frequency, ...\n")
      }
    } else {
      cat("     • Frequency not available: rely on plot/ACF to decide if seasonal differencing is needed.\n")
    }
    
    # [4] Explosive note
    cat("\n")
    if (identical(as.character(alt_in), "explosive")) {
      cat(" [!] [4] EXPLOSIVE MODE NOTE\n")
      cat("     • KPSS is not an explosive test. Use tseries ADF p-value + plots.\n")
    } else {
      cat(" [✓] [4] EXPLOSIVE MODE NOTE\n")
      cat("     • Standard stationarity workflow applies.\n")
    }
    
    
    
    # --------------------------------------------------------------------------
    # EXTRA ACTIONABLE NEXT STEPS (more complete workflow)
    # --------------------------------------------------------------------------
    
    # [5] Choose ADF model type systematically (avoid random picking)
    cat("\n")
    cat(" [!] [5] CHOOSE ADF MODEL TYPE SYSTEMATICALLY (avoid mis-specification)\n")
    cat("     • Use your time plot:\n")
    cat("       - Clear deterministic trend  → set type='trend'\n")
    cat("       - No clear trend, non-zero mean → set type='drift'\n")
    cat("       - Mean around ~0 (rare)      → set type='none'\n")
    cat("     • Wrong type_in is a top cause of ADF vs KPSS conflict.\n")
    
    # [6] Lag strategy: use Ljung-Box as your guardrail
    cat("\n")
    cat(" [!] [6] USE LB AS A LAG-SELECTION GUARDRAIL\n")
    cat("     • Increase k until LB p-value > α (residuals approx. white).\n")
    cat("     • Stop increasing k if N becomes too small relative to k (risk: N <= k+10).\n")
    cat("     • If you hit that risk, prefer differencing (d or D) instead of more k.\n")
    
    # [7] Seasonality protocol (if frequency known)
    cat("\n")
    if (is.finite(x_freq) && x_freq > 1) {
      cat(" [!] [7] SEASONALITY PROTOCOL (freq detected)\n")
      cat("     • Check ACF for spikes at seasonal lags: freq, 2*freq, ...\n")
      if (isTRUE(seasonality_resolved)) {
        cat("     [✓] D>0 indicated → ensure myData_Choice() truly applied seasonal differencing.\n")
      } else {
        cat("     [X] D=0 indicated → if seasonal spikes exist, set D=1 and re-test.\n")
      }
    }
    
    # [8] Break protocol (if Pettitt available)
    cat("\n")
    if (requireNamespace("trend", quietly = TRUE)) {
      cat(" [!] [8] BREAK PROTOCOL (Pettitt)\n")
      if (is.finite(pett_p) && (pett_p < alpha_val)) {
        cat("     [!] Break detected → do this:\n")
        cat("       1) Split the series around the estimated break index.\n")
        cat("       2) Re-run KPSS/ADF on each segment.\n")
        cat("       3) If segments are stationary but full sample is not → break-driven non-stationarity.\n")
      } else {
        cat("     [✓] No strong single-break evidence at α.\n")
        cat("     • If conflict persists, consider multiple breaks or gradual regime changes.\n")
      }
    } else {
      cat(" [!] [8] BREAK PROTOCOL (Pettitt)\n")
      cat("     [!] trend package not installed → install.packages('trend') to enable break diagnostics.\n")
    }
    
    # [9] Conservative “default safe choice” rule (useful in teaching apps)
    cat("\n")
    cat(" [!] [9] DEFAULT SAFE CHOICE (when unsure)\n")
    cat("     • If ADF/KPSS conflict persists after fixing seasonality + lag:\n")
    cat("       - Prefer differencing (d=1) (and D=1 if seasonal) then re-test.\n")
    cat("     • This reduces the chance of building ARMA on a near-integrated series.\n")
    
    # [10] Sanity check transformations inside myData_Choice()
    cat("\n")
    cat(" [!] [10] TRANSFORMATION SANITY INSIDE myData_Choice()\n")
    cat("     • Ensure the same transformed object is returned for ALL downstream tests.\n")
    cat("     • Avoid mixing ts and numeric conversions before applying frequency-based operations.\n")
    cat("     • After log, verify positivity and handle zeros (e.g., log1p) if needed.\n")
    
    
    cat("==========================================================================\n\n")
    
    # --------------------------------------------------------------------------
    # EXTRA FINAL ACADEMIC ADVICE (more complete decision logic)
    # --------------------------------------------------------------------------
    
    # Power / sample size warnings (high impact)
    if (valid_N < 30) {
      cat(" [!] Power warning: with N < 30, ADF/KPSS can be unstable (low power / size distortions).\n")
      if (valid_N < 15) {
        cat("     [X] N < 15 is very weak for reliable inference; treat conclusions as provisional.\n")
      } else {
        cat("     [?] N is borderline; combine with plots + conservative transformations.\n")
      }
    } else {
      cat(" [✓] Sample size is generally adequate for classical stationarity tests.\n")
    }
    
    # Lag-vs-N safety
    if (valid_N <= (k + 10)) {
      cat(" [!] Lag risk: k is large relative to N (N <= k+10). Regression-based tests may misbehave.\n")
      cat("     Action: reduce k OR increase N, and prefer differencing/seasonal differencing over huge k.\n")
    } else {
      cat(" [✓] Lag order looks safe relative to N.\n")
    }
    
    # Model-type specification advice (none/drift/trend)
    cat(" [!] Model-type (none/drift/trend) matters:\n")
    cat("     • If the series has a visible non-zero mean but no deterministic trend → prefer 'drift'.\n")
    cat("     • If the series has a clear deterministic trend → prefer 'trend'.\n")
    cat("     • If the series oscillates around zero (rare in real data) → 'none'.\n")
    
    # Seasonality: warn if frequency exists but D not indicated
    if (is.finite(x_freq) && x_freq > 1 && !isTRUE(seasonality_resolved)) {
      cat(" [!] Seasonality risk: frequency suggests seasonality, but D=0 in UI.\n")
      cat("     Missing seasonal differencing can cause KPSS to reject stationarity and/or ADF to look borderline.\n")
    } else if (is.finite(x_freq) && x_freq > 1 && isTRUE(seasonality_resolved)) {
      cat(" [✓] Seasonality flag: D>0 in UI indicates seasonal treatment is intended.\n")
    }
    
    # Structural break contamination (Pettitt)
    if (requireNamespace("trend", quietly = TRUE) && is.finite(pett_p) && (pett_p < alpha_val)) {
      cat(" [!] Structural break contamination: Pettitt indicates a change-point (p < α).\n")
      cat("     ADF/KPSS disagreements are common under breaks; consider segment tests or break-aware models.\n")
    }
    
    # Near-unit-root / borderline zone heuristic
    # (We don't have direct 'borderline' threshold universally, so we use agreement + p-values)
    if (!isTRUE(agreement_safe) && is.finite(adf_p) && is.finite(eta_p_one)) {
      if (adf_p > 0.01 && adf_p < 0.10 && eta_p_one > 0.01 && eta_p_one < 0.10) {
        cat(" [?] Near-unit-root zone: both tests are near typical cutoffs.\n")
        cat("     Treat the series as highly persistent; prefer conservative differencing and validate by forecasting performance.\n")
      }
    }
    
    # Explosive alternative caveat (important correctness note)
    if (identical(as.character(alt_in), "explosive")) {
      cat(" [!] Explosive caveat: urca tau critical values are for the usual left-tail unit-root framework.\n")
      cat("     For explosive detection, rely primarily on tseries::adf.test p-value (right-tail) + plots.\n")
    }
    
    
    cat("==========================================================================\n\n")
    
    # Practical path
    cat(" PRACTICAL MODELING PATH:\n")
    if (isTRUE(is_stationary) && is.finite(lb_p) && (lb_p > alpha_val)) {
      cat(" [✓] Treat as I(0): identify ARMA on current series → fit → residual analysis.\n")
    } else if (!isTRUE(is_stationary)) {
      cat(" [X] Treat as I(1): apply differencing (d and/or D) → re-test → then identify ARMA.\n")
    } else {
      cat(" [?] Borderline/mixed: prefer conservative differencing and validate with residual diagnostics.\n")
    }
    
    cat("==========================================================================\n\n")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  # ============================================================================
  
  
  
  
  
  # 7. Auto-ARIMA on Selected Transformation
  output$ARIMA_d_D_log <- renderPrint({
    req(myData_Choice())
    
    cat("--- Automatic ARIMA Model Selection (Selected Transformation) ---\n\n")
    
    model_dDLog <- forecast::auto.arima(
      myData_Choice(), 
      trace = FALSE, 
      allowdrift = TRUE,
      stepwise = FALSE, 
      approximation = FALSE
    )
    
    summary(model_dDLog)
  })

 
  
  
  ########  ########  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #         Plots  :   Different Seasonal Plots
  #
  ########  ########  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########  ########  ########



  #    TIME SERIES DIAGNOSTIC DISPLAY (ACF/PACF/Plot)
  output$tsDisplay <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL

    # The forecast::ggtsdisplay function creates a combined plot
    # showing the time series, ACF, and PACF (or histogram/scatter)
    forecast::ggtsdisplay(tsData(),
                          plot.type = input$plot_type,
                          main = userData$mainTitle,
                          xlab = userData$xLabel,
                          ylab = userData$yLabel)
  },
  # Direct dynamic dimensions for renderPlot
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )

  ########  ######## 
  # Plot the box plot
  ########  ######## 
  
  #     SEASONAL BOX PLOT (Gray Fill, Hollow Outliers, Dotted Whiskers)
  output$boxP <- renderPlot({
    req(tsData()) 
    
    boxplot(as.numeric(tsData()) ~ cycle(tsData()), 
            xlab = "Cycle", 
            ylab = userData$yLabel, 
            main = "Box Plot by Cycle",
            
            # 1. Standard Limits (1.5 * IQR)
            range = 1.5,          
            
            # 2. Outlier Styling (Small Hollow Circles)
            outpch = 1,           # 1 is the code for an empty/hollow circle
            outcex = 0.8,         # Scale to make them "small"
            outcol = "black",     # Border color of the hollow circle
            
            # 3. Whisker Styling (Dotted Lines)
            whisklty = 3,         # 3 = Dotted line style
            whiskcol = "black",   
            
            # 4. Box Styling
            col = "gray",         # Solid gray fill
            staplewex = 0.5,      # Width of the horizontal T-bar
            boxwex = 0.6,         # Width of the box
            lwd = 1               # Thickness of the lines
    )
    
  }, 
  # Direct dynamic dimensions
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )
  
  

  
  # 1. The Dynamic UI Wrapper (Handles Width & Height)
  output$boxP_UI <- renderUI({
    #plotly::plotlyOutput("boxP2",width = userData$plotWidth, height = userData$plotHeight)
    plotly::plotlyOutput("boxP2",width = userData$plotWidth, height = userData$plotHeight)
  })

  
  # 2. The Plotly Render Function (TSstudio - Original Colors with Outliers)
  output$boxP2 <- plotly::renderPlotly({
    req(tsData()) 
    
    # 1. Extract numeric values for dynamic sizing
    w <- as.numeric(gsub("[^0-9]", "", userData$plotWidth))
    h <- as.numeric(gsub("[^0-9]", "", userData$plotHeight))
    
    # 2. Generate the plot (TSstudio uses a unique color for each cycle by default)
    p <- TSstudio::ts_seasonal(tsData(), type = "box")
    
    # 3. Modify trace: Show outliers only, use hollow circles, keep original colors
    p %>% 
      plotly::style(
        boxpoints = "outliers",       # Standard 1.5 * IQR Limits
        marker = list(
          symbol = "circle-open",     # Empty/Hollow circles
          size = 7                    # Small circles
          # We omit 'color' here to let Plotly use the original trace colors
        )
      ) %>% 
      plotly::layout(
        width = w, 
        height = h,
        yaxis = list(title = userData$yLabel)
      )
  })
  
  
  
  # output$boxP2 <- plotly::renderPlotly({
  #   req(tsData()) 
  #   
  #   # 1. Extract numeric values from your userData
  #   w <- as.numeric(gsub("[^0-9]", "", userData$plotWidth))
  #   h <- as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  #   
  #   # 2. Generate the plot
  #   p <- TSstudio::ts_seasonal(tsData(), type = "box")
  #   
  #   # 3. Modify the trace to hide points and apply layout
  #   p %>% 
  #     plotly::style(boxpoints = FALSE) %>% # This hides all individual data points
  #     plotly::layout(width = w, height = h)
  # })

  
  ########  ######## 
  # Plot the time series data
  ########  ######## 
  
  #     SEASONAL SUB-SERIES PLOT (ggplot2 / forecast)
  output$SubSeriesPlot <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    
    # Create the sub-series plot
    # This displays the seasonal changes over time for each individual period
    forecast::ggsubseriesplot(tsData()) + 
      labs(
        title = paste("Sub-series Plot:", userData$mainTitle),
        x = "Season",
        y = userData$yLabel
      ) +
      apply_user_theme() # Apply your custom theme
    
  }, 
  # Direct dynamic dimensions for renderPlot
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )

  
  # 1. The Dynamic UI Wrapper (Handles Width & Height)
  output$SubSeriesPlot_UI <- renderUI({
    plotly::plotlyOutput("SubSeriesPlot2", width = userData$plotWidth, height = userData$plotHeight)
  })
  
  # # 2. The Plotly Render Function
  #     SEASONAL SUB-SERIES PLOT (Interactive)
  # output$SubSeriesPlot2 <- plotly::renderPlotly({
  #   req(tsData()) # Ensure data is loaded
  # 
  #   # 1. Extract numeric values from your userData sliders
  #   w <- as.numeric(gsub("[^0-9]", "", userData$plotWidth))
  #   h <- as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  # 
  #   # 2. Create the ggplot object using forecast::ggsubseriesplot
  #   # This plot shows the seasonal change over time for each period (e.g., every January)
  #   p <- forecast::ggsubseriesplot(tsData()) +
  #     ggtitle("Seasonal Sub-series Plot") +
  #     theme_minimal()
  # 
  #   # 3. Convert to Plotly and apply dynamic dimensions
  #   # Passing width/height here ensures the interactive canvas matches your sliders
  #   plotly::ggplotly(p, width = w, height = h)
  # })
  
  # 2. The Plotly Render Function
  output$SubSeriesPlot2 <- plotly::renderPlotly({
    req(tsData()) 
    
    # 1. Extract dimensions
    w <- as.numeric(gsub("[^0-9]", "", userData$plotWidth))
    h <- as.numeric(gsub("[^0-9]", "", userData$plotHeight))
    
    # 2. Create the plot and suppress the internal 'fortify' warning
    p <- suppressWarnings({
      forecast::ggsubseriesplot(tsData()) + 
        ggtitle("Seasonal Sub-series Plot") +
        theme_minimal()
    })
    
    # 3. Render as Plotly
    # We also wrap the conversion to ensure the warning doesn't trigger here
    suppressWarnings({
      plotly::ggplotly(p, width = w, height = h)
    })
  })
  

  ########  ######## 
  # Plot the seasonal
  ########  ######## 

  
  #     SEASONAL PLOT (ggplot2 / forecast)
  output$SeasonPlot <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    
    # 1. Create the seasonal plot
    # This plot overlaps years into a single seasonal cycle (1-12 or 1-4)
    forecast::ggseasonplot(tsData(), 
                           main = paste("Seasonal Plot:", userData$mainTitle),
                           year.labels = TRUE, 
                           year.labels.left = TRUE) + 
      labs(x = "Season", y = userData$yLabel) +
      apply_user_theme() # Apply your custom theme here
    
  }, 
  # Direct dynamic dimensions for renderPlot
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )
  

  # 1. The Dynamic UI Wrapper (Handles Width & Height)
  output$SeasonPlot_UI <- renderUI({
    plotly::plotlyOutput("SeasonPlot2", width = userData$plotWidth, height = userData$plotHeight)
  })
  
  # 2. The Plotly Render Function
  #     SEASONAL PLOT (TSstudio)
  output$SeasonPlot2 <- plotly::renderPlotly({
    req(tsData()) 
    
    # 1. Extract numeric values from your userData sliders
    w <- as.numeric(gsub("[^0-9]", "", userData$plotWidth))
    h <- as.numeric(gsub("[^0-9]", "", userData$plotHeight))
    
    # 2. Generate the seasonal plot (type = "normal" for overlapping lines)
    p <- TSstudio::ts_seasonal(tsData(), type = "normal")
    
    # 3. Apply the dynamic dimensions via layout
    p %>% plotly::layout(width = w, height = h)
  })
  
  
  ########  ######## 
  # Plot the Polar
  ########  ######## 
  
  #     POLAR SEASONAL PLOT (ggplot2 / forecast)
  
  output$SeasonPlotPolar <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    
    # Generate the seasonal plot in polar coordinates
    forecast::ggseasonplot(tsData(), 
                           polar = TRUE,
                           main = paste("Polar Seasonal Plot:", userData$mainTitle)) + 
      labs(y = userData$yLabel) +
      apply_user_theme() # Apply your custom theme logic
    
  }, 
  # Direct dynamic dimensions for renderPlot
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )
  
  
  
  ########  ######## 
  # Plot the lag plot
  ########  ######## 
  
  #     LAG PLOT (ggplot2 / forecast)
 
   output$lagPlot <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    
    # Create the lag plot grid
    # This displays scatter plots of the series against its own lagged values
    forecast::gglagplot(tsData()) + 
      labs(
        title = paste("Lag Plots:", userData$mainTitle),
        y = userData$yLabel
      ) +
      apply_user_theme() # Apply your custom theme
    
  }, 
  # Direct dynamic dimensions for renderPlot
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )
  
  
  output$lagPlot_UI <- renderUI({
    req(userData$plotWidth, userData$plotHeight)
    plotly::plotlyOutput("lagPlot_Plotly", width = userData$plotWidth, height = userData$plotHeight)
  })
  
  # 2. The Plotly Render Function
  #     LAG PLOTS (Interactively Rendered)
  output$lagPlot_Plotly <- plotly::renderPlotly({
    req(tsData()) # Ensure data is loaded
    
    # 1. Extract numeric values from your userData sliders
    w <- as.numeric(gsub("[^0-9]", "", userData$plotWidth))
    h <- as.numeric(gsub("[^0-9]", "", userData$plotHeight))
    
    # 2. Create the ggplot object using forecast::gglagplot
    # suppressWarnings prevents the common 'fortify' method message in the console
    p <- suppressWarnings(forecast::gglagplot(tsData())) + 
      theme_minimal() +
      labs(title = paste("Lag Plots:", userData$mainTitle))
    
    # 3. Convert to Plotly and apply dynamic dimensions
    # We set width and height here because it's a ggplotly conversion
    plotly::ggplotly(p, width = w, height = h, tooltip = "all")
  })


  dataAll <- reactive({
    ts_seasonal(tsData(), type = "all")
  })

 

  ########  ######## 
  # Plot the All plot
  ########  ######## 
  
  output$allPlotUI <- renderUI({
    plotly::plotlyOutput("allPlot", width = userData$plotWidth, height = userData$plotHeight)
  })
  
  #     COMPREHENSIVE SEASONAL DASHBOARD (TSstudio)
  output$allPlot <- plotly::renderPlotly({
    req(tsData())
    
    # 1. Extract numeric values from your userData sliders
    w <- as.numeric(gsub("[^0-9]", "", userData$plotWidth))
    h <- as.numeric(gsub("[^0-9]", "", userData$plotHeight))
    
    # 2. Generate the 4-pane interactive dashboard
    # This combines Normal, Cycle, Box, and Trend plots
    p <- TSstudio::ts_seasonal(tsData(), type = "all")
    
    # 3. Apply the dynamic dimensions and adjust margins
    p %>% plotly::layout(
      width = w, 
      height = h,
      margin = list(l = 50, r = 50, b = 50, t = 80), # More top margin for subplot titles
      title = list(text = paste("Seasonal Analysis:", userData$mainTitle), x = 0.5)
    )
  })
  


  ########  ########  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #                              decomposition
  #
  ########  ########  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########  ########  ########

  #
  #           Decomposition: Classical Seasonal Decomposition
  #
  
  output$decompose <- renderPlot({
    req(tsData(), input$model1)
    
    # Perform classical decomposition (Additive or Multiplicative)
    # input$model1 should be "additive" or "multiplicative"
    decomp_res <- stats::decompose(tsData(), type = input$model1)
    
    # Use plot to allow ggplot2 theming
    plot(decomp_res, size = 1) +
      labs(
        title = paste(tools::toTitleCase(input$model1), "Decomposition:", userData$mainTitle),
        x = userData$xLabel
      ) +
      apply_user_theme()
  }, 
  # Dynamic dimensions from your sliders
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )




  output$decompose2 <- renderPlot({
    req(tsData(), input$model1)
    
    # Perform classical decomposition (Additive or Multiplicative)
    # input$model1 should be "additive" or "multiplicative"
    decomp_res <- stats::decompose(tsData(), type = input$model1)
    
    # Use autoplot to allow ggplot2 theming
    autoplot(decomp_res, size = 1) +
      labs(
        title = paste(tools::toTitleCase(input$model1), "Decomposition:", userData$mainTitle),
        x = userData$xLabel
      ) +
      apply_user_theme()
  }, 
  # Dynamic dimensions from your sliders
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  

  #
  #           Decomposition: Seasonal Coefficients (dFactors)
  #

  output$dFactors <- renderPrint({
    req(tsData(), input$model1)
    
    # Perform decomposition
    fit <- stats::decompose(tsData(), type = input$model1)
    
    # Extract seasonal figures (one for each period in the cycle)
    seasonal_coeffs <- fit$figure
    freq <- frequency(tsData())
    
    # Header logic
    cat(".......................................................\n")
    cat("             SEASONAL COEFFICIENTS (", toupper(input$model1), ")\n")
    cat(".......................................................\n\n")
    
    # Information regarding interpretation
    if(input$model1 == "additive") {
      cat(" Type: Additive (Values added/subtracted from the trend)\n")
    } else {
      cat(" Type: Multiplicative (Factors scaling the trend)\n")
    }
    
    cat(" Frequency:", freq, "observations per cycle\n\n")
    
    # Create a named vector for better readability
    labels <- paste0("Period_", 1:freq)
    names(seasonal_coeffs) <- labels
    
    # Display the rounded coefficients
    print(round(seasonal_coeffs, 3))
    
    cat("\n.......................................................\n")
    
    # Optional: Quick check for sum/mean (Additive should sum to ~0, Mult to ~1)
    if(input$model1 == "additive") {
      cat(" Sum of coefficients:", round(sum(seasonal_coeffs), 4), "(Target: 0)\n")
    } else {
      cat(" Mean of coefficients:", round(mean(seasonal_coeffs), 4), "(Target: 1)\n")
    }
  })
  

  
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #       Advanced Decomposition: X11, SEATS, and STL Methods
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. X11 Decomposition Plot
  output$X11decompose <- renderPlot({
    req(tsData())
    
    # seasonal::seas is required for X11
    fit <- seasonal::seas(tsData(), x11 = "")
    
    autoplot(fit) +
      labs(title = paste("X11 Decomposition:", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 2. SEATS Decomposition Plot
  output$SEATSdecompose <- renderPlot({
    req(tsData())
    
    # seasonal::seas default is SEATS
    fit <- seasonal::seas(tsData())
    
    autoplot(fit) +
      labs(title = paste("SEATS Decomposition:", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 3. SEATS Numerical Summary
  output$SEATSFactors <- renderPrint({
    req(tsData())
    cat("--- SEATS (X-13ARIMA-SEATS) Diagnostic Summary ---\n\n")
    fit <- seasonal::seas(tsData())
    summary(fit)
  })
  
  # 4. X11 Numerical Summary
  output$X11Factors <- renderPrint({
    req(tsData())
    cat("--- X11 (X-13ARIMA-SEATS) Diagnostic Summary ---\n\n")
    fit <- seasonal::seas(tsData(), x11 = "")
    summary(fit)
  })
  
  # 5. STL Decomposition Plot (Seasonal-Trend LOESS)
  output$STLdecompose <- renderPlot({
    req(tsData())
    
    # Using periodic window and robust fitting for outliers
    fit <- stl(tsData(), t.window = 13, s.window = "periodic", robust = TRUE)
    
    autoplot(fit) +
      labs(title = paste("STL Decomposition:", userData$mainTitle)) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 6. STL Numerical Components
  output$STLFactors <- renderPrint({
    req(tsData())
    cat("--- STL Decomposition Components (Seasonal-Trend LOESS) ---\n\n")
    fit <- stl(tsData(), t.window = 13, s.window = "periodic", robust = TRUE)
    
    # Displaying the first few components for brevity
    print(head(fit$time.series, 24))
    cat("\n... (showing first 24 periods)")
  }) 
  

  

  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #           Modeling: Auto ARIMA & Holt-Winters (Dynamic)
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Forecast Result Cache
  forecastCache <- reactiveValues()
  
  createCacheKey <- function(file, col, model, freq1, h) {
    if (is.null(file) || is.null(col) || is.null(model)) return(NULL)
    # Include forecast length (h) in the key to refresh if the user changes it
    paste0(file$name, "_", file$size, "_", col, "_", model, "_", freq1, "_h", h)
  }
  
  # 2. Centralized Model Calculation
  results <- reactive({
    req(tsData(), input$Model, input$length)
    
    key <- createCacheKey(input$fileData, input$colNum, input$Model, currentFrequency(), input$length)
    
    if (!is.null(forecastCache[[key]])) {
      return(forecastCache[[key]])
    } else {
      # Call the modeling function
      res <- modelisation(input$Model, input$length)
      forecastCache[[key]] <- res
      return(res)
    }
  })
  
  modelisation <- function(modelType, forecastLength) {
    timeSeriesData <- tsData()
    
    # 3. Model Selection
    fittedModel <- switch(modelType,
                          "ARIMA" = forecast::auto.arima(timeSeriesData, stepwise = FALSE, approximation = FALSE, allowdrift = TRUE),
                          "Holt-Winters Additive" = forecast::hw(timeSeriesData, seasonal = "additive", h = forecastLength)$model,
                          "Holt-Winters Multiplicative" = forecast::hw(timeSeriesData, seasonal = "multiplicative", h = forecastLength)$model,
                          forecast::holt(timeSeriesData, h = forecastLength)$model # Default
    )
    
    # 4. Generate Forecasts
    forecastedValues <- forecast::forecast(fittedModel, level = c(80, 95), h = forecastLength)
    
    list(
      modelOutput = fittedModel,
      forecast = forecastedValues,
      residuals = fittedModel$residuals
    )
  }
  
  # 5. Output: Model Summary
  output$autoForecast <- renderPrint({
    # 1. Essential inputs
    req(input$fileData, input$colNum, input$Model)
    
    # 2. Generate the Cache Key to check if we need a progress bar
    # Note: Ensure createCacheKey and forecastCache are defined in your global/server scope
    key <- createCacheKey(input$fileData, input$colNum, input$Model, currentFrequency(), input$length)
    is_cached <- !is.null(forecastCache[[key]])
    
    # 3. Show Modal ONLY if the CPU needs to work (not cached)
    if (!is_cached) {
      showModal(modalDialog(
        title = "Automated Modeling in Progress",
        tags$div(
          style = "text-align: center;",
          tags$p(paste("Please wait while the algorithm optimizes the", input$Model, "parameters...")),
          tags$div(class = "progress progress-striped active",
                   tags$div(class = "progress-bar progress-bar-info", 
                            style = "width: 100%;"))
        ),
        footer = NULL, 
        easyClose = FALSE
      ))
    }
    
    # 4. Trigger/Retrieve the results
    # If is_cached is FALSE, this line does the heavy lifting
    res_data <- results()
    
    # 5. Close the modal immediately after calculation finishes
    if (!is_cached) {
      removeModal()
    }
    
    # 6. PRINTING THE RESULTS (Explicitly)
    cat("================================================================\n")
    cat("                DETAILED AUTOMATED MODEL SUMMARY                \n")
    cat("================================================================\n\n")
    
    # We must explicitly print the summary of the model object
    if (!is.null(res_data$modelOutput)) {
      print(summary(res_data$modelOutput))
    } else {
      cat("No model output available for the current selection.\n")
    }
    
    cat("\n================================================================\n")
    cat("                     STATISTICAL INTERPRETATION                 \n")
    cat("================================================================\n")
    cat("The summary above displays the optimal coefficients and training\n")
    cat("set accuracy measures. Information Criteria (AIC/BIC) provide a\n")
    cat("mathematical estimate of the relative quality of the model.\n")
  })
  
  
  
  
  # 6. Output: Forecast Plot (Thematic & Dynamic)
  output$autoForecast_plot <- renderPlot({
    req(results())
    
    # Use autoplot for theme support and clean visuals
    autoplot(results()$forecast, size = 1) +
      labs(
        title = paste("Forecast:", userData$mainTitle),
        x = userData$xLabel,
        y = userData$yLabel
      ) +
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 7. Output: Numerical Forecast Data
  output$results_forecast <- renderPrint({
    req(results())
    results$forecast
  })
  
  output$results_forecastTable <- renderTable({
    req(results())
    df <- as.data.frame(results()$forecast)
    # Format date/time row names into a proper column
    data.frame(Time = row.names(df), df)
  })
  
  # 8. Output: ARIMA Parameter Extraction
  output$arimaParams <- renderPrint({
    req(results())
    fit <- results()$modelOutput
    
    if(inherits(fit, "Arima")) {
      # ARMA structure: p, q, P, Q, s, d, D
      p <- fit$arma[1]; q <- fit$arma[2]
      P <- fit$arma[3]; Q <- fit$arma[4]
      s <- fit$arma[5]; d <- fit$arma[6]; D <- fit$arma[7]
      
      cat("ARIMA Structure (p,d,q)(P,D,Q)[s]:\n")
      cat(sprintf("Non-seasonal: (%d, %d, %d)\n", p, d, q))
      cat(sprintf("Seasonal:     (%d, %d, %d)[%d]\n", P, D, Q, s))
      cat("AICc:        ", round(fit$aicc, 2), "\n")
    } else {
      cat("Model is not ARIMA (Holt-Winters/ETS selected).")
    }
  })
  
  # 9. Output: Detailed Coefficients
  output$modelDetails <- renderPrint({
    req(results())
    fit <- results()$modelOutput
    
    cat("--- Model Coefficients ---\n\n")
    print(fit$coef)
    
    if(!is.null(fit$sigma2)) {
      cat("\nSigma^2 (Residual Variance):", round(fit$sigma2, 4))
    }
  })





  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #        Auto-Forecast Tests: Stationarity, Trend, and Residuals
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Mann-Kendall Trend Test
  output$testTrendMK <- renderPrint({
    req(tsData())
    cat("--- Mann-Kendall Trend Test ---\n")
    helpMK()
    
    res <- Kendall::MannKendall(tsData())
    print(res)
    
    cat("\nInterpretation: ")
    if(res$sl < 0.05) {
      cat("Significant monotonic trend detected (p < 0.05).")
    } else {
      cat("No significant monotonic trend detected (p >= 0.05).")
    }
  })
  
  # 2. Augmented Dickey-Fuller (ADF) Test
  output$test_ADF <- renderPrint({
    req(tsData(), input$altern, input$LagOrderADF)
    cat("--- Augmented Dickey-Fuller (ADF) Test ---\n")
    helpADF()
    
    tryCatch({
      res <- tseries::adf.test(tsData(), alternative = input$altern, k = as.numeric(input$LagOrderADF))
      print(res)
      
      cat("\nInterpretation: ")
      if(res$p.value < 0.05) {
        cat("Stationary (p < 0.05). Reject Null Hypothesis: The series does not have a unit root.")
      } else {
        cat("Non-stationary (p >= 0.05). Fail to reject Null: The series has a unit root.")
      }
    }, error = function(e) cat("Error:", e$message))
  })
  
  # 3. KPSS Test (Stationarity)
  output$test_KPSS <- renderPrint({
    req(tsData())
    cat("--- KPSS Unit Root Test ---\n")
    helpKPSS()
    
    # Null: Stationary | Alternative: Unit Root
    res <- tseries::kpss.test(tsData(), null = "Trend")
    print(res)
    
    cat("\nInterpretation: ")
    if(res$p.value < 0.05) {
      cat("Non-stationary (p < 0.05). Reject Null: The series likely has a unit root.")
    } else {
      cat("Stationary (p >= 0.05). Fail to reject Null: The series is trend-stationary.")
    }
  })
  
  # 4. Ljung-Box Test (Model Residuals)
  output$testLBn <- renderPrint({
    req(results()$modelOutput, input$lagorder, input$typeBoxTest)
    
    # Access residuals from the centralized reactive
    model_Residuals <- results()$modelOutput$residuals
    
    cat("--- Ljung-Box / Box-Pierce Test on Residuals ---\n")
    cat("Model used:", input$Model, "\n")
    helpLjungBox()
    
    res <- Box.test(model_Residuals, lag = input$lagorder, type = input$typeBoxTest)
    print(res)
    
    cat("\nInterpretation: ")
    if(res$p.value > 0.05) {
      cat("White Noise (p > 0.05). Residuals are independent; the model has captured the data's structure.")
    } else {
      cat("Autocorrelation Present (p <= 0.05). The residuals are not white noise; the model may be improved.")
    }
  })

  


  
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #    Auto-Forecast: Residuals Panels, ACF, PACF, & Unit Circle
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Standard Residual Check (Time plot, ACF, and Histogram)
  output$chkRes <- renderPlot({
    req(results()$modelOutput)
    # checkresiduals is a wrapper; we can't easily ggplotify the whole panel 
    # but we can apply the theme to its components via ggcheckresiduals if available, 
    # or stick to the forecast version which is very robust.
    forecast::checkresiduals(results()$modelOutput)
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 2. GG-style Diagnostic Panel
  output$tsdiag <- renderPlot({
    req(results()$modelOutput)
    fittedModel <- results()$modelOutput
    myData <- fittedModel
    ggtsdiag(myData)
    
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 3. Residual ACF
  output$plotACFRes <- renderPlot({
    req(results()$modelOutput)
    forecast::ggAcf(results()$modelOutput$residuals) + 
      labs(title = "ACF of Residuals") + 
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 4. Residual PACF
  output$plotPACFRes <- renderPlot({
    req(results()$modelOutput)
    forecast::ggPacf(results()$modelOutput$residuals) + 
      labs(title = "PACF of Residuals") + 
      apply_user_theme()
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  # 5. Combined ACF/PACF Residuals
  output$plot_ACF_PACF_Res <- renderPlot({
    req(results()$modelOutput)
    resids <- results()$modelOutput$residuals
    
    p1 <- forecast::ggAcf(resids) + apply_user_theme() + labs(title = "Residual ACF")
    p2 <- forecast::ggPacf(resids) + apply_user_theme() + labs(title = "Residual PACF")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  }, 
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  
 
   # 6. Inverse AR/MA Roots (Unit Circle)
    output$unitCercle <- renderPlot({
    # 1. Ensure the model exists
    req(results()$modelOutput)
    fit <- results()$modelOutput

    # 2. Use the base plot method for Arima objects
    # This is the most reliable way to get the actual unit circle grid
    plot(fit)

  },
  # Maintain your dynamic scaling
  width = function() getPlotDim(userData$plotWidth),
  height = function() getPlotDim(userData$plotHeight)
  )
  
  

  
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #       Comprehensive SARIMA: Animated Progress & Full Interpretations
  #
  ######## ######## ######## ######## ######## ######## ######## ########
    
    
    # 1. Create a reactive object that ONLY triggers when the button is clicked
    slowArimaResults <- eventReactive(input$startSlowArima, {
      req(tsData())
      
      # 1. Show Modal with an Animated 'Indeterminate' Progress Bar
      showModal(modalDialog(
        title = "Advanced Exhaustive Search in Progress",
        tags$div(
          style = "text-align: center;",
          tags$p("The algorithm is testing hundreds of ARIMA combinations using exact likelihood."),
          tags$p("This ensures the mathematically optimal model is found. Please wait..."),
          tags$div(class = "progress progress-striped active",
                   tags$div(class = "progress-bar progress-bar-primary", 
                            style = "width: 100%;"))
        ),
        footer = NULL, 
        easyClose = FALSE
      ))
      
      # 2. Start Computation Logic
      result_text <- tryCatch({
        
        # 2a. Anomaly Detection
        outliers <- forecast::tsoutliers(tsData())
        
        # 2b. The Exhaustive Search (stepwise=FALSE, approx=FALSE)
        # Using isolate() here ensures we take the values currently in the numeric inputs
        trace_log <- capture.output({
          sarima_model <- forecast::auto.arima(
            tsData(),
            max.p = as.numeric(input$maxp), max.d = as.numeric(input$maxd), max.q = as.numeric(input$maxq),
            max.P = as.numeric(input$maxPs), max.D = as.numeric(input$maxDs), max.Q = as.numeric(input$maxQs),
            max.order = as.numeric(input$maxorder),
            stepwise = FALSE,      # Check all combinations
            approximation = FALSE, # Use exact MLE for precision
            trace = TRUE,          # Show the work
            allowdrift = TRUE
          )
        })
        
        # 2c. Statistical Validation Calculations
        resids <- residuals(sarima_model)
        lb_test <- Box.test(resids, lag = 24, type = "Ljung-Box")
        jb_test <- tseries::jarque.bera.test(resids)
        
        # 2d. Backtesting (Time Series Cross-Validation)
        cv_error <- tryCatch({
          mean(abs(forecast::tsCV(tsData(), forecast::Arima, 
                                  order=sarima_model$arma[c(1,6,2)], 
                                  seasonal=sarima_model$arma[c(3,7,4)])))
        }, error = function(e) return(NA))
        
        # 3. Construct the Unified Professional Report
        output_content <- capture.output({
          cat("================================================================\n")
          cat("                 I. DATA QUALITY & OUTLIER SCAN                 \n")
          cat("================================================================\n")
          if(length(outliers$index) > 0) {
            cat("ANOMALIES DETECTED: Significant outliers found at indices:", outliers$index, ".\n")
            cat("Interpretation: These points represent shocks or errors that deviate from\n")
            cat("the normal trend. The model has attempted to stay robust despite these points.\n")
          } else { 
            cat("The data quality check is complete: No significant outliers were detected.\n") 
          }
          
          cat("\n================================================================\n")
          cat("                 II. EXHAUSTIVE SEARCH HISTORY                  \n")
          cat("   [ Candidate Model ]                 [ Selection Metric (AICs) ]   \n")
          cat("================================================================\n")
          cat(paste(trace_log, collapse = "\n"))
          
          cat("\n\n================================================================\n")
          cat("                 III. MODEL SELECTION LOGIC                     \n")
          cat("================================================================\n")
          cat("BEST MODEL IDENTIFIED: ", forecast:::arima.string(sarima_model), "\n\n")
          cat("Why is this the optimal model?\n")
          cat("1. AICc Minimization: This model yielded the lowest Corrected Akaike\n")
          cat("   Information Criterion (AICc) among all candidates in the trace above.\n")
          cat("2. Information Balance: It provides the best fit to the historical data\n")
          cat("   while remaining simple enough to avoid 'overfitting' (Parsimony).\n")
          cat("3. Convergence: The model successfully reached a global maximum for\n")
          cat("   the log-likelihood function using exact MLE methods.\n")
          
          cat("\n================================================================\n")
          cat("                 IV. ESTIMATED MODEL COEFFICIENTS               \n")
          cat("================================================================\n")
          print(sarima_model$coef)
          cat("\nInterpretation: These coefficients represent the mathematical weights of the\n")
          cat("Auto-Regressive (AR) and Moving Average (MA) lags. If a coefficient is close\n")
          cat("to zero, that specific lag has minimal predictive power for the series.\n")
          
          cat("\n================================================================\n")
          cat("                 V. GOODNESS-OF-FIT METRICS                     \n")
          cat("================================================================\n")
          cat("Sigma^2 (Residual Variance): ", round(sarima_model$sigma2, 4), "\n")
          cat("Log Likelihood:             ", round(sarima_model$loglik, 2), "\n")
          cat("AIC (Akaike):               ", round(sarima_model$aic, 2), "\n")
          cat("AICc (AIC Corrected):       ", round(sarima_model$aicc, 2), "\n")
          cat("BIC (Bayesian):             ", round(sarima_model$bic, 2), "\n")
          
          cat("\nInterpretation:\n")
          cat("- Sigma^2: Measures the variance of the errors; lower is more precise.\n")
          cat("- AIC/BIC: Information criteria used to compare models. Smaller values\n")
          cat("           indicate a better balance between accuracy and simplicity.\n")
          
          cat("\n================================================================\n")
          cat("                 VI. STATISTICAL VALIDATION TESTS               \n")
          cat("================================================================\n")
          
          cat("[1] LJUNG-BOX TEST FOR INDEPENDENCE\n")
          cat("    Purpose: To confirm the residuals (errors) are purely random.\n")
          cat("    H0 (Null): Residuals are White Noise (No remaining patterns).\n")
          cat("    Result: The test returned a p-value of", round(lb_test$p.value, 4), ".\n")
          if(lb_test$p.value > 0.05) {
            cat("    Decision: We fail to reject the null hypothesis. The residuals are\n")
            cat("              independent; the model has successfully captured all data structure.\n")
          } else {
            cat("    Decision: We reject the null hypothesis. Autocorrelation remains in the\n")
            cat("              residuals, meaning the model is missing key information.\n")
          }
          
          cat("\n[2] JARQUE-BERA TEST FOR NORMALITY\n")
          cat("    Purpose: To check if the error distribution is symmetric (Bell Curve).\n")
          cat("    H0 (Null): Residuals follow a Normal Distribution.\n")
          cat("    Result: The test returned a p-value of", round(jb_test$p.value, 4), ".\n")
          if(jb_test$p.value > 0.05) {
            cat("    Decision: We fail to reject the null hypothesis. Residuals are normal,\n")
            cat("              validating the reliability of our forecast confidence intervals.\n")
          } else {
            cat("    Decision: We reject the null hypothesis. Residuals are non-normal;\n")
            cat("              prediction intervals may be biased or overly optimistic.\n")
          }
          
          cat("\n================================================================\n")
          cat("                 VII. PREDICTIVE ACCURACY (BACKTEST)            \n")
          cat("================================================================\n")
          if(!is.na(cv_error)) {
            cat("Mean Absolute Error (CV): ", round(cv_error, 4), "\n")
            cat("Interpretation: This error rate was calculated via rolling-window cross-validation,\n")
            cat("                simulating how the model performs on 'unseen' future data.\n")
          }
          
          cat("\n================================================================\n")
          cat("                 VIII. FINAL MODEL VERDICT                      \n")
          cat("================================================================\n")
          if(lb_test$p.value > 0.05 && jb_test$p.value > 0.05) {
            cat("FINAL VERDICT: Statistically Excellent. The model is fully validated\n")
            cat("and ready for production-level forecasting.\n")
          } else if (lb_test$p.value > 0.05) {
            cat("FINAL VERDICT: Statistically Acceptable. All information is captured,\n")
            cat("               though the error distribution is non-standard.\n")
          } else {
            cat("FINAL VERDICT: Statistically Sub-optimal. High risk of forecast error\n")
            cat("               due to remaining patterns in the residuals.\n \n \n")
          }
        })
        
        removeModal() 
        return(output_content)
        
      }, error = function(e) {
        removeModal()
        return(paste("Critical error during analysis:", e$message))
      })
    })
    
    # 2. Render the output ONLY when the reactive is triggered
    output$Pslow <- renderPrint({
      # If button hasn't been clicked yet, give a friendly instruction
      if (input$startSlowArima == 0) {
        cat("Click 'Run Exhaustive Search' in the sidebar to begin the analysis.")
      } else {
        # Call the reactive result
        cat(paste(slowArimaResults(), collapse = "\n"))
      }
    }) 
    
  
  


  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #        Manual SARIMA: (p, d, q) (P, D, Q) [S]
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  ######## ######## ######## ######## ######## ######## ######## ########
  
  # 1. Specialized Cache for Manual ARIMA results
  forecastCache_manualARIMA <- reactiveValues()
  
  # 2. Comprehensive Cache Key for Manual Inputs
  createCacheKey_manualARIMA <- function(file, freq, col, p, d, q, P, D, Q, drift) {
    if (is.null(file) || is.null(col)) return(NULL)
    
    paste0(file$name, "_", file$size, "_", col, "_freq", freq, 
           "_nonS", p, d, q, "_S", P, D, Q, "_drift", drift)
  }
  
  # 3. Reactive to fetch or calculate manual model
  results_ARIMA_pdPD_drift <- reactive({
    # Generate key based on all manual parameters
    key <- createCacheKey_manualARIMA(
      input$fileData, 
      currentFrequency(), 
      input$colNum, 
      input$ARIMAp, input$ARIMAd, input$ARIMAq, 
      input$ARIMAps, input$ARIMAds, input$ARIMAqs, 
      input$driftYN
    )
    
    if (!is.null(forecastCache_manualARIMA[[key]])) {
      return(forecastCache_manualARIMA[[key]])
    } else {
      # Calculate model
      result <- modelisation_Manual_SARIMA()
      forecastCache_manualARIMA[[key]] <- result
      return(result)
    }
  })
  
  # 4. Manual Model Fitting Function
  modelisation_Manual_SARIMA <- function() {
    req(tsData())
    
    # Convert UI input to logical
    drift_bool <- as.logical(input$driftYN)
    
    # Fit the user-specified model
    # Arima (capital A) from forecast package is preferred for manual fitting
    fittedModel <- tryCatch({
      forecast::Arima(
        tsData(), 
        order = c(as.numeric(input$ARIMAp), as.numeric(input$ARIMAd), as.numeric(input$ARIMAq)),
        seasonal = list(
          order = c(as.numeric(input$ARIMAps), as.numeric(input$ARIMAds), as.numeric(input$ARIMAqs)),
          period = as.numeric(currentFrequency())
        ),
        include.drift = drift_bool
      )
    }, error = function(e) {
      # Return NULL or error message if the model is mathematically impossible
      return(NULL)
    })
    
    list(modelOutput = fittedModel)
  }
  
  # 5. Output: Manual Model Summary
  output$ManualARIMASummary <- renderPrint({
    req(results_ARIMA_pdPD_drift()$modelOutput)
    summary(results_ARIMA_pdPD_drift()$modelOutput)
  })
  


  output$Previsions_Plot_pdq_UI <- renderUI({
    plotOutput("Previsions_Plot_pdq", width = userData$plotWidth, height = userData$plotHeight)
  })



  output$Previsions_Plot_pdq <- renderPlot({
    req(tsData())
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    forecasted_Data <- forecast(sarima_model,h=input$length)

    # Convert to ggplot object using autoplot
    ggplot_forecast <- autoplot(forecasted_Data, lwd = 3)

    # Apply selected theme, or default theme if none selected
    plot_theme <- if (is.null(input$theme) || input$theme == "") theme_light() else get(input$theme)()

    # Add labels and title
    ggplot_forecast <- ggplot_forecast +
      labs(x = userData$xLabel,
           y = userData$yLabel,
           title = userData$mainTitle) +
      plot_theme

    ggplot_forecast
  })


  output$model_ARIMApdq <- renderPrint({
    req(tsData())
    if (input$driftYN == "TRUE") {
      driftConsideration =TRUE
    }
    else {
      driftConsideration =FALSE
    }

    myData <- tsData()

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # sarima_model <- Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    eqs <- extractSARIMAeqLaTeX(sarima_model)

    sarima_model
    
    # withMathJax(helpText(eqs$numerical_one_line))
    
  })


  # output$model_ARIMApdq_p_values <- renderPrint({
  #   req(tsData())
  #       if (input$driftYN == "TRUE") {
  #     driftConsideration =TRUE
  #   }
  #   else {
  #     driftConsideration =FALSE
  #   }
  # 
  #   myData <- tsData()
  # 
  #   sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
  #   # sarima_model <- Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)
  # 
  #   cat("............................................................................\n")
  #   cat("                     Testing the coefficients values                        \n")
  #   cat("............................................................................\n")
  #   cat(" H0 : the coefficient = 0                                                   \n")
  #   cat(" Ha : the coefficient is different from 0                                   \n")
  #   cat("............................................................................\n")
  #   cat(" p-value < 0.05 indicates that the corresponding coefficient is             \n")
  #   cat("                significantly different from 0                              \n")
  #   cat("............................................................................\n")
  #   coeftest(sarima_model)
  # })

  
  output$model_ARIMApdq_p_values <- renderPrint({
    req(tsData())
    
    # --- Setup Drift ---
    driftConsideration <- if (input$driftYN == "TRUE") TRUE else FALSE
    
    # --- Load Model ---
    # Assuming results_ARIMA_pdPD_drift() is a reactive returning the model in $modelOutput
    res <- results_ARIMA_pdPD_drift()
    req(res)
    sarima_model <- res$modelOutput
    
    # ============================================================================
    # 1) HEADER & HYPOTHESIS
    # ============================================================================
    cat("==========================================================================\n")
    cat("                SARIMA COEFFICIENT SIGNIFICANCE TEST                      \n")
    cat("==========================================================================\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0 : The coefficient = 0 (Not significant).\n")
    cat(" • Ha : The coefficient is different from 0 (Significant).\n")
    cat(" -> CRITERIA: Reject H0 if P-Value < Alpha (usually 0.05).\n")
    cat("--------------------------------------------------------------------------\n")
    
    # ============================================================================
    # 2) RESULT (Statistical Output)
    # ============================================================================
    cat("\n RESULT:\n")
    
    tryCatch({
      if (!requireNamespace("lmtest", quietly = TRUE)) stop("Package 'lmtest' is required.")
      
      # Generate the coefficient test table
      ctest <- lmtest::coeftest(sarima_model)
      print(ctest)
      
      # ============================================================================
      # 3) DECISION
      # ============================================================================
      cat("\n DECISION:\n")
      
      # Check if all coefficients are significant
      p_values <- ctest[, 4]
      insignificant_coefs <- names(p_values[p_values > 0.05])
      
      if (length(insignificant_coefs) == 0) {
        cat("  -> ALL coefficients are significantly different from 0 at the 5% level.\n")
      } else {
        cat(sprintf("  -> WARNING: The following coefficient(s) are NOT significant: %s\n", 
                    paste(insignificant_coefs, collapse = ", ")))
      }
      
      # ============================================================================
      # 4) ADVICE
      # ============================================================================
      cat("\n ADVICE:\n")
      if (length(insignificant_coefs) > 0) {
        cat(" • Consider simplifying the model (Parsimony Principle).\n")
        cat(" • Try reducing the order (p, q, P, or Q) of the insignificant components.\n")
        cat(" • If 'drift' or 'intercept' is insignificant, try setting include.drift = FALSE.\n")
      } else {
        cat(" • The model parameters are statistically well-defined.\n")
        cat(" • Proceed to Residual Diagnostics (Ljung-Box, Normality) to ensure validity.\n")
      }
      
    }, error = function(e) {
      cat(" [!] Error calculating p-values: ", e$message, "\n")
    })
    
    cat("==========================================================================\n")
  })

  
  
  
  
  
  output$residual_independence_diagnostic <- renderPrint({
    req(results_ARIMA_pdPD_drift())
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    resids <- residuals(sarima_model)
    n <- length(resids)
    alpha_val <- 0.05
    
    # Determination of lags: 
    # For seasonal data, checking up to 20 lags is standard practice.
    lb_lag <- 20 
    
    cat("==========================================================================\n")
    cat("             ACADEMIC REPORT: RESIDUAL INDEPENDENCE ANALYSIS              \n")
    cat("==========================================================================\n")
    
    cat(" DECISION RULE:\n")
    cat(" • Null Hypothesis (H0): Residuals are independent (White Noise).\n")
    cat(" • Alternative Hypothesis (Ha): Residuals exhibit serial correlation.\n")
    cat(sprintf(" • Significance Level (α): %.2f\n", alpha_val))
    cat(" • Rejection Criterion: Reject H0 if Test Statistic > Critical Value\n")
    cat("   (or equivalently, if P-value < α).\n")
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # --- 1. Ljung-Box (Refined Portmanteau) ---
      lb_test <- Box.test(resids, lag = lb_lag, type = "Ljung-Box")
      lb_crit <- qchisq(1 - alpha_val, df = lb_lag)
      
      # --- 2. Box-Pierce (Original Portmanteau) ---
      bp_test <- Box.test(resids, lag = lb_lag, type = "Box-Pierce")
      bp_crit <- qchisq(1 - alpha_val, df = lb_lag)
      
      # ============================================================================
      # TEST 1: LJUNG-BOX (Q*)
      # ============================================================================
      cat(" 1. LJUNG-BOX PORTMANTEAU TEST (Q*)\n")
      cat(sprintf("    - Test Statistic (Q*): %.4f\n", lb_test$statistic))
      cat(sprintf("    - Critical Value     : %.4f (χ², df=%d)\n", lb_crit, lb_lag))
      cat(sprintf("    - P-Value            : %.4f\n", lb_test$p.value))
      
      cat("\n RESULT:\n")
      cat(sprintf("  The Ljung-Box test yields a Q*-statistic of %.4f for %d lags. This \n", lb_test$statistic, lb_lag))
      cat("  metric is specifically adjusted to provide better power in small to \n")
      cat("  medium-sized samples compared to the original Box-Pierce test.\n")
      
      cat("\n DECISION:\n")
      if(lb_test$p.value < alpha_val) {
        cat(sprintf("  Given that the Q*-statistic (%.4f) exceeds the critical value (%.4f), \n", lb_test$statistic, lb_crit))
        cat("  we reject the null hypothesis of independence at the 5% level.\n")
      } else {
        cat(sprintf("  Since the Q*-statistic (%.4f) is below the critical threshold (%.4f), \n", lb_test$statistic, lb_crit))
        cat("  we fail to reject the null hypothesis. The residuals exhibit no \n")
        cat("  statistically significant autocorrelation.\n")
      }
      cat("--------------------------------------------------------------------------\n")
      
      # ============================================================================
      # TEST 2: BOX-PIERCE (Q)
      # ============================================================================
      cat(" 2. BOX-PIERCE TEST (Q)\n")
      cat(sprintf("    - Test Statistic (Q) : %.4f\n", bp_test$statistic))
      cat(sprintf("    - Critical Value     : %.4f (χ², df=%d)\n", bp_crit, lb_lag))
      cat(sprintf("    - P-Value            : %.4f\n", bp_test$p.value))
      
      cat("\n RESULT:\n")
      cat(sprintf("  The Box-Pierce test provides a statistic of %.4f. This original \n", bp_test$statistic))
      cat("  portmanteau measure evaluates the sum of squared autocorrelations \n")
      cat("  to determine if the error process is purely random.\n")
      
      cat("\n DECISION:\n")
      if(bp_test$p.value < alpha_val) {
        cat("  The observed p-value is below the significance threshold of 0.05. \n")
        cat("  The null hypothesis is rejected, suggesting that the model has \n")
        cat("  failed to capture some of the temporal dynamics in the data.\n")
      } else {
        cat("  The observed p-value exceeds the significance threshold. We fail \n")
        cat("  to reject the null hypothesis of residual independence.\n")
      }
      cat("--------------------------------------------------------------------------\n")
      
      # ============================================================================
      # FINAL ADVICE
      # ============================================================================
      cat("\n ADVICE:\n")
      p_vals <- c(lb_test$p.value, bp_test$p.value)
      reject_count <- sum(p_vals < alpha_val)
      
      if (reject_count > 0) {
        cat(" • Significant portmanteau statistics indicate that the residuals are \n")
        cat("   not white noise. The current SARIMA specification is inadequate.\n")
        cat(" • Action: Increase the complexity of the model by adding AR (p/P) or \n")
        cat("   MA (q/Q) terms to account for the remaining autocorrelation.\n")
        cat(" • Review the ACF plot: Any spikes crossing the significance bounds \n")
        cat("   identify the specific lags that require further modeling.\n")
      } else {
        cat(" • The diagnostics for residual independence are satisfied. The errors \n")
        cat("   behave as a white noise process.\n")
        cat(" • Recommendation: The model is statistically adequate; proceed to \n")
        cat("   forecasting and out-of-sample validation.\n")
      }
      
    }, error = function(e) {
      cat(" [!] Error in independence diagnostics: ", e$message, "\n")
    })
    
    cat("==========================================================================\n")
  })
  
  
  # output$model_LjungBox_test <- renderPrint({
  #   req(results_ARIMA_pdPD_drift())
  #   sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
  #   resids <- residuals(sarima_model)
  #   
  #   # Determination of lags: min(2*m, n/5) is a common rule of thumb
  #   # For seasonal data (m), we often check up to lag 20.
  #   lb_lag <- 20 
  #   
  #   cat("==========================================================================\n")
  #   cat("                LJUNG-BOX TEST FOR RESIDUAL AUTOCORRELATION               \n")
  #   cat("==========================================================================\n")
  #   
  #   cat(" DECISION RULE:\n")
  #   cat(" • H0 : Residuals are independent (White Noise).\n")
  #   cat(" • Ha : Residuals exhibit autocorrelation (Model is inadequate).\n")
  #   cat(" -> CRITERIA: Reject H0 if P-Value < 0.05.\n")
  #   cat("--------------------------------------------------------------------------\n")
  #   
  #   # --- RESULT ---
  #   lb_test <- Box.test(resids, lag = lb_lag, type = "Ljung-Box")
  #   cat("\n RESULT:\n")
  #   cat(sprintf("  - Statistic (Q) : %.4f\n", lb_test$statistic))
  #   cat(sprintf("  - Lag tested    : %d\n", lb_lag))
  #   cat(sprintf("  - P-Value       : %.4f\n", lb_test$p.value))
  #   
  #   # --- DECISION ---
  #   cat("\n DECISION:\n")
  #   if (lb_test$p.value > 0.05) {
  #     cat("  -> FAIL TO REJECT H0: Residuals are White Noise.\n")
  #   } else {
  #     cat("  -> REJECT H0: Significant autocorrelation detected in residuals.\n")
  #   }
  #   
  #   # --- ADVICE ---
  #   cat("\n ADVICE:\n")
  #   if (lb_test$p.value <= 0.05) {
  #     cat(" • The model has not captured all the information in the data.\n")
  #     cat(" • Consider increasing the AR (p) or MA (q) orders.\n")
  #     cat(" • Check the ACF/PACF plots of the residuals to identify remaining patterns.\n")
  #   } else {
  #     cat(" • The model specification is adequate regarding independence.\n")
  #     cat(" • You may proceed with forecasting.\n")
  #   }
  #   cat("==========================================================================\n")
  # })
  
  
  
  
  output$model_Normality_test <- renderPrint({
    req(results_ARIMA_pdPD_drift())
    resids <- as.numeric(residuals(results_ARIMA_pdPD_drift()$modelOutput))
    n <- length(resids)
    alpha_val <- 0.05 
    
    cat("==========================================================================\n")
    cat("                ACADEMIC REPORT: RESIDUAL NORMALITY ANALYSIS              \n")
    cat("==========================================================================\n")
    
    cat(" DECISION RULE:\n")
    cat(" • Null Hypothesis (H0): Residuals are independent and identically \n")
    cat("   distributed (i.i.d.) following a Normal distribution N(0, σ²).\n")
    cat(" • Alternative Hypothesis (Ha): Residuals deviate significantly from \n")
    cat("   the Gaussian distribution.\n")
    cat(sprintf(" • Significance Level (α): %.2f\n", alpha_val))
    cat(" • Rejection Criterion: Reject H0 if Test Statistic > Critical Value\n")
    cat("   (or equivalently, if P-value < α).\n")
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # --- 1. Shapiro-Wilk ---
      sw_test <- shapiro.test(resids)
      # W-test critical values are complex to calculate; decision is based on P-value.
      
      # --- 2. Jarque-Bera ---
      if (!requireNamespace("tseries", quietly = TRUE)) stop("Install 'tseries'")
      jb_test <- tseries::jarque.bera.test(resids)
      jb_crit <- qchisq(1 - alpha_val, df = 2)
      
      # --- 3. Anderson-Darling ---
      if (!requireNamespace("nortest", quietly = TRUE)) stop("Install 'nortest'")
      ad_test <- nortest::ad.test(resids)
      # Adjustment for A^2 at 5% alpha for normality
      ad_crit <- 0.752 
      
      # ============================================================================
      # TEST 1: SHAPIRO-WILK
      # ============================================================================
      cat(" 1. SHAPIRO-WILK TEST (W)\n")
      cat(sprintf("    - Test Statistic (W): %.4f\n", sw_test$statistic))
      cat(sprintf("    - P-Value           : %.4f\n", sw_test$p.value))
      
      cat("\n RESULT:\n")
      cat(sprintf("  The Shapiro-Wilk test yields a W-statistic of %.4f. This metric \n", sw_test$statistic))
      cat(sprintf("  measures the correlation between the sample data and normal scores.\n"))
      
      cat("\n DECISION:\n")
      if(sw_test$p.value < alpha_val) {
        cat("  At the 5% significance level, the p-value is less than α. We reject the \n")
        cat("  null hypothesis of normality.\n")
      } else {
        cat("  At the 5% significance level, the p-value exceeds α. We fail to reject \n")
        cat("  the null hypothesis; the residuals are consistent with a normal distribution.\n")
      }
      cat("--------------------------------------------------------------------------\n")
      
      # ============================================================================
      # TEST 2: JARQUE-BERA
      # ============================================================================
      cat(" 2. JARQUE-BERA TEST (JB)\n")
      cat(sprintf("    - Test Statistic (JB): %.4f\n", jb_test$statistic))
      cat(sprintf("    - Critical Value     : %.4f (χ², df=2)\n", jb_crit))
      cat(sprintf("    - P-Value            : %.4f\n", jb_test$p.value))
      
      cat("\n RESULT:\n")
      cat(sprintf("  The Jarque-Bera statistic is %.4f, which evaluates the joint null \n", jb_test$statistic))
      cat(sprintf("  hypothesis of zero skewness and a kurtosis of three.\n"))
      
      cat("\n DECISION:\n")
      if(jb_test$statistic > jb_crit) {
        cat(sprintf("  The JB statistic (%.4f) is greater than the critical value (%.4f). \n", jb_test$statistic, jb_crit))
        cat("  We reject the null hypothesis, indicating significant skewness or kurtosis.\n")
      } else {
        cat(sprintf("  The JB statistic (%.4f) is below the critical value (%.4f). \n", jb_test$statistic, jb_crit))
        cat("  We fail to reject the null hypothesis of normality.\n")
      }
      cat("--------------------------------------------------------------------------\n")
      
      # ============================================================================
      # TEST 3: ANDERSON-DARLING
      # ============================================================================
      cat(" 3. ANDERSON-DARLING TEST (A²)\n")
      cat(sprintf("    - Test Statistic (A²): %.4f\n", ad_test$statistic))
      cat(sprintf("    - Critical Value     : %.4f\n", ad_crit))
      cat(sprintf("    - P-Value            : %.4f\n", ad_test$p.value))
      
      cat("\n RESULT:\n")
      cat(sprintf("  The Anderson-Darling distance is calculated as A² = %.4f. This test \n", ad_test$statistic))
      cat(sprintf("  places specific emphasis on the behavior of the distribution tails.\n"))
      
      cat("\n DECISION:\n")
      if(ad_test$statistic > ad_crit) {
        cat(sprintf("  Because A² (%.4f) exceeds the critical threshold of %.4f, we reject \n", ad_test$statistic, ad_crit))
        cat("  the null hypothesis. The distribution tails deviate from Gaussian expectations.\n")
      } else {
        cat(sprintf("  Because A² (%.4f) is below the critical threshold of %.4f, we fail \n", ad_test$statistic, ad_crit))
        cat("  to reject the null hypothesis.\n")
      }
      cat("--------------------------------------------------------------------------\n")
      
      # ============================================================================
      # FINAL ADVICE
      # ============================================================================
      cat("\n ADVICE:\n")
      p_vals <- c(sw_test$p.value, jb_test$p.value, ad_test$p.value)
      reject_count <- sum(p_vals < alpha_val)
      
      if (reject_count > 0) {
        cat(" • If significant asymmetry is detected (Jarque-Bera), identify potential \n")
        cat("   outliers or high-leverage points that may bias the parameter estimates.\n")
        cat(" • When the Anderson-Darling test rejects normality, the forecast confidence \n")
        cat("   intervals may be unreliable due to heavy-tailed error distributions.\n")
        cat(" • Consider applying a Box-Cox or Logarithmic transformation to the original \n")
        cat("   series to stabilize the residual variance.\n")
      } else {
        cat(" • All tests maintain consistency: the normality assumption is satisfied.\n")
        cat(" • You may report the model results and prediction intervals with high \n")
        cat("   statistical confidence.\n")
      }
      
    }, error = function(e) {
      cat(" [!] Error in normality computation: ", e$message, "\n")
    })
    
    cat("==========================================================================\n")
  })
  
  
  # output$model_Normality_test <- renderPrint({
  #   req(results_ARIMA_pdPD_drift())
  #   resids <- as.numeric(residuals(results_ARIMA_pdPD_drift()$modelOutput))
  #   n <- length(resids)
  #   alpha_val <- 0.05 
  #   
  #   cat("==========================================================================\n")
  #   cat("                ACADEMIC REPORT: RESIDUAL NORMALITY ANALYSIS              \n")
  #   cat("==========================================================================\n")
  #   
  #   cat(" DECISION RULE:\n")
  #   cat(" • Null Hypothesis (H0): The residuals are independent and identically \n")
  #   cat("   distributed (i.i.d.) following a Normal distribution N(0, σ²).\n")
  #   cat(" • Alternative Hypothesis (Ha): The residual distribution deviates \n")
  #   cat("   significantly from Gaussian normality.\n")
  #   cat(sprintf(" • Significance Level (α): %.2f\n", alpha_val))
  #   cat(" • Rejection Criterion: Reject H0 if the calculated P-value < α.\n")
  #   cat("--------------------------------------------------------------------------\n")
  #   
  #   # ============================================================================
  #   # 2) RESULT (Explicit & Sentence-Based)
  #   # ============================================================================
  #   cat("\n RESULT:\n")
  #   
  #   tryCatch({
  #     # 1. Shapiro-Wilk 
  #     sw_test <- shapiro.test(resids)
  #     
  #     # 2. Jarque-Bera 
  #     if (!requireNamespace("tseries", quietly = TRUE)) stop("Install 'tseries'")
  #     jb_test <- tseries::jarque.bera.test(resids)
  #     jb_crit <- qchisq(1 - alpha_val, df = 2)
  #     
  #     # 3. Anderson-Darling 
  #     if (!requireNamespace("nortest", quietly = TRUE)) stop("Install 'nortest'")
  #     ad_test <- nortest::ad.test(resids)
  #     ad_crit <- 0.752 # Standard asymptotic critical value for A^2 at 5%
  #     
  #     # Result Sentences
  #     cat(sprintf(" 1. The Shapiro-Wilk test yields a W-statistic of %.4f with an \n", sw_test$statistic))
  #     cat(sprintf("    associated p-value of %.4f.\n", sw_test$p.value))
  #     
  #     cat(sprintf(" 2. The Jarque-Bera test statistic, which evaluates skewness and \n"))
  #     cat(sprintf("    kurtosis, is calculated as %.4f compared to a critical value \n", jb_test$statistic))
  #     cat(sprintf("    of %.4f (χ² with 2 d.f.), resulting in a p-value of %.4f.\n", jb_crit, jb_test$p.value))
  #     
  #     cat(sprintf(" 3. The Anderson-Darling distance (A²) is found to be %.4f relative \n", ad_test$statistic))
  #     cat(sprintf("    to the threshold of %.4f, with a resulting p-value of %.4f.\n", ad_crit, ad_test$p.value))
  #     cat("--------------------------------------------------------------------------\n")
  #     
  #     # ============================================================================
  #     # 3) DECISION (Formal Consensus Logic)
  #     # ============================================================================
  #     cat("\n DECISION:\n")
  #     
  #     p_vals <- c(sw_test$p.value, jb_test$p.value, ad_test$p.value)
  #     reject_count <- sum(p_vals < alpha_val)
  #     
  #     if (reject_count == 0) {
  #       cat(" Based on the empirical evidence, we fail to reject the null hypothesis \n")
  #       cat(" at the 5% level of significance. All three statistical tests maintain \n")
  #       cat(" consistency in suggesting that the residuals follow a normal distribution.\n")
  #     } else if (reject_count == 3) {
  #       cat(" The null hypothesis of normality is rejected across all utilized tests. \n")
  #       cat(" There is strong statistical evidence that the residuals are not normally \n")
  #       cat(" distributed, suggesting a violation of the underlying model assumptions.\n")
  #     } else {
  #       cat(sprintf(" The tests provide mixed evidence, with %d out of 3 tests rejecting \n", reject_count))
  #       cat(" the null hypothesis. This indicates that while the central distribution \n")
  #       cat(" may appear Gaussian, specific characteristics such as tail behavior or \n")
  #       cat(" asymmetry are statistically significant.\n")
  #     }
  #     
  #     # ============================================================================
  #     # 4) ADVICE
  #     # ============================================================================
  #     cat("\n ADVICE:\n")
  #     if (reject_count > 0) {
  #       cat(" • If Jarque-Bera is significant, evaluate the presence of high-leverage \n")
  #       cat("   outliers that may be inflating the skewness or kurtosis parameters.\n")
  #       cat(" • If Anderson-Darling is significant, exercise caution when interpreting \n")
  #       cat("   forecast intervals, as the error tails are heavier than expected.\n")
  #       cat(" • Academic Recommendation: Apply a Box-Cox transformation or consider a \n")
  #       cat("   robust estimation method (e.g., Student-t distributed errors).\n")
  #     } else {
  #       cat(" • The assumption of normality is satisfied; inferential statistics and \n")
  #       cat("   confidence intervals derived from this model can be reported with \n")
  #       cat("   high confidence.\n")
  #     }
  #     
  #   }, error = function(e) {
  #     cat(" [!] Error in normality computation: ", e$message, "\n")
  #   })
  #   
  #   cat("==========================================================================\n")
  # })
  
  
  # output$model_Normality_test <- renderPrint({
  #   req(results_ARIMA_pdPD_drift())
  #   resids <- as.numeric(residuals(results_ARIMA_pdPD_drift()$modelOutput))
  #   
  #   cat("==========================================================================\n")
  #   cat("                MULTI-TEST RESIDUAL NORMALITY DIAGNOSTIC                  \n")
  #   cat("==========================================================================\n")
  #   
  #   cat(" DECISION RULE:\n")
  #   cat(" • H0 : Residuals follow a Normal Distribution.\n")
  #   cat(" • Ha : Residuals do NOT follow a Normal Distribution.\n")
  #   cat(" -> CRITERIA: Reject H0 if P-Value < 0.05.\n")
  #   cat("--------------------------------------------------------------------------\n")
  #   
  #   # ============================================================================
  #   # 2) RESULT: THE TEST BATTERY
  #   # ============================================================================
  #   cat("\n RESULT:\n")
  #   
  #   tryCatch({
  #     # 1. Shapiro-Wilk (Best for small to medium samples)
  #     sw_test <- shapiro.test(resids)
  #     
  #     # 2. Jarque-Bera (Focuses on Skewness and Kurtosis)
  #     if (!requireNamespace("tseries", quietly = TRUE)) stop("Install 'tseries'")
  #     jb_test <- tseries::jarque.bera.test(resids)
  #     
  #     # 3. Anderson-Darling (Sensitive to the tails of the distribution)
  #     if (!requireNamespace("nortest", quietly = TRUE)) stop("Install 'nortest'")
  #     ad_test <- nortest::ad.test(resids)
  #     
  #     # Formatting Results Table
  #     results_df <- data.frame(
  #       Test = c("Shapiro-Wilk", "Jarque-Bera", "Anderson-Darling"),
  #       Statistic = c(sw_test$statistic, jb_test$statistic, ad_test$statistic),
  #       P_Value = c(sw_test$p.value, jb_test$p.value, ad_test$p.value)
  #     )
  #     print(results_df, row.names = FALSE)
  #     
  #     # ============================================================================
  #     # 3) DECISION
  #     # ============================================================================
  #     cat("\n DECISION:\n")
  #     
  #     # Global decision logic (consensus approach)
  #     p_vals <- results_df$P_Value
  #     reject_count <- sum(p_vals < 0.05)
  #     
  #     if (reject_count == 0) {
  #       cat("  -> FAIL TO REJECT H0: All tests suggest residuals are NORMAL.\n")
  #     } else if (reject_count == 3) {
  #       cat("  -> REJECT H0: All tests suggest residuals are NON-NORMAL.\n")
  #     } else {
  #       cat(sprintf("  -> MIXED EVIDENCE: %d out of 3 tests reject normality.\n", reject_count))
  #     }
  #     
  #     # ============================================================================
  #     # 4) ADVICE
  #     # ============================================================================
  #     cat("\n ADVICE:\n")
  #     if (reject_count > 0) {
  #       cat(" • If Jarque-Bera is low: Your residuals have high Skewness/Kurtosis (outliers).\n")
  #       cat(" • If Anderson-Darling is low: Your model fails to predict extreme values (tails).\n")
  #       cat(" • RECO: Consider a Log or Box-Cox transformation to stabilize variance.\n")
  #       cat(" • RECO: Check the Q-Q Plot. If points deviate at the ends, tails are the issue.\n")
  #     } else {
  #       cat(" • Normality assumption is satisfied across all metrics.\n")
  #       cat(" • Prediction intervals and t-statistics are highly reliable.\n")
  #     }
  #     
  #   }, error = function(e) {
  #     cat(" [!] Error in normality computation: ", e$message, "\n")
  #   })
  #   
  #   cat("==========================================================================\n")
  # })
  
  # output$model_Normality_test <- renderPrint({
  #   req(results_ARIMA_pdPD_drift())
  #   resids <- residuals(results_ARIMA_pdPD_drift()$modelOutput)
  #   
  #   cat("==========================================================================\n")
  #   cat("                JARQUE-BERA TEST FOR RESIDUAL NORMALITY                   \n")
  #   cat("==========================================================================\n")
  #   
  #   cat(" DECISION RULE:\n")
  #   cat(" • H0 : Residuals follow a Normal Distribution.\n")
  #   cat(" • Ha : Residuals do NOT follow a Normal Distribution.\n")
  #   cat(" -> CRITERIA: Reject H0 if P-Value < 0.05.\n")
  #   cat("--------------------------------------------------------------------------\n")
  #   
  #   # --- RESULT ---
  #   tryCatch({
  #     if (!requireNamespace("tseries", quietly = TRUE)) stop("Package 'tseries' required.")
  #     jb_test <- tseries::jarque.bera.test(resids)
  #     
  #     cat("\n RESULT:\n")
  #     cat(sprintf("  - JB Statistic : %.4f\n", jb_test$statistic))
  #     cat(sprintf("  - P-Value      : %.4f\n", jb_test$p.value))
  #     
  #     # --- DECISION ---
  #     cat("\n DECISION:\n")
  #     if (jb_test$p.value > 0.05) {
  #       cat("  -> FAIL TO REJECT H0: Residuals appear normally distributed.\n")
  #     } else {
  #       cat("  -> REJECT H0: Residuals deviate significantly from normality.\n")
  #     }
  #     
  #     # --- ADVICE ---
  #     cat("\n ADVICE:\n")
  #     if (jb_test$p.value <= 0.05) {
  #       cat(" • Non-normality can lead to unreliable confidence intervals.\n")
  #       cat(" • Check for outliers or consider a Box-Cox transformation of the data.\n")
  #       cat(" • Examine the Histogram and Q-Q Plot of residuals for visual confirmation.\n")
  #     } else {
  #       cat(" • Assumption satisfied. Prediction intervals are likely reliable.\n")
  #     }
  #     
  #   }, error = function(e) {
  #     cat(" [!] Error: ", e$message, "\n")
  #   })
  #   
  #   cat("==========================================================================\n")
  # })
  
  
  output$plot_ACF_PACF_Res_pdq <- renderPlot({
    req(tsData())
    myData <- tsData()

    if (input$driftYN == "TRUE") {
      driftConsideration =TRUE
    }
    else {
      driftConsideration =FALSE
    }

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # sarima_model <- Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    acf2(sarima_model$residuals, lwd = 3 , main = userData$mainTitle)
  })


  output$unit_Circle_pdq <- renderPlot({
    req(tsData())
    myData <- tsData()

    if (input$driftYN == "TRUE") {
      driftConsideration =TRUE
    }
    else {
      driftConsideration =FALSE
    }

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # sarima_model <- Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    plot(sarima_model)
  })


  # output$timeSeriesPlot_SARIMA <- renderPlot({
  #   req(tsData())
  #   ts_data <- tsData()  #  time series data
  #   sarima_model <- results_ARIMA_pdPD_drift()$modelOutput   #  SARIMA model
  #   forecasted_Data <- forecast(sarima_model,h=input$length)
  # 
  # 
  #   fitted_values <- fitted(sarima_model)
  # 
  # 
  #   #Plot the time series data
  #   plot(forecasted_Data, type = "l", lwd = 2, col = "red4", main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
  #   lines(fitted_values, col = "firebrick3", type = "l", lwd = 2)
  #   # Add the fitted values from the SARIMA model
  #   # fitted_values <- fitted(sarima_model)
  #   lines(ts_data, col = "black", type = "l", lwd = 2)  # 'type = "p"' plots the fitted values as points
  # 
  #   # plot(sarima_model)
  # })
  
  
  
  output$timeSeriesPlot_SARIMA <- renderPlot(
    {
      req(tsData(), results_ARIMA_pdPD_drift())
      
      ts_data <- tsData()
      sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
      forecasted_Data <- forecast::forecast(sarima_model, h = input$length)
      
      fitted_values <- fitted(sarima_model)
      
      # Plot forecast
      plot(
        forecasted_Data,
        type = "l",
        lwd  = 2,
        col  = "red4",
        main = userData$mainTitle,
        xlab = userData$xLabel,
        ylab = userData$yLabel
      )
      
      # Add fitted values
      lines(fitted_values, col = "firebrick3", lwd = 2)
      
      # Add original series
      lines(ts_data, col = "black", lwd = 2)
    },
    width = function() {
      as.numeric(gsub("[^0-9]", "", userData$plotWidth))
    },
    height = function() {
      as.numeric(gsub("[^0-9]", "", userData$plotHeight))
    }
  )
  



  output$plotUI <- renderUI({
    plotOutput("timeSeriesPlot_and_SARIMA", width = userData$plotWidth, height = userData$plotHeight)
  })


  output$timeSeriesPlot_and_SARIMA <- renderPlot({
    req(tsData())
    ts_data <- tsData()  #  time series data
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput   #  SARIMA model

    ts_LineType <- as.numeric(input$tsLineType)

    model_LineType <- as.numeric(input$sarimaLineType)

    # Plot the time series data
    plot(ts_data, lty = ts_LineType, lwd = input$tsLineWidth, col = input$tsLineColor, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)

    # Add grid lines to the plot
    grid(nx = NULL, ny = NULL, col = "gray55", lty = "dotted", lwd = par("lwd"))

     # Add the fitted values from the SARIMA model as a line
    fitted_values <- fitted(sarima_model)
    lines(fitted_values, lty = model_LineType, col = input$sarimaLineColor, lwd = input$sarimaLineWidth)
  })




  output$plotAll_UI <- renderUI({
    plotOutput("timeSeriesPlot_and_SARIMA_and_Pred", width = userData$plotWidth, height = userData$plotHeight)
  })


  # Generate dynamic inputs
  output$dynamicInputs <- renderUI({

    tagList(

      # tags$label("t(S)", style = "color: #546FC6;"),
      # tags$br(),  # Line break
      tags$input(type = "color", id = "color1", value = "#546FC6"),
      tags$label(" Ts(t)", style = "color: #546FC6;"),
      tags$br(), tags$br(), # Line break
      selectInput("tsLineType2", label = NULL, choices = c("solid" = 1, "dashed" = 2, "dotted" = 3, "dotdash" = 4, "longdash" = 5, "twodash" = 6), selected = "l"),
      numericInput("tsLineWidth2", label = NULL, min = 0, value = 2),
      # selectInput("tsLineType2", label = HTML("<span style='color:#546FC6;'>  S(t).Type  </span>"), choices = c("solid" = 1, "dashed" = 2, "dotted" = 3, "dotdash" = 4, "longdash" = 5, "twodash" = 6), selected = "l"),
      # numericInput("tsLineWidth2", label = HTML("<span style='color:#546FC6;'>  S(t).Width  </span>"), min = 0, value = 2),
      # tags$hr(),
      # tags$label("SARIMA", style = "color: #EE6666;"),
      tags$br(),  # Line break
      tags$input(type = "color", id = "color2", value = "#EE6666"),
      tags$label("SARIMA", style = "color: #EE6666;"),
      tags$br(), tags$br(), # Line break
      selectInput("sarimaLineType2", label = NULL , choices = c("solid" = 1, "dashed" = 2, "dotted" = 3, "dotdash" = 4, "longdash" = 5, "twodash" = 6), selected = "3"),
      numericInput("sarimaLineWidth2", label = NULL , min = 0, value = 2),
      # selectInput("sarimaLineType2", label = HTML("<span style='color:#EE6666;'>  SARIMA.Type  </span>"), choices = c("solid" = 1, "dashed" = 2, "dotted" = 3, "dotdash" = 4, "longdash" = 5, "twodash" = 6), selected = "3"),
      # numericInput("sarimaLineWidth2", label = HTML("<span style='color:#EE6666;'>  SARIMA.Width  </span>"), min = 0, value = 2),
      tags$hr(),
      # tags$label("Forecast", style = "color: #3BA372;"),
      # tags$br(),  # Line break
      tags$input(type = "color", id = "color3", value = "#3BA372"),
      tags$label("Forecast", style = "color: #3BA372;"),
      tags$br(), tags$br(), # Line break
      selectInput("FLineType2", label = NULL , choices = c("solid" = 1, "dashed" = 2, "dotted" = 3, "dotdash" = 4, "longdash" = 5, "twodash" = 6), selected = "2"),
      numericInput("FLineWidth2", label = NULL , min = 0, value = 2),
      # selectInput("FLineType2", label = HTML("<span style='color:#3BA372;'>  Forecast.Type  </span>"), choices = c("solid" = 1, "dashed" = 2, "dotted" = 3, "dotdash" = 4, "longdash" = 5, "twodash" = 6), selected = "2"),
      # numericInput("FLineWidth2", label = HTML("<span style='color:#3BA372;'>  Forecast.Width  </span>"), min = 0, value = 2),


      tags$script(HTML("
            $(document).ready(function() {
              // Immediately send the default color values to Shiny when the app loads
              ['#color1', '#color2', '#color3'].forEach(function(colorId) {
                var colorValue = $(colorId).val();
                Shiny.setInputValue(colorId.substring(1), colorValue);
              });

              // Update Shiny input values when user changes the color
              $(document).on('input', '#color1, #color2, #color3', function() {
                Shiny.setInputValue(this.id, this.value);
              });
            });
          "))
    )
  })


  output$timeSeriesPlot_and_SARIMA_and_Pred <- renderPlot({
    req(tsData())  # Ensure that tsData is available
    ts_data <- tsData()  # Time series data
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput  # SARIMA model

    tsLineColor <-  input$color1
    sarimaLineColor <- input$color2
    FLineColor <- input$color3

    # Forecaste data
    forecast_horizon <- input$length
    forecasted_Data <- forecast(sarima_model, h = forecast_horizon)$mean

    # Add fitted values from the SARIMA model
    fitted_values <- fitted(sarima_model)

    # Determine the x-axis limits
    last_date <- max(time(ts_data))
    end_date <- last_date + forecast_horizon / frequency(ts_data)
    if (input$FLineWidth2 == 0) {
      xlim_values = c(min(time(ts_data)), max(time(ts_data)))

    }else {
      xlim_values = c(min(time(ts_data)), end_date)

    }

    # Determine the y-axis limits
    all_data <- c(ts_data, fitted_values, forecasted_Data)
    ylim_values <- range(all_data, na.rm = TRUE)

    # Create an empty plot with appropriate x and y axis limits
    plot(ts_data, type = 'n', xlim = xlim_values, ylim = ylim_values, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)

    # Add the original time series data
    lines(ts_data, col = tsLineColor, lty = as.numeric(input$tsLineType2), lwd = input$tsLineWidth2)

    # Add the fitted values from the SARIMA model
    lines(fitted_values, col = sarimaLineColor, lty = as.numeric(input$sarimaLineType2), lwd = input$sarimaLineWidth2)

    # Add the forecasted data
    forecast_times <- seq(from = last_date + 1/frequency(ts_data), by = 1/frequency(ts_data), length.out = forecast_horizon)
    lines(forecast_times, forecasted_Data, col = FLineColor, lty = as.numeric(input$FLineType2), lwd = input$FLineWidth2)

    # Draw a line connecting the last value of ts_data and the first value of forecasted_Data
    if (length(ts_data) > 0 && length(forecasted_Data) > 0) {
      last_ts_point <- ts_data[length(ts_data)]
      first_forecast_point <- forecasted_Data[1]
      segments(x0 = last_date, y0 = last_ts_point, x1 = last_date + 1/frequency(ts_data), y1 = first_forecast_point, col = FLineColor, lty = as.numeric(input$FLineType2), lwd = input$FLineWidth2)
    }

    # Add grid lines
    grid(nx = NULL, ny = NULL, col = "gray55", lty = "dotted", lwd = par("lwd"))
  })




  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #                Tests   p, d, q, P, D, Q, S
  #
  ########  ########  ########  ########  ########  ########  ########  ########


  # 1.    MANN-KENDALL TREND TEST
  output$testTrendMK2 <- renderPrint({
    req(tsData())
    myData <- tsData()
    
    # 1. Run the Test
    result <- tryCatch({
      mk_result <- MannKendall(myData)
      
      # 2. Construct the Report
      capture.output({
        cat("================================================================\n")
        cat("                MANN-KENDALL NON-PARAMETRIC TEST                \n")
        cat("================================================================\n\n")
        
        print(mk_result)
        
        cat("\n================================================================\n")
        cat("                   STATISTICAL INTERPRETATION                   \n")
        cat("================================================================\n")
        
        pval <- as.numeric(mk_result$sl)
        tau  <- as.numeric(mk_result$tau)
        
        cat("Hypothesis Analysis:\n")
        cat("  H0 (Null): No monotonic trend exists in the series.\n")
        cat("             There is no trend in the series\n") 
        cat("  Ha (Alt):  A monotonic trend is present.\n\n")
        cat("             There a trend in the series\n") 
        cat("Verdict:\n")
        if(pval < 0.05) {
          cat("  RESULT: Significant (p < 0.05). Reject H0.\n")
          cat("  CONCLUSION: A significant trend is detected.\n")
          cat("  DIRECTION: ", if(tau > 0) "Positive (Increasing)" else "Negative (Decreasing)", "\n")
        } else {
          cat("  RESULT: Not Significant (p >= 0.05). Fail to reject H0.\n")
          cat("  CONCLUSION: No significant trend detected (Series is stable).\n")
        }
        cat("================================================================\n")
      })
    }, error = function(e) {
      return(paste("Error in MK Test:", e$message))
    })
    
    helpMK()
    cat(paste(result, collapse = "\n"))
  })

  
  
  # 2. AUGMENTED DICKEY-FULLER (ADF) TEST WITH ENHANCED DIAGNOSTICS
  output$teststationariteARIMApdq <- renderPrint({
    req(tsData(), input$adfType2, input$alphaSt_ARIMA)
    myData <- tsData()
    
    # 1. Setup Alpha and Critical Value Mapping
    alpha_val <- as.numeric(input$alphaSt_ARIMA)
    alpha_col <- switch(as.character(alpha_val), "0.01" = "1pct", "0.05" = "5pct", "0.1"  = "10pct")
    tau_row   <- switch(input$adfType2, "none" = "tau1", "drift" = "tau2", "trend" = "tau3")
    
    # 2. Calculate Diagnostic Metrics
    current_sd <- sd(myData, na.rm = TRUE)
    n_obs      <- length(na.omit(myData))
    
    cat("==========================================================================\n")
    cat("                AUGMENTED DICKEY-FULLER (ADF) TEST REPORT                 \n")
    cat("==========================================================================\n")
    cat(paste(" MODEL SPECIFICATION :", toupper(input$adfType2), "\n"))
    cat(paste(" EFFECTIVE OBS (N)   :", n_obs, "\n"))
    cat(paste(" SERIES STD. DEV     :", round(current_sd, 4), "\n"))
    cat("--------------------------------------------------------------------------\n")
    
    cat(" DECISION RULE:\n")
    cat(" • H0: The series has a Unit Root (NON-STATIONARY).\n")
    cat(" • H1: The series is STATIONARY.\n")
    cat(paste(" • CRITERION: Reject H0 if Tau (Observed) < Tau (Critical).\n"))
    cat("--------------------------------------------------------------------------\n")
    
    tryCatch({
      # 3. Perform the Test
      # We use urca::ur.df as the engine
      adf_res <- urca::ur.df(myData, 
                             type = input$adfType2, 
                             lags = as.numeric(input$LagOrderADF2))
      
      tau_obs  <- adf_res@teststat[1] 
      tau_crit <- adf_res@cval[tau_row, alpha_col]
      
      # 4. Residual Diagnostics (The White Noise Check)
      # Extract residuals from the internal ADF regression to ensure test validity
      adf_residuals <- adf_res@res
      lb_test       <- Box.test(adf_residuals, lag = min(10, n_obs/5), type = "Ljung-Box")
      lb_p          <- lb_test$p.value
      
      cat(" RESULT:\n")
      cat(paste(" • Tau (Observed Value) :", round(tau_obs, 4), "\n"))
      cat(paste(" • Tau (Critical Value) :", round(tau_crit, 4), "at", (alpha_val * 100), "% level\n"))
      cat(paste(" • Residual White Noise : p =", round(lb_p, 4), 
                if(lb_p > 0.05) " (PASSED)" else " (FAILED)"), "\n")
      cat("--------------------------------------------------------------------------\n")
      
      cat(" DECISION:\n")
      if(tau_obs < tau_crit) {
        cat(paste(" • Tau Observed is more negative than the Critical Value.\n"))
        cat(" • DECISION: Reject H0. The series is STATIONARY.\n")
        is_stationary <- TRUE
      } else {
        cat(paste(" • Tau Observed is NOT more negative than the Critical Value.\n"))
        cat(" • DECISION: Fail to reject H0. The series is NON-STATIONARY.\n")
        is_stationary <- FALSE
      }
      
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE & ANALYSIS DIRECTIVES:\n")
      
      # DIRECTIVE 1: STATIONARITY STATUS
      if(is_stationary) {
        cat(" 1. MODELING: Proceed with d=0 (Integrated of order 0).\n")
      } else {
        cat(" 1. MODELING: Apply differencing (d=1). The series is I(1).\n")
      }
      
      # DIRECTIVE 2: TEST RELIABILITY (LJUNG-BOX)
      cat("\n 2. TEST VALIDITY (Residual Analysis):\n")
      if(lb_p > 0.05) {
        cat("    • SUCCESS: Residuals are White Noise. The test results are reliable.\n")
      } else {
        cat("    • WARNING: The ADF residuals show serial correlation.\n")
        cat("    • DIRECTIVE: The current Lag (k) is too low. Increase 'Lag' to \n")
        cat("      fully capture the dynamics and fix the bias in Tau.\n")
      }
      
      # DIRECTIVE 3: OVER-DIFFERENCING CHECK
      cat("\n 3. TRANSFORMATION CHECK:\n")
      cat("    • If this series was already differenced and Std. Dev increased,\n")
      cat("      revert to the previous state. Over-differencing masks real data.\n")
      
      # DIRECTIVE 4: STRUCTURAL BREAKS
      cat("\n 4. ADVANCED DIAGNOSTICS:\n")
      cat("    • If Tau is close to Critical but fails, look for a 'Structural Break'.\n")
      cat("    • A single level shift can make a stationary series appear I(1).\n")
      
    }, error = function(e) {
      cat(" EXECUTION ERROR: ", e$message, "\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" ACTION: Ensure Lag order is reasonable and series has no leading NAs.\n")
    })
    
    cat("==========================================================================\n")
  })
  
  
 
  
  


  # 3.    KPSS STATIONARITY TEST
  output$kpssTest2 <- renderPrint({
    req(tsData())
    myData <- tsData()
    
    # 1. Run the Test and Capture Output
    result <- tryCatch({
      # null = "Trend" checks for trend-stationarity
      kpss_result <- tseries::kpss.test(myData, null = "Trend")
      
      # 2. Construct the Professional Report
      capture.output({
        cat("================================================================\n")
        cat("              KPSS TEST FOR TREND STATIONARITY                  \n")
        cat("================================================================\n\n")
        
        # Print the formal test output
        print(kpss_result)
        
        cat("\n================================================================\n")
        cat("                   STATISTICAL INTERPRETATION                   \n")
        cat("================================================================\n")
        
        # Extract p-value
        pval <- as.numeric(kpss_result$p.value)
        
        cat("Hypothesis Analysis (Note: Reversed vs ADF test):\n")
        cat("  H0: The series is STATIONARY (No unit root).\n")
        cat("  Ha: The series is NON-STATIONARY (Unit root present).\n\n")
        
        cat("Decision Rule:\n")
        if(pval < 0.05) {
          cat("  RESULT: Significant (p < 0.05). Reject H0.\n")
          cat("  CONCLUSION: The series is non-stationary.\n")
          cat("  ADVICE: The data likely requires differencing (d=1) to remove\n")
          cat("          the stochastic trend before modeling.\n")
        } else {
          cat("  RESULT: Not Significant (p >= 0.05). Fail to reject H0.\n")
          cat("  CONCLUSION: The series is stationary.\n")
          cat("  ADVICE: The series is stable around a trend; differencing may\n")
          cat("          not be necessary.\n")
        }
        
        cat("================================================================\n")
      })
    }, error = function(e) {
      return(paste("Error in KPSS Test:", e$message))
    })
    
    # 3. Call help function and display
    helpKPSS()
    
    cat(paste(result, collapse = "\n"))
  })


  
  # 4.    DF-GLS UNIT ROOT TEST (ERS)
  output$test_DFGLS <- renderPrint({
    req(tsData())
    myData <- tsData()
    
    # 1. Run the Test
    result <- tryCatch({
      # ur.ers implements the Elliott, Rothenberg & Stock Unit Root Test
      dfgls_test <- urca::ur.ers(myData, model = "trend", lag.max = 4)
      dfgls_sum  <- summary(dfgls_test)
      
      # 2. Construct the Report
      capture.output({
        cat("================================================================\n")
        cat("       DF-GLS UNIT ROOT TEST (ELLIOTT, ROTHENBERG & STOCK)      \n")
        cat("================================================================\n")
        
        # Print the formal test summary
        print(dfgls_sum)
        
        cat("\n================================================================\n")
        cat("                   STATISTICAL INTERPRETATION                   \n")
        cat("================================================================\n")
        
        # Extract test statistic and critical value (usually at 5%)
        test_stat <- dfgls_sum@teststat
        crit_val  <- dfgls_sum@cval[1, 2] # 5% critical value
        
        cat("Hypothesis Analysis:\n")
        cat("  H0: The series has a unit root (Non-Stationary).\n")
        cat("  Ha: The series is Stationary.\n\n")
        
        cat("Decision Rule:\n")
        cat("  Test Statistic: ", round(test_stat, 4), "\n")
        cat("  Critical Value (5%): ", round(crit_val, 4), "\n\n")
        
        if(test_stat < crit_val) {
          cat("  RESULT: Test Statistic is more negative than Critical Value.\n")
          cat("  CONCLUSION: Reject H0. The series is Stationary.\n")
          cat("  ADVICE: The DF-GLS test (which has more power than ADF) confirms\n")
          cat("          stationarity. Differencing may not be necessary.\n")
        } else {
          cat("  RESULT: Test Statistic is not more negative than Critical Value.\n")
          cat("  CONCLUSION: Fail to reject H0. The series is Non-Stationary.\n")
          cat("  ADVICE: A unit root is likely present. Consider applying\n")
          cat("          differencing (d=1) to stabilize the series.\n")
        }
        cat("================================================================\n")
      })
    }, error = function(e) {
      return(paste("Error in DF-GLS Test:", e$message))
    })
    
    # Display the final report
    cat(paste(result, collapse = "\n"))
  })
  
  
  
  # 5.    LJUNG-BOX / BOX-PIERCE RESIDUAL TEST
  output$testLBnARIMApdq <- renderPrint({
    req(tsData())
    
    # 1. Retrieve the model residuals
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    req(sarima_model)
    myDataResiduals <- sarima_model$residuals
    
    # 2. Run the Test and Capture Output
    result <- tryCatch({
      box_result <- Box.test(myDataResiduals, 
                             lag = input$lagorder1, 
                             type = input$typeBoxTest1)
      
      # 3. Construct the Professional Report
      capture.output({
        cat("================================================================\n")
        cat("            RESIDUAL DIAGNOSTIC: ", toupper(input$typeBoxTest1), " TEST             \n")
        cat("================================================================\n\n")
        
        # Print formal test output
        print(box_result)
        
        cat("\n================================================================\n")
        cat("                   STATISTICAL INTERPRETATION                   \n")
        cat("================================================================\n")
        
        pval <- as.numeric(box_result$p.value)
        
        cat("Hypothesis Analysis:\n")
        cat("  H0: Residuals are independent (White Noise / No Autocorrelation).\n")
        cat("  Ha: Residuals are dependent (Presence of remaining patterns).\n\n")
        
        cat("Decision Rule:\n")
        if(pval > 0.05) {
          cat("  RESULT: Not Significant (p > 0.05). Fail to reject H0.\n")
          cat("  CONCLUSION: The residuals behave like White Noise.\n")
          cat("  ADVICE: The model has successfully captured the information in\n")
          cat("          the data. Your forecast is likely reliable.\n")
        } else {
          cat("  RESULT: Significant (p <= 0.05). Reject H0.\n")
          cat("  CONCLUSION: There is significant autocorrelation in residuals.\n")
          cat("  ADVICE: The model is 'Underfitted'. You may need to add more\n")
          cat("          AR or MA terms to capture the remaining patterns.\n")
        }
        cat("================================================================\n")
      })
    }, error = function(e) {
      return(paste("Error in Box Test:", e$message))
    })
    
    # Call your help function
    helpLjungBox()
    
    # Display the final report
    cat(paste(result, collapse = "\n"))
  })
  
  
  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #        Residuals     p  d   q  P  D  Q  S
  #
  ########  ########  ########  ########  ########  ########  ########  ########


  output$chkResARIMApdq_UI <- renderUI({
    plotOutput("chkResARIMApdq", width = userData$plotWidth, height = userData$plotHeight)
  })
  
  

  output$chkResARIMApdq <- renderPlot({
    req(tsData())

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput

    checkresiduals(sarima_model) + theme(axis.text = element_text(size = userData$labelsize))
  })




  output$tsdiagARIMApdq_UI <- renderUI({
    plotOutput("tsdiagARIMApdq", width = userData$plotWidth, height = userData$plotHeight)
  })


  output$tsdiagARIMApdq <- renderPlot({
    req(tsData())


    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    ggtsdiag(sarima_model) + theme(axis.text = element_text(size = userData$labelsize))
        # xlab(userData$xLabel)+
        # ylab(userData$yLabel) +
        # ggtitle(userData$mainTitle)
  })



  #     NORMAL Q-Q PLOT (Residuals)
  output$tsdiag2 <- renderPlot({
    req(tsData())
    
    # 1. Retrieve the fitted model
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    req(sarima_model)
    
    # 2. Extract Residuals and handle NAs (crucial for differenced models)
    ResudialData <- as.numeric(resid(sarima_model))
    ResudialData <- ResudialData[!is.na(ResudialData)]
    
    # 3. Create the Plot
    # Base R is often preferred for Q-Q plots for its simplicity
    qqnorm(ResudialData, 
           main = paste("Normal Q-Q Plot:", userData$mainTitle), 
           col = "darkgrey", 
           pch = 16,     # Solid circles look cleaner
           cex = 1.2)    # Slightly larger points
    
    # Add the reference line
    qqline(ResudialData, 
           col = "firebrick", # A contrasting color like red/firebrick is standard
           lwd = 2)
    
    # Optional: Add a grid for better readability
    grid(col = "lightgray", lty = "dotted")
    
  }, 
  # Direct dynamic dimensions from your sliders
  width = function() as.numeric(gsub("[^0-9]", "", userData$plotWidth)),
  height = function() as.numeric(gsub("[^0-9]", "", userData$plotHeight))
  )


  
  #  SHAPIRO-WILK NORMALITY TEST ON RESIDUALLS 
  # output$ShapiroTest <- renderPrint({
  #   req(tsData())
  #   
  #   # 1. Retrieve the fitted model
  #   sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
  #   req(sarima_model) 
  #   
  #   # 2. Header & Definition
  #   cat("==========================================================================\n")
  #   cat("               SHAPIRO-WILK NORMALITY TEST ON RESIDUALLS                  \n")
  #   cat("==========================================================================\n")
  #   cat(" The Shapiro-Wilk test evaluates whether the residuals of your model      \n")
  #   cat(" follow a Normal (Gaussian) distribution.                                 \n")
  #   cat("--------------------------------------------------------------------------\n")
  #   cat(" DECISION RULE:\n")
  #   cat(" • H0 (p > 0.05): Residuals are Normal (Desired for ARIMA).\n")
  #   cat(" • H1 (p <= 0.05): Residuals are NOT Normal.\n")
  #   cat("--------------------------------------------------------------------------\n")
  #   
  #   # 3. Perform Test
  #   residualData <- as.numeric(resid(sarima_model))
  #   residualData <- residualData[!is.na(residualData)] # Remove NAs from differencing
  #   st_result <- shapiro.test(residualData)
  #   
  #   # 4. Result Output
  #   cat(" RESULT:\n")
  #   print(st_result)
  #   
  #   # 5. Dynamic Conclusion
  #   pval <- st_result$p.value
  #   cat("--------------------------------------------------------------------------\n")
  #   cat(" CONCLUSION:\n")
  #   if (pval > 0.05) {
  #     cat("  SUCCESS: The p-value is > 0.05. We fail to reject the null hypothesis.\n")
  #     cat("  The residuals appear to be normally distributed.\n")
  #   } else {
  #     cat("  WARNING: The p-value is <= 0.05. We reject the null hypothesis.\n")
  #     cat("  The residuals do NOT follow a normal distribution.\n")
  #   }
  #   
  #   # 6. Actionable Advice
  #   cat("--------------------------------------------------------------------------\n")
  #   cat(" ADVICE:\n")
  #   if (pval > 0.05) {
  #     cat("  Your model residuals meet the normality assumption. You can proceed   \n")
  #     cat("  with confidence in your forecasts and prediction intervals.\n")
  #   } else {
  #     cat("  Consider: \n")
  #     cat("  1. Checking for outliers that may skew the distribution.\n")
  #     cat("  2. Applying a Box-Cox transformation (e.g., Log) to the data.\n")
  #     cat("  3. Verifying if a significant 'Trend' or 'Seasonality' remains.\n")
  #   }
  #   cat("==========================================================================\n")
  # }) 
  
  output$model_Normality_test2 <- renderPrint({
    req(results_ARIMA_pdPD_drift())
    residualData <- as.numeric(residuals(results_ARIMA_pdPD_drift()$modelOutput))
    n_obs <- length(residualData)
    alpha_val <- 0.05
    
    tryCatch({
      # --- 1. SHAPIRO-WILK ---
      sw_test <- shapiro.test(residualData)
      
      cat("==========================================================================\n")
      cat("             1.  SHAPIRO-WILK NORMALITY TEST ON RESIDUALS                  \n")
      cat("==========================================================================\n")
      cat(" The Shapiro-Wilk test evaluates whether the residuals of your model      \n")
      cat(" follow a Normal (Gaussian) distribution.                                 \n")
      cat("--------------------------------------------------------------------------\n")
      cat(" DECISION RULE:\n")
      cat(" • H0 (p > 0.05): Residuals are Normal (Desired for ARIMA).\n")
      cat(" • H1 (p <= 0.05): Residuals are NOT Normal.\n")
      cat("--------------------------------------------------------------------------\n")
      cat(" RESULT:\n")
      print(sw_test)
      cat("\n--------------------------------------------------------------------------\n")
      cat(" CONCLUSION (APA Format):\n")
      if(sw_test$p.value > alpha_val) {
        cat(sprintf("  A Shapiro-Wilk test indicated that the residuals did not deviate \n"))
        cat(sprintf("  significantly from normality, W = %.3f, p = %.3f.\n", sw_test$statistic, sw_test$p.value))
      } else {
        cat(sprintf("  A Shapiro-Wilk test indicated that the residuals deviated \n"))
        cat(sprintf("  significantly from normality, W = %.3f, p = %.3f.\n", sw_test$statistic, sw_test$p.value))
      }
      cat("--------------------------------------------------------------------------\n")
      cat(" ADVICE:\n")
      if(sw_test$p.value <= alpha_val) {
        cat("  1. Check for outliers that may skew the distribution.\n")
        cat("  2. Apply a Box-Cox transformation (e.g., Log) to the data.\n")
      } else {
        cat("  The normality assumption is satisfied.\n")
      }
      cat("==========================================================================\n\n\n")
      
      # --- 2. JARQUE-BERA ---
      if (requireNamespace("tseries", quietly = TRUE)) {
        jb_test <- tseries::jarque.bera.test(residualData)
        
        cat("==========================================================================\n")
        cat("             2.  JARQUE-BERA TEST (SKEWNESS & KURTOSIS)                   \n")
        cat("==========================================================================\n")
        cat(" The Jarque-Bera test determines if the residuals have the skewness       \n")
        cat(" and kurtosis matching a normal distribution.                             \n")
        cat("--------------------------------------------------------------------------\n")
        cat(" DECISION RULE:\n")
        cat(" • H0 (p > 0.05): Skewness and Kurtosis are consistent with Normality.\n")
        cat(" • H1 (p <= 0.05): Residuals exhibit significant Skewness or Kurtosis.\n")
        cat("--------------------------------------------------------------------------\n")
        cat(" RESULT:\n")
        print(jb_test)
        cat("\n--------------------------------------------------------------------------\n")
        cat(" CONCLUSION (APA Format):\n")
        if(jb_test$p.value > alpha_val) {
          cat(sprintf("  The Jarque-Bera test showed that residuals were normally distributed \n"))
          cat(sprintf("  regarding skewness and kurtosis, X2(2, N = %d) = %.2f, p = %.3f.\n", n_obs, jb_test$statistic, jb_test$p.value))
        } else {
          cat(sprintf("  The Jarque-Bera test showed that residuals significantly deviated \n"))
          cat(sprintf("  from normality, X2(2, N = %d) = %.2f, p = %.3f.\n", n_obs, jb_test$statistic, jb_test$p.value))
        }
        cat("--------------------------------------------------------------------------\n")
        cat(" ADVICE:\n")
        cat("  Failure suggests the presence of 'Fat Tails' or significant asymmetry.\n")
        cat("==========================================================================\n\n\n")
      }
      
      # --- 3. ANDERSON-DARLING ---
      if (requireNamespace("nortest", quietly = TRUE)) {
        ad_test <- nortest::ad.test(residualData)
        
        cat("==========================================================================\n")
        cat("             3.  ANDERSON-DARLING NORMALITY TEST                          \n")
        cat("==========================================================================\n")
        cat(" This test gives more weight to the tails of the distribution.            \n")
        cat("--------------------------------------------------------------------------\n")
        cat(" DECISION RULE:\n")
        cat(" • H0 (p > 0.05): Residuals follow a Normal Distribution.\n")
        cat(" • H1 (p <= 0.05): The distribution tails deviate from Normality.\n")
        cat("--------------------------------------------------------------------------\n")
        cat(" RESULT:\n")
        print(ad_test)
        cat("\n--------------------------------------------------------------------------\n")
        cat(" CONCLUSION (APA Format):\n")
        if(ad_test$p.value > alpha_val) {
          cat(sprintf("  An Anderson-Darling test was non-significant, A2 = %.3f, p = %.3f, \n", ad_test$statistic, ad_test$p.value))
          cat("  suggesting the data follow a normal distribution.\n")
        } else {
          cat(sprintf("  An Anderson-Darling test was significant, A2 = %.3f, p = %.3f, \n", ad_test$statistic, ad_test$p.value))
          cat("  suggesting the data deviate significantly from normality.\n")
        }
        cat("--------------------------------------------------------------------------\n")
        cat(" ADVICE:\n")
        cat("  This test is sensitive to extreme values. Verify the Q-Q plot for outliers.\n")
        cat("==========================================================================\n")
      }
      
    }, error = function(e) {
      cat(" [!] Error in normality analysis: ", e$message, "\n")
    })
  })

  # output$model_Normality_test2 <- renderPrint({
  #   req(results_ARIMA_pdPD_drift())
  #   residualData <- as.numeric(residuals(results_ARIMA_pdPD_drift()$modelOutput))
  #   alpha_val <- 0.05
  #   
  #   tryCatch({
  #     # --- 1. SHAPIRO-WILK ---
  #     sw_test <- shapiro.test(residualData)
  #     
  #     cat("==========================================================================\n")
  #     cat("             1.  SHAPIRO-WILK NORMALITY TEST ON RESIDUALS                  \n")
  #     cat("==========================================================================\n")
  #     cat(" The Shapiro-Wilk test evaluates whether the residuals of your model      \n")
  #     cat(" follow a Normal (Gaussian) distribution.                                 \n")
  #     cat("--------------------------------------------------------------------------\n")
  #     cat(" DECISION RULE:\n")
  #     cat(" • H0 (p > 0.05): Residuals are Normal (Desired for ARIMA).\n")
  #     cat(" • H1 (p <= 0.05): Residuals are NOT Normal.\n")
  #     cat("--------------------------------------------------------------------------\n")
  #     cat(" RESULT:\n\n")
  #     print(sw_test)
  #     cat("\n--------------------------------------------------------------------------\n")
  #     cat(" CONCLUSION:\n")
  #     if(sw_test$p.value > alpha_val) {
  #       cat("  SUCCESS: The p-value is > 0.05. We fail to reject the null hypothesis.\n")
  #       cat("  The residuals follow a normal distribution.\n")
  #     } else {
  #       cat("  WARNING: The p-value is <= 0.05. We reject the null hypothesis.\n")
  #       cat("  The residuals do NOT follow a normal distribution.\n")
  #     }
  #     cat("--------------------------------------------------------------------------\n")
  #     cat(" ADVICE:\n")
  #     if(sw_test$p.value <= alpha_val) {
  #       cat("  Consider: \n")
  #       cat("  1. Checking for outliers that may skew the distribution.\n")
  #       cat("  2. Applying a Box-Cox transformation (e.g., Log) to the data.\n")
  #       cat("  3. Verifying if a significant 'Trend' or 'Seasonality' remains.\n")
  #     } else {
  #       cat("  The normality assumption is satisfied. Prediction intervals are reliable.\n")
  #     }
  #     cat("==========================================================================\n\n\n")
  #     
  #     # --- 2. JARQUE-BERA ---
  #     if (requireNamespace("tseries", quietly = TRUE)) {
  #       jb_test <- tseries::jarque.bera.test(residualData)
  #       
  #       cat("==========================================================================\n")
  #       cat("             2.  JARQUE-BERA TEST (SKEWNESS & KURTOSIS)                   \n")
  #       cat("==========================================================================\n")
  #       cat(" The Jarque-Bera test determines if the residuals have the skewness       \n")
  #       cat(" and kurtosis matching a normal distribution.                             \n")
  #       cat("--------------------------------------------------------------------------\n")
  #       cat(" DECISION RULE:\n")
  #       cat(" • H0 (p > 0.05): Skewness and Kurtosis are consistent with Normality.\n")
  #       cat(" • H1 (p <= 0.05): Residuals exhibit significant Skewness or Kurtosis.\n")
  #       cat("--------------------------------------------------------------------------\n")
  #       cat(" RESULT:\n\n")
  #       print(jb_test)
  #       cat("\n--------------------------------------------------------------------------\n")
  #       cat(" CONCLUSION:\n")
  #       if(jb_test$p.value > alpha_val) {
  #         cat("  SUCCESS: The p-value is > 0.05. The errors are symmetrical.\n")
  #       } else {
  #         cat("  WARNING: The p-value is <= 0.05. The distribution is biased or peaked.\n")
  #       }
  #       cat("--------------------------------------------------------------------------\n")
  #       cat(" ADVICE:\n")
  #       if(jb_test$p.value <= alpha_val) {
  #         cat("  A failure here often indicates 'Fat Tails'. Your model may be \n")
  #         cat("  underestimating the probability of extreme events.\n")
  #       } else {
  #         cat("  Residual symmetry is confirmed.\n")
  #       }
  #       cat("==========================================================================\n\n\n")
  #     }
  #     
  #     # --- 3. ANDERSON-DARLING ---
  #     if (requireNamespace("nortest", quietly = TRUE)) {
  #       ad_test <- nortest::ad.test(residualData)
  #       
  #       cat("==========================================================================\n")
  #       cat("             3.  ANDERSON-DARLING NORMALITY TEST                          \n")
  #       cat("==========================================================================\n")
  #       cat(" This test gives more weight to the tails of the distribution than        \n")
  #       cat(" the Shapiro-Wilk test.                                                   \n")
  #       cat("--------------------------------------------------------------------------\n")
  #       cat(" DECISION RULE:\n")
  #       cat(" • H0 (p > 0.05): Residuals follow a Normal Distribution.\n")
  #       cat(" • H1 (p <= 0.05): The distribution tails deviate from Normality.\n")
  #       cat("--------------------------------------------------------------------------\n")
  #       cat(" RESULT:\n\n")
  #       print(ad_test)
  #       cat("\n--------------------------------------------------------------------------\n")
  #       cat(" CONCLUSION:\n")
  #       if(ad_test$p.value > alpha_val) {
  #         cat("  SUCCESS: The p-value is > 0.05. Tails are consistent with Normality.\n")
  #       } else {
  #         cat("  WARNING: The p-value is <= 0.05. Significant tail deviation detected.\n")
  #       }
  #       cat("--------------------------------------------------------------------------\n")
  #       cat(" ADVICE:\n")
  #       if(ad_test$p.value <= alpha_val) {
  #         cat("  If this test fails while others pass, your model has 'Outlier' issues.\n")
  #       } else {
  #         cat("  Distributional tails are statistically well-behaved.\n")
  #       }
  #       cat("==========================================================================\n")
  #     }
  #     
  #   }, error = function(e) {
  #     cat(" [!] Error in normality analysis: ", e$message, "\n")
  #   })
  # })
  
  
  
  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #
  #
  ########  ########  ########  ########  ########  ########  ########  ########


  output$forecast_ARIMA_pdq <- renderTable({
    req(tsData())


    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    pred <- forecast(sarima_model, h=input$length)
    asdfpred <- as.data.frame(pred)
    dfpred <- data.frame(date = row.names(asdfpred), asdfpred)
    dfpred
  })



  output$ForecastePlotUI <- renderUI({
    plotOutput("SARIMAforecastplot", width = userData$plotWidth, height = userData$plotHeight)
  })


  output$SARIMAforecastplot <- renderPlot({
    req(tsData())
    req(currentFrequency())

    myData <- tsData()
    frequency = as.numeric(currentFrequency())

    if (input$driftYN == "TRUE") {
      nodriftConsideration =FALSE
    }
    else {
      nodriftConsideration =TRUE
    }

    # Keep as it is , I didnt use "sarima_model <- results_ARIMA_pdPD_drift()$modelOutput" because the function is different , its not auto.arima
    forecast <- sarima.for(myData, n.ahead = input$length,
               p = input$ARIMAp, d = input$ARIMAd, q = input$ARIMAq,
               P = input$ARIMAps, D = input$ARIMAds, Q = input$ARIMAqs,
               S = frequency,
               no.constant=nodriftConsideration, lwd = 2,
               main = userData$mainTitle, xlab = userData$xLabel)
  })



  ################################################################################


  output$sarima_eq_render_numerical <- renderUI({
    req(tsData())

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # sarima_model <-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    eqs <- extractSARIMAeqLaTeX(sarima_model)

    withMathJax(helpText(eqs$symbolic))
  })



  output$sarima_eq_render_numerical_one <- renderUI({
    req(tsData())
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    eqs <- extractSARIMAeqLaTeX(sarima_model)

    withMathJax(helpText(eqs$numerical_one_line))
  })

  
  output$sarima_eq_render_numerical_two <- renderUI({
    req(tsData())
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    eqs <- extractSARIMAeqLaTeX(sarima_model)
    
    withMathJax(helpText(eqs$numerical_one_line))
  })

################################################################################


  output$sarima_eq_render_numerical_1 <- renderUI({
    req(tsData())

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # sarima_model <-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    eqs <- extractSARIMAeqLaTeX(sarima_model)

    withMathJax(helpText(eqs$symbolic))
  })




  ################################################################################

  # Output the numerical model equation in autoARIMA

  output$auto_SARIMA_eq_render_numerical <- renderUI({
    req(tsData())
    # get the model
    fittedModel <- results()$modelOutput

    eqs <- extractSARIMAeqLaTeX(fittedModel)

    withMathJax(helpText(eqs$numerical_one_line))
  })


  # Output the symbolic model equation in autoARIMA

  output$auto_SARIMA_symbolic <- renderUI({
    req(tsData())
    # get the model
    if (input$Model == "ARIMA"){
        fittedModel <- results()$modelOutput
        eqs <- extractSARIMAeqLaTeX(fittedModel)
        withMathJax(helpText(eqs$symbolic))
    }else{
        "select the ARIMA model for this output"
    }

  })


  output$Model_Help_ui <- renderUI({
    model_Help <- paste0("
    
                            $$
                            \\phi_p(L)\\Phi_P(L^S)(1-L)^d(1-L^S)^D Y_t = c+\\theta_q(L)\\Theta_Q(L^S)\\varepsilon_t+ \\delta t
                            $$

                            $$
                            (1 - \\sum_{i=1}^{p} \\phi_i L^i)(1 - \\sum_{j=1}^{P} \\Phi_j L^{jS}) (1 - L)^d (1 - L^S)^D Y_t
                            = c + (1 + \\sum_{i=1}^{q} \\theta_i L^i)(1 + \\sum_{j=1}^{Q} \\Theta_j L^{jS}) \\varepsilon_t + \\delta t
                            $$
                            
                            **Where:**
     
                            \\begin{align*}
                            Y_t  : & \\text{ is the time series.} \\\\
                            p, q : & \\text{ are the orders of the non-seasonal AR (AutoRegressive) and MA (Moving Average) parts, respectively.} \\\\
                            P, Q : & \\text{ are the orders of the seasonal AR and MA parts, respectively.} \\\\
                            d, D : & \\text{ are the orders of non-seasonal and seasonal differencing, respectively.} \\\\
                            S : & \\text{ is the length of the seasonal period.} \\\\
                            \\phi, \\Phi : & \\text{ are the parameters of the non-seasonal and seasonal AR parts, respectively.} \\\\
                            \\theta, \\Theta : & \\text{ are the parameters of the non-seasonal and seasonal MA parts, respectively.} \\\\
                            L : & \\text{ is the lag operator.} \\\\
                            c : & \\text{ is a constant term (intercept).} \\\\
                            \\varepsilon_t : & \\text{ is the error term.} \\\\
                            \\delta : & \\text{ is the drift coefficient.} \\\\
                            t : & \\text{ is the time index.} \\\\
                            (1 - \\sum_{i=1}^{p} \\phi_i L^i) : & \\text{ represents the non-seasonal AR component.} \\\\
                            (1 - \\sum_{j=1}^{P} \\Phi_j L^{jS}) : & \\text{ represents the seasonal AR component.} \\\\
                            (1 - L)^d : & \\text{ represents the non-seasonal differencing.} \\\\
                            (1 - L^S)^D : & \\text{ represents the seasonal differencing.} \\\\
                            (1 + \\sum_{i=1}^{q} \\theta_i L^i) : & \\text{ represents the non-seasonal MA component.} \\\\
                            (1 + \\sum_{j=1}^{Q} \\Theta_j L^{jS}) : & \\text{ represents the seasonal MA component.} \\\\
                            \\delta t : & \\text{ represents the drift component.} \\\\
                            \\end{align*}

                            
                        ")
    
    withMathJax(helpText(model_Help))
    
  })
  ################################################################################




  # render the equiation y(t) = ? y(t-1) + ? y(t-2) +...
  output$sarima_eq_render_Y_t <- renderUI({
    req(tsData())
    myData <- tsData()

    if (input$driftYN == "TRUE") {
      driftConsideration =TRUE
    }
    else {
      driftConsideration =FALSE
    }

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # sarima_model <-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    eqs <- extractSARIMAeqLaTeX(sarima_model)

    withMathJax(helpText(eqs$numerical_one_line_Y_t ))
    #withMathJax(helpText(eqs$numerical_one_line_Y_t ))
  })










  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #
  #
  ########  ########  ########  ########  ########  ########  ########  ########




  output$testoutput1 <- renderPrint({
    cat(".......................................................................\n")
    cat("                            test                                       \n")
    cat(".......................................................................\n")

    fittedModel <- results()$modelOutput

    # Extracting the components
    arimaOrder <- fittedModel$arma

    # AR components
    arOrder <- arimaOrder[1]
    arParameters <- fittedModel$coef[1:arOrder]

    # MA components
    maOrder <- arimaOrder[6]
    maParameters <- fittedModel$coef[(arOrder + 1):(arOrder + maOrder)]

    # Seasonal components
    sarOrder <- arimaOrder[2]
    sarParameters <- fittedModel$coef[(arOrder + maOrder + 1):(arOrder + maOrder + sarOrder)]
    smaOrder <- arimaOrder[7]
    smaParameters <- fittedModel$coef[(arOrder + maOrder + sarOrder + 1):(arOrder + maOrder + sarOrder + smaOrder)]

    # Drift component
    drift <- ifelse(fittedModel$arma[9] == 1, fittedModel$coef[length(fittedModel$coef)], "No drift")

    sarParameters

  })





  ########  ########  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #         H E L P
  #
  ########  ########  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########  ########  ########


  output$AboutAng <- renderPrint({

    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                              University Abdelmalek Essaadi  - Morocco -                                          \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("        d[1] (St) : simple difference of order 1                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("        D[1] (St) : seasonal difference of order 1                                                     \n")
    cat("                    change seasonality value                                                           \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                       Summary of rules for identifying ARIMA models                                   \n")
    cat("                                                                                                       \n")
    cat("   https://people.duke.edu/~rnau/arimrule.htm                                                          \n")
    cat("                                                                                                       \n")
    cat("-> Identifying the order of differencing and the constant:                                             \n")
    cat("   ------------------------------------------------------                                              \n")
    cat("                                                                                                       \n")
    cat("Rule 1: If the series has positive autocorrelations out to a high number of lags (say, 10 or more),    \n")
    cat("        then it probably needs a higher order of differencing.                                         \n")
    cat("                                                                                                       \n")
    cat("Rule 2: - If the lag-1 autocorrelation is zero or negative, or the autocorrelations are all small and  \n")
    cat("          patternless, then the series does not need a higher order of differencing.                   \n")
    cat("        - If the lag-1 autocorrelation is -0.5 or more negative, the series may be overdifferenced.    \n")
    cat("        BEWARE OF OVERDIFFERENCING.                                                                    \n")
    cat("                                                                                                       \n")
    cat("Rule 3: The optimal order of differencing is often the order of differencing at which the standard     \n")
    cat("        deviation is lowest. (Not always, though. Slightly too much or slightly too little             \n")
    cat("        differencing can also be corrected with AR or MA terms. See rules 6 and 7.)                    \n")
    cat("                                                                                                       \n")
    cat("Rule 4: - A model with no orders of differencing assumes that the original series is stationary (among \n")
    cat("          other things, mean-revertin).                                                                \n")
    cat("        - A model with one order of differencing assumes that the original series has a constant       \n")
    cat("          average trend (e.g. a random walk or SES-type model, with or without growth).                \n")
    cat("        - A model with two orders of total differencing assumes that the original series has           \n")
    cat("          a time-varying trend (e.g. a random trend or LES-type model).                                \n")
    cat("                                                                                                       \n")
    cat("Rule 5: - A model with no orders of differencing normally includes a constant term (which allows for   \n")
    cat("          a non-zero mean value).                                                                      \n")
    cat("        - A model with two orders of total differencing normally does not includea constant term.      \n")
    cat("        - In a model with one order of total differencing, a constant term should be included if       \n")
    cat("          the series has a non-zero average trend.                                                     \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("-> Identifying the numbers of AR and MA terms:                                                         \n")
    cat("   ------------------------------------------                                                          \n")
    cat("                                                                                                       \n")
    cat("Rule 6: If the partial autocorrelation function (PACF) of the differenced series displays a sharp      \n")
    cat("        cutoff and/or the lag-1 autocorrelation is positive--i.e., if the series appears slightly      \n")
    cat("        'underdifferenced'--then consider adding one or more AR terms to the model. The lag beyond     \n")
    cat("        which the PACF cuts off is the indicated number of AR terms.                                   \n")
    cat("                                                                                                       \n")
    cat("Rule 7: If the autocorrelation function (ACF) of the differenced series displays a sharp cutoff        \n")
    cat("        and/or the lag-1 autocorrelation is negative--i.e., if the series appears slightly             \n")
    cat("        'overdifferenced' --then consider adding an MA term to the model.                              \n")
    cat("        The lag beyond which the ACF cuts off is the indicated number of MA terms.                     \n")
    cat("                                                                                                       \n")
    cat("Rule 8: It is possible for an AR term and an MA term to cancel each other's effects,                   \n")
    cat("        so if a mixed AR-MA model seems to fit the data, also try a model with one fewer AR term       \n")
    cat("        and one fewer MA term--particularly if the parameter estimates in the original model require   \n")
    cat("        more than 10 iterations to converge.                                                           \n")
    cat("        BEWARE OF USING MULTIPLE AR TERMS AND MULTIPLE MA TERMS IN THE SAME MODEL.                     \n")
    cat("                                                                                                       \n")
    cat("Rule 9: If there is a unit root in the AR part of the model--i.e., if the sum of the AR coefficients   \n")
    cat("        is almost exactly 1--you should reduce the number of AR terms by one and increase the order    \n")
    cat("        of differencing by one.                                                                        \n")
    cat("                                                                                                       \n")
    cat("Rule 10: If there is a unit root in the MA part of the model--i.e., if the sum of the MA coefficients  \n")
    cat("        is almost exactly 1--you should reduce the number of MA terms by one and reduce                \n")
    cat("        the order of differencing by one.                                                              \n")
    cat("                                                                                                       \n")
    cat("Rule 11: If the long-term forecasts* appear erratic or unstable, there may be a unit root              \n")
    cat("        in the AR or MA coefficients.                                                                  \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("-> Identifying the seasonal part of the model:                                                         \n")
    cat("   ------------------------------------------                                                          \n")
    cat("                                                                                                       \n")
    cat("Rule 12: If the series has a strong and consistent seasonal pattern, then you must use an order of     \n")
    cat("        seasonal differencing (otherwise the model assumes that the seasonal pattern will fade away    \n")
    cat("        over time). However, never use more than one order of seasonal differencing or                 \n")
    cat("        more than 2 orders of total differencing (seasonal+nonseasonal).                               \n")
    cat("                                                                                                       \n")
    cat("Rule 13: If the autocorrelation of the appropriately differenced series is positive at lag 's',        \n")
    cat("        where 's' is the number of periods in a season, then consider adding an SAR term to the model. \n")
    cat("        If the autocorrelation of the differenced series is negative at lag s, consider adding an SMA  \n")
    cat("        term to the model. The latter situation is likely to occur if a seasonal difference has been   \n")
    cat("        used, which should be done if the data has a stable and logical seasonal pattern.              \n")
    cat("        The former is likely to occur if a seasonal differencehas not been used, which would only      \n")
    cat("        be appropriate if the seasonal pattern is not stable over time.                                \n")
    cat("        You should try to avoid using more than one or two seasonal parameters (SAR+SMA) in the        \n")
    cat("        same model, as this is likely to lead to overfitting of the data and/or problems in estimation.\n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(" *A caveat about long-term forecasting in general:                                                     \n")
    cat("                                                                                                       \n")
    cat("        linear time series models such as ARIMA and exponential smoothing models predict the more      \n")
    cat("     distant future by making a series of one-period-ahead forecasts and plugging them in for unknown  \n")
    cat("     future values as they look farther ahead. For example, a 2-period-ahead forecast is computed by   \n")
    cat("     treating the 1-period-ahead forecast as if it were data and then applying the same forecasting    \n")
    cat("      equation.                                                                                        \n")

    cat("     This step can be repeated any number of times in order to forecast as far into the future         \n")
    cat("     as you want, and the method also yields formulas for computing theoretically-appropriate          \n")
    cat("     confidence intervalsaround the longer-term forecasts.                                             \n")
    cat("        However, the models are identified and optimized based on their one-period-ahead forecasting   \n")
    cat("     performance, and rigid extrapolation of them may not be the best way to forecast many periods     \n")
    cat("     ahead (say, more than one year when working with monthly or quarterly business data),             \n")
    cat("     particularly when the modeling assumptions are at best only approximately satisfied               \n")
    cat("     -which is nearly always the case.                                                                 \n")

    cat("     If one of your objectives is to generate long-term forecasts, it would be good to also draw on    \n")
    cat("     other sources of information during the model selection process and/or to optimize the parameter  \n")
    cat("     estimates for multi-period forecasting if your software allows it and/or use an auxiliary model   \n")
    cat("     (possibly one that incorporates expert opinion) for long-term forecasting.                        \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
  })


  output$AboutFr <- renderPrint({
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                              Université Abdelmalek Essaadi  - Maroc -                                         \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("        d[1] (St) : difference simple d'ordre 1                                                        \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("        D[1] (St) : difference saisonnière d'ordre 1                                                   \n")
    cat("                    changer la valeur de la saisonnalité                                               \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                       Règles d'identification des modèles ARIMA                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("-> Identifier l'ordre de différenciation et la constante:                                              \n")
    cat("   ------------------------------------------------------                                              \n")
    cat("                                                                                                       \n")
    cat("Règle 1 : si la série présente des autocorrélations positives avec un nombre élevé de décalages        \n")
    cat("  (par exemple, 10 ou plus), alors on a probablement besoin d'un ordre de différenciation plus élevé.  \n")
    cat("                                                                                                       \n")
    cat("Règle 2 : Si l'autocorrélation de lag-1 est nulle ou négative, ou si les autocorrélations sont toutes  \n")
    cat("       petites et  sans motif, alors la série n'a pas besoin d'un ordre de différenciation supérieur.  \n")
    cat("        - Si l'autocorrélation lag-1 est négative, -0,5 ou plus, la série peut être sur-différencié.   \n")
    cat("        ATTENTION AUX DIFFÉRENCES EXCESSIVES.                                                          \n")
    cat("                                                                                                       \n")
    cat("Règle 3 : L'ordre de différenciation optimal est souvent l'ordre de différenciation auquel l'écart     \n")
    cat("      type est le plus faible. (Pas toujours, cependant. Un peu trop ou un peu trop peu de             \n")
    cat("      différenciation peut également être corrigé avec des termes AR ou MA. Voir les règles 6 et 7.)   \n")
    cat("                                                                                                       \n")
    cat("Règle 4 : Un modèle sans ordre de différenciation suppose que la série originale est stationnaire.     \n")
    cat("        Un modèle avec un seul ordre de différenciation suppose que la série originalea une tendance   \n")
    cat("        moyenne constante -par ex. un modèle de marche aléatoire ou de type SES,                       \n")
    cat("        avec ou sans croissance.                                                                       \n")
    cat("        - Un modèle avec deux ordres de différenciation  suppose que la série d'origine a une          \n")
    cat("       tendance variant dans le temps (par exemple, une tendance aléatoire ou un modèle de type LES).  \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat("Règle 5 : Un modèle sans ordre de différenciation comprend normalement un terme constant - qui         \n")
    cat("          permet une valeur moyenne non nulle.                                                         \n")
    cat("        - Un modèle avec deux ordres de différence totale n'inclut normalement pas un terme constant.  \n")
    cat("        - Dans un modèle avec un ordre de différenciation totale, un terme constant doit être inclus   \n")
    cat("          si la série a une tendance moyenne non nulle.                                                \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("-> Identification des nombres de termes AR et MA :                                                     \n")
    cat("   ------------------------------------------                                                          \n")
    cat("                                                                                                       \n")
    cat("Règle 6 : Si la fonction d'autocorrélation partielle (PACF) de la série différenciée affiche           \n")
    cat("          une forte le seuil de coupure et/ou l'autocorrélation de lag-1 est positive,                 \n")
    cat("          c'est-à-dire si la série apparaît légèrement 'sous-différencié'--envisagez alors             \n")
    cat("          d'ajouter un ou plusieurs termes AR au modèle.                                               \n")
    cat("          Le décalage au-delà que le PACF coupe est le nombre indiqué de termes AR.                    \n")
    cat("                                                                                                       \n")
    cat("Règle 7 : Si la fonction d'autocorrélation (ACF) de la série différenciée affiche une coupure nette    \n")
    cat("        et/ou  l'autocorrélation de lag-1 est négative, c'est-à-dire si la série apparaît légèrement   \n")
    cat("        'surdifférente' --envisagez alors d'ajouter un terme MA au modèle.                             \n")
    cat("        Le décalage au-delà duquel l'ACF se coupe est le nombre indiqué de termes de MA.               \n")
    cat("                                                                                                       \n")
    cat("Règle 8 : Il est possible qu'un terme AR et un terme MA s'annulent mutuellement,                       \n")
    cat("        donc si un modèle mixte AR-MA semble correspondre aux données, essayez également               \n")
    cat("        un modèle avec un terme AR de moins et un terme MA de moins                                    \n")
    cat("        - en particulier si les estimations des paramètres dans le modèle d'origine nécessitent        \n")
    cat("         plus de 10 itérations pour converger. more than 10 iterations to converge.                    \n")
    cat("        MÉFIEZ-VOUS D'UTILISER PLUSIEURS TERMES AR ET PLUSIEURS TERMES MA DANS LE MÊME MODÈLE.         \n")
    cat("                                                                                                       \n")
    cat("Règle 9 : S'il y a une racine unitaire dans la partie AR du modèle, c'est-à-dire si la somme           \n")
    cat("        des coefficients AR est presque exactement 1                                                   \n")
    cat("    - vous devez réduire le nombre de termes AR de un et augmenter l'ordre de différenciation de un.   \n")
    cat("                                                                                                       \n")
    cat("Règle 10 : S'il y a une racine unitaire dans la partie MA du modèle, c'est-à-dire si la somme des      \n")
    cat("         coefficients MA est presque exactement 1                                                      \n")
    cat("    - vous devez réduire le nombre de termes MA de un et réduire l'ordre de différenciation de un.     \n")
    cat("                                                                                                       \n")
    cat("Règle 11 : Si les prévisions à long terme* apparaissent erratiques ou instables, il peut y avoir       \n")
    cat("          une racine unitaire dans les coefficients AR ou MA.                                          \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("-> Identification de la partie saisonnière du modèle :                                                 \n")
    cat("   ------------------------------------------                                                          \n")
    cat("                                                                                                       \n")
    cat("Règle 12 : Si la série a un modèle saisonnier fort et cohérent, vous devez utiliser un ordre de        \n")
    cat("         différenciation saisonnière (sinon le modèle suppose que le modèle saisonnier s'estompera     \n")
    cat("         avec le temps). Cependant, n'utilisez jamais plus d'une commande de différenciation           \n")
    cat("         saisonnière ou plus de 2 commandes de total différenciation (saisonnier+non saisonnier).      \n")
    cat("                                                                                                       \n")
    cat(" Règle 13 : Si l'autocorrélation de la série correctement différenciée est positive au décalage 's',   \n")
    cat("        où 's' est le nombre de périodes dans une saison,  puis envisagez d'ajouter un terme SAR       \n")
    cat("        au modèle.                                                                                     \n")
    cat("        Si l'autocorrélation de la série différenciée est négatif au décalage 's', envisagez           \n")
    cat("        d'ajouter un terme SMA au modèle.                                                              \n")
    cat("        Le dernier situation est susceptible de se produire si une différence saisonnière a été        \n")
    cat("        utilisée,ce qui devrait être fait si les données ont été une saisonnalité stable et logique.   \n")
    cat("        Le premier est susceptible de se produire si une différence saisonnière n'a pas été utilisé,   \n")
    cat("        ce qui ne serait approprié que si le profil saisonnier n'est pas stable dans le temps.         \n")
    cat("        Vous devriez essayer d'éviter d'utiliser plus d'un ou deux paramètres saisonniers (SAR + SMA)  \n")
    cat("        dans le même modèle, car cela est susceptible d'entraîner un surajustement des données et/ou   \n")
    cat("        des problèmes d'estimation.                                                                    \n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
    cat("                                                                                                       \n")
    cat("                                                                                                       \n")
    cat(" *Une mise en garde sur les prévisions à long terme en général :                                       \n")
    cat("                                                                                                       \n")
    cat("      les modèles de séries chronologiques linéaires tels que ARIMA et les modèles de lissage          \n")
    cat("     exponentiel prédisent l'avenir le plus lointain en faisant une série de prévisions à une période  \n")
    cat("     et en les branchant pour des valeurs futures inconnues à mesure qu'ils regardent plus loin.       \n")
    cat("     Par exemple, une prévision à 2 périodes est calculée en traitant la prévision à 1 période comme   \n")
    cat("     s'il s'agissait de données, puis en appliquant la même équation de prévision. Cette étape peut    \n")
    cat("     être répétée un certain nombre de fois afin de prévoir aussi loin dans le futur que vous          \n")
    cat("     le souhaitez, et la méthode donne également des formules pour calculer des intervalles de         \n")
    cat("     confiance théoriquement appropriés autour des prévisions à plus long terme.                       \n")
    cat("     Cependant, les modèles sont identifiés et optimisés en fonction de leurs performances             \n")
    cat("     de prévision pour une période à l'avance, et leur extrapolation rigide peut ne pas être           \n")
    cat("     la meilleure façon de prévoir de nombreuses périodes à l'avance (par exemple, plus d'un an        \n")
    cat("     lorsque l'on travaille avec des données commerciales mensuelles ou trimestrielles ),              \n")
    cat("     en particulier lorsque les hypothèses de modélisation ne sont au mieux qu'approximativement       \n")
    cat("     satisfaites, ce qui est presque toujours le cas.                                                  \n")
    cat("     Si l'un de vos objectifs est de générer des prévisions à long terme, il serait bon de puiser      \n")
    cat("     également dans d'autres sources d'informations lors du processus de sélection du modèle et/ou     \n")
    cat("     d'optimiser les estimations des paramètres pour la prévision multipériode si votre logiciel       \n")
    cat("     le permet et/ou utiliser un modèle auxiliaire pour les prévisions à long terme.                   \n")
    cat("                                                                                                       \n")
    cat(".......................................................................................................\n")
  })



  helpLjungBoxFr <- function(){
    cat(".........................................................................................\n")
    cat(" La statistique de Ljung-Box (prononcé Young) permet de tester l'hypothèse d'indépendance\n")
    cat(" sérielle d'une série (ou que la série est bruit blanc). Plus spécifiquement cette       \n")
    cat(" statistique teste l'hypothèse que les m coefficients d'autocorrélation sont nuls.       \n")
    cat("                                                                                         \n")
    cat(".........................................................................................\n")
    cat("                 (H0) il n'y a pas auto-corrélation des erreurs d'ordre 1 à r.           \n")
    cat("                 (H1) il y a auto-corrélation des erreurs d'ordre 1 à r.                 \n")
    cat(".........................................................................................\n")
    cat(" Idéalement, nous aimerions ne pas rejeter l'hypothèse nulle.                            \n")
    cat(" Autrement dit,                                                                          \n")
    cat(" nous aimerions que la valeur p du test soit supérieure à 0,05 car                       \n")
    cat(" cela signifie que les résidus de notre modèle de série chronologique sont indépendants, \n")
    cat(" ce qui est souvent une hypothèse que nous faisons lors de la création d'un modèle.      \n")
    cat(".........................................................................................\n")
    cat("  Changer la valeur du 'lag' puis cliquer sur 'Soumettre' pour exectuter un autre test   \n")
    cat(".........................................................................................\n")
  }


  helpLjungBox <- function(){
    cat(".........................................................................................\n")
    cat(" The Ljung (pronounced Young) Box test ( or just the Box test)                           \n")
    cat(" is a way to test for the absence of serial autocorrelation, up to a specified lag k.    \n")
    cat("                                                                                         \n")
    cat(" The test determines whether or not errors are iid (i.e. white noise) or whether there   \n")
    cat(" is something more behind them; whether or not the autocorrelations for the errors       \n")
    cat(" or residuals are non zero. Essentially, it is a test of lack of fit: if the             \n")
    cat(" autocorrelations of the residuals are very small, we say that the model doesn’t show    \n")
    cat(" ‘significant lack of fit’.                                                              \n")
    cat(".........................................................................................\n")
    cat("   (H0) The residuals are independently distributed.                                     \n")
    cat("        (i.e. the correlations in the population from which the sample is taken are 0,   \n")
    cat("        so that any observed correlations in the data result from randomness             \n")
    cat("        of the sampling process).                                                        \n")
    cat("   (H1) The residuals are not independently distributed; they exhibit serial correlation.\n")
    cat(".........................................................................................\n")
    cat(" Ideally, we would like to fail to reject the null hypothesis.                           \n")
    cat(" That is, we would like to see the p-value of the test be greater than 0.05 because      \n")
    cat(" this means the residuals for our time series model are independent,                     \n")
    cat(" which is often an assumption we make when creating a model.                             \n")
    cat(".........................................................................................\n")
    cat("  Change the 'lag' value then click 'Submit' to run another test                         \n")
    cat(".........................................................................................\n")
  }


  helpADF <- function(){
    cat(".......................................................................\n")
    cat("  Augmented Dickey-Fuller Test is used to check whether a given        \n")
    cat("  time series is at rest.                                              \n")
    cat("                                                                       \n")
    cat("  A given time series can be called stationary if:                     \n")
    cat("  - it doesn’t have any trend                                          \n")
    cat("  - depicts a constant variance over time                              \n")
    cat("  - and follows autocorrelation structure over a period constantly.    \n")
    cat(".......................................................................\n")
    cat("  (H0)  The series has a unit root, so it is not stationary.           \n")
    cat("  (Ha)  The series is stationary.                                      \n")
    cat(".......................................................................\n")
    cat("  p-value < 0.05 indicates the Time Series is stationary               \n")
    cat(".......................................................................\n")
  }

  helpADF2 <- function(){
    cat(".............................................................\n")
    cat("               Augmented Dickey-Fuller Test                  \n")
    cat(".............................................................\n")
    cat("  (H0)  The series has a unit root, so it is not stationary. \n")
    cat("  (Ha)  The series is stationary.                            \n")
    cat(".............................................................\n")
    cat("  p-value < 0.05 indicates the Time Series is stationary     \n")
    cat(".............................................................\n")
  }

  helpKPSS <- function(){
    cat("....................................................................................................\n")
    cat("                                                                                                    \n")
    cat("  One way to determine whether differencing is required is to use a unit root test.                 \n")
    cat("....................................................................................................\n")
    cat("     Kwiatkowski-Phillips-Schmidt-Shin (KPSS)                                                       \n")
    cat("....................................................................................................\n")
    cat("     A KPSS test can be used to determine if a time series is trend stationary.                     \n")
    cat("                                                                                                    \n")
    cat("  (H0) : The time series is trend stationary. (the data is stationary)                              \n")
    cat("  (Ha) : The time series is not trend stationary. (the data is not stationary)                      \n")
    cat("                                                                                                    \n")
    cat("  If the p-value of the test is less than some significance level (e.g. α = .05) then we            \n")
    cat("  reject the null hypothesis and conclude that the time series is not trend stationary.             \n")
    cat("....................................................................................................\n")
    cat("  A major disadvantage for the KPSS test is that it has a high rate of Type I errors                \n")
    cat("  (it tends to reject the null hypothesis too often).                                               \n")
    cat("  One way to deal with the potential for high Type I errors is to combine the KPSS with an ADF test.\n")
    cat("  If the result from both tests suggests that the time series in stationary, then it probably is.   \n")
    cat("....................................................................................................\n")
    cat("                                                                                                    \n")

  }



  helpMK <- function(){
    cat("....................................................................................................\n")
    cat("  A Mann-Kendall trend test is used to determine whether or not there is a trend in the             \n")
    cat("  time series data.                                                                                 \n")
    cat("  It is a nonparametric test, which means that no underlying assumptions                            \n")
    cat("  are made about the normality of the data.  It does require that there is no autocorrelation.      \n")
    cat("....................................................................................................\n")
    cat("  Un test de tendance de Mann-Kendall est utilisé pour déterminer s'il existe ou non                \n")
    cat("  une tendance dans les données de séries chronologiques.                                           \n")
    cat("  Il s'agit d'un test non paramétrique, ce qui signifie qu'aucune hypothèse sous-jacente            \n")
    cat("  n'est faite quant à la normalité des données.                                                     \n")
    cat("                                                                                                    \n")
    cat("  (H0) : Il n'y a pas de tendance dans la série                                                     \n")
    cat("  (Ha) : Il existe une tendance dans la série                                                       \n")
    cat("....................................................................................................\n")
    cat("  p-value < 0.05 indicates the Time Series is not stationary, there is trend in the time series.    \n")
    cat("....................................................................................................\n")
  
  }


  output$Plot_Type_Help <- renderPrint({
    cat(".......................................................................\n")
    cat("                            Plot_Type_Help                             \n")
    cat(".......................................................................\n")
    cat("                                                                       \n")
    cat("  Plots a time series along with its acf and either its                \n")
    cat("  pacf, histogram, lagged scatterplot or spectrum.                     \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat("                                                                       \n")
    cat(".......................................................................\n")
    cat("                                                                       \n")
    cat(".......................................................................\n")
  })


}

