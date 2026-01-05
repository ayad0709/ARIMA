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
      head(df, 100) 
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


  # getMyData <- function(tsData, frequency, islog, d_n, DS_n) {
  #   # Ensure tsData is not NULL
  #   req(tsData)
  # 
  #   # Convert frequency to numeric
  #   frequency <- as.numeric(frequency)
  # 
  #   if (islog == "Yes") {
  #     if (d_n == 0 && DS_n == 0) {
  #       myData <- log(tsData)
  #     } else {
  #       if (d_n == 0 && DS_n > 0) {
  #         myData <- diff(log(tsData), DS_n * frequency)
  #       } else {
  #         if (d_n > 0 && DS_n == 0) {
  #           myData <- diff(log(tsData), difference = d_n)
  #         } else {
  #           myData <- diff(diff(log(tsData), DS_n * frequency), difference = d_n)
  #         }
  #       }
  #     }
  #   } else {
  #     if (d_n == 0 && DS_n == 0) {
  #       myData <- tsData
  #     } else {
  #       if (d_n == 0 && DS_n > 0) {
  #         myData <- diff(tsData, DS_n * frequency)
  #       } else {
  #         if (d_n > 0 && DS_n == 0) {
  #           myData <- diff(tsData, difference = d_n)
  #         } else {
  #           myData <- diff(diff(tsData, DS_n * frequency), difference = d_n)
  #         }
  #       }
  #     }
  #   }
  #   return(myData)
  # }


  
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

  # ########  ########  ########  ########  ########  ########  ########  ########
  # ########  ########  ########  ########  ########  ########  ########  ########
  # #
  # #      Statistics output for selected column
  # #
  # ########  ########  ########  ########  ########  ########  ########  ########
  # ########  ########  ########  ########  ########  ########  ########  ########
  # 
  # 
  #   #  descr()  stats Row format
  #   output$data_StatisticsText1 <- renderPrint({
  #   req(input$fileData)
  #   req(input$colNum)
  # 
  #   df <- data()
  #   data_Statistics <- df[[input$colNum]]
  # 
  #   # Ensure the selected column data is numeric
  #   if(is.numeric(data_Statistics)) {
  #     descr(data_Statistics)
  #   } else {
  #     "Selected column data is not numeric"
  #   }
  # })
  # 
  # #  descr()  stats in a table format
  # output$data_StatisticsText1_Table <- renderTable({
  #   req(input$fileData)
  #   req(input$colNum)
  # 
  #   df <- data()
  #   data_Statistics <- df[[input$colNum]]
  # 
  #   # Ensure the selected column data is numeric
  #   if(is.numeric(data_Statistics)) {
  #     descr_Statistics <- descr(data_Statistics)
  #     descr_DataFrame <- as.data.frame(descr_Statistics)
  # 
  #     descr_Table <- data.frame(date = row.names(descr_DataFrame), descr_DataFrame)
  # 
  #     # Specify the custom column names here in the vector
  #     # Make sure the number of names matches the number of columns in descr_Table
  #     my_column_names <- c("Statistics", "Value")
  # 
  #     # Set the column names
  #     colnames(descr_Table) <- my_column_names
  # 
  #     descr_Table
  # 
  #   } else {
  #     "Selected column data is not numeric"
  #   }
  # })
  # 
  # 
  # ###############################################################
  # 
  # 
  # output$data_StatisticsText2 <- renderPrint({
  #   req(input$fileData)
  #   req(input$colNum)
  # 
  #   df <- data()
  #   colData <- df[[input$colNum]]
  # 
  #   # Ensure the selected column data is numeric
  #   if(is.numeric(colData)) {
  #     #stat.desc(colData )
  # 
  #     # Compute descriptive statistics
  #     stats_output <- stat.desc(colData)
  # 
  #     # Extract the statistics names and values
  #     stat_names <- names(stats_output)
  #     stat_values <- unname(stats_output)
  # 
  #     # Print the formatted output
  #     cat("Descriptive Statistics \n")
  #     cat("data_Statistics   \n")
  #     cat(" \n")
  #     cat(" \n")
  #     cat("         data_Statistics\n")
  #     cat("-------  -----------------\n")
  #     for (i in seq_along(stat_names)) {
  #       cat(sprintf("%-10s %s\n", stat_names[i], stat_values[i]))
  #     }
  # 
  # 
  #   } else {
  #     "Selected column data is not numeric"
  #   }
  # })
  # 
  # 
  # 
  # 
  # # Output for the forecasting model based on the user's selection
  # output$modelOutput <- renderPrint({
  #   req(input$Model)  # Ensure 'Select the Model' input is provided
  #   col_data <- selected_column_data() # The column data as vector
  #   frequency <- 1 # Placeholder for frequency, adjust as needed
  #   ts_data <- ts(col_data, frequency = frequency) # Convert to ts
  # 
  #   # Fit the model based on selection
  #   model_selected <- input$Model
  #   fit <- switch(model_selected,
  #                 "ARIMA" = auto.arima(ts_data),
  #                 "Holt-Winters Additive" = HoltWinters(ts_data, seasonal = "additive"),
  #                 "Holt-Winters Multiplicative" = HoltWinters(ts_data, seasonal = "multiplicative"),
  #                 stop("Invalid model selection.")
  #   )
  #   # Output the summary or forecast
  #   summary(fit)  # Or use forecast::forecast(fit) if we want to forecast
  # })


  
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
  
  # 3. Detailed Statistics (pastecs::stat.desc)
  output$data_StatisticsText2 <- renderPrint({
    req(selected_column_data())
    
    # Compute descriptive statistics using pastecs
    stats_output <- pastecs::stat.desc(selected_column_data(), norm = TRUE)
    
    cat("Detailed Descriptive Statistics\n")
    cat("-------------------------------\n")
    
    # Format as a named list for cleaner print output
    print(as.data.frame(stats_output))
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
  
  
  
  
  # ########  ########  ########  ########  ########  ########  ########  ########
  # ########  ########  ########  ########  ########  ########  ########  ########
  # #
  # #       t(s) plot + ACF + PACF
  # #
  # ########  ########  ########  ########  ########  ########  ########  ########
  # ########  ########  ########  ########  ########  ########  ########  ########
  # 
  # output$tsPlot <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  # 
  #   plot(tsData(),main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel, type = 'l',lwd = 2)
  # })
  # 
  # 
  # output$StACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  # 
  #   plot(Acf(tsData()), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$StPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  # 
  #   plot(Pacf(tsData()), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$StACFPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  # 
  #   acf2(tsData(), lwd = 3,main = userData$mainTitle)
  # })
  # 
  # 
  # output$tsDisplay2 <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  # 
  #   ggtsdisplay(tsData(), plot.type = input$plot_type , main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
  # })
  # 
  # 
  # output$teststationariteSt <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   helpADF()
  # 
  #   adf.test(tsData(), alternative =input$alternSt, k=input$LagOrderADFSt)
  # })
  # 
  # output$teststationariteSt <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   helpADF()
  # 
  #   adf.test(tsData(), alternative =input$alternSt, k=input$LagOrderADFSt)
  # })



  ######## ######## ######## ######## ######## ######## ######## ########
  #
  #           Time Series Plots, ACF/PACF & Stationarity Tests
  #
  ######## ######## ######## ######## ######## ######## ######## ########
  

  
  # # 1. Main Time Series Plot [S(t)]
  # output$tsPlot <- renderPlot({
  #   req(tsData())
  #   
  #   autoplot(tsData(), size = 1, colour = "steelblue") +
  #     labs(
  #       title = userData$mainTitle, 
  #       x = userData$xLabel, 
  #       y = userData$yLabel
  #     ) +
  #     apply_user_theme()
  # })
  
  
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
    req(tsData(), input$alternSt, input$LagOrderADFSt)
    
    cat("--- Augmented Dickey-Fuller (ADF) Test Results ---\n\n")
    
    # Safe execution to prevent crash on invalid lag orders
    tryCatch({
      res <- tseries::adf.test(
        tsData(), 
        alternative = input$alternSt, 
        k = as.numeric(input$LagOrderADFSt)
      )
      print(res)
      
      cat("\n-------------------------------------------------\n")
      cat("Decision: ")
      if(res$p.value < 0.05) {
        cat("Reject H0. The series is stationary (p < 0.05).\n")
      } else {
        cat("Fail to reject H0. The series is non-stationary.\n")
      }
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      cat("Suggestion: Try reducing the 'Lag' value.")
    })
  })
  
  
  
  
  
  ########  ########  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########  ########  ########
  #
  #       log(St)  plot + ACF + PACF
  #
  ########  ########  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########  ########  ########
  
  
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
    req(logTsData(), input$alternLogSt, input$LagOrderADFLogSt)
    
    cat("--- ADF Test on Log-Transformed Series ---\n\n")
    
    tryCatch({
      res <- tseries::adf.test(
        logTsData(), 
        alternative = input$alternLogSt, 
        k = as.numeric(input$LagOrderADFLogSt)
      )
      print(res)
      
      cat("\nInterpretation: ")
      if(res$p.value < 0.05) {
        cat("Stationary after log-transform (p < 0.05).")
      } else {
        cat("Non-stationary. Consider differencing (d=1).")
      }
    }, error = function(e) {
      cat("Execution Error: ", e$message)
    })
  })
  

  # output$plotLogSt <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   log_st <- log(tsData())
  # 
  #   plot(log_st, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel, type = 'l', lwd = 2)
  # })
  # 
  # 
  # output$logStACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   log_st <- log(tsData())
  # 
  #   plot(Acf(log_st), lwd = 2, main=userData$mainTitle)
  # })
  # 
  # 
  # output$logStPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   log_st <- log(tsData())
  # 
  #   plot(Pacf(log_st), lwd = 2, main=userData$mainTitle)
  # })
  # 
  # 
  # output$logStACFPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   log_st <- log(tsData())
  # 
  #   acf2(log_st, lwd = 3, main=userData$mainTitle)
  # })
  # 
  # 
  # output$log_ts_Display <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   log_st <- log(tsData())
  # 
  #   ggtsdisplay(log_st, plot.type = input$plot_type , main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
  # })
  # 
  # 
  # output$teststationariteLogSt <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   log_st <- log(tsData())
  #   helpADF()
  # 
  #   adf.test(log_st, alternative =input$alternLogSt, k=input$LagOrderADFLogSt)
  # })




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
    req(diff1TsData(), input$alternd1St, input$LagOrderADFd1St)
    
    cat("--- ADF Test on First Differenced Series (d=1) ---\n\n")
    
    tryCatch({
      res <- tseries::adf.test(
        diff1TsData(), 
        alternative = input$alternd1St, 
        k = as.numeric(input$LagOrderADFd1St)
      )
      print(res)
      
      cat("\nInterpretation: ")
      if(res$p.value < 0.05) {
        cat("Stationary (p < 0.05). First-order differencing was sufficient.")
      } else {
        cat("Non-stationary. Consider a second difference (d=2) or seasonal differencing (D=1).")
      }
    }, error = function(e) {
      cat("Execution Error: ", e$message)
    })
  })

  # output$difference1 <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_St <- diff(tsData())
  # 
  #   plot(d1_St, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel, type = 'l', lwd = 2)
  # })
  # 
  # 
  # output$d1StACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_St <- diff(tsData())
  # 
  #   plot(Acf(d1_St), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$d1StPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_St <- diff(tsData())
  # 
  #   plot(Pacf(d1_St), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$d1StACFPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_St <- diff(tsData())
  # 
  #   acf2(d1_St, lwd = 3, main = userData$mainTitle)
  # })
  # 
  # 
  # output$d1_ts_Display <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_St <- diff(tsData())
  # 
  #   ggtsdisplay(d1_St, plot.type = input$plot_type , main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
  # })
  # 
  # 
  # output$teststationarited1St <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_St <- diff(tsData())
  #   helpADF()
  # 
  #   adf.test(d1_St, alternative =input$alternd1St, k=input$LagOrderADFd1St)
  # })


  

  
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
    req(diffSeasonTsData(), input$alternDs1St, input$LagOrderADFDs1St)
    
    cat("--- ADF Test on Seasonal Differenced Series (D=1) ---\n\n")
    
    tryCatch({
      res <- tseries::adf.test(
        diffSeasonTsData(), 
        alternative = input$alternDs1St, 
        k = as.numeric(input$LagOrderADFDs1St)
      )
      print(res)
      
      cat("\nInterpretation: ")
      if(res$p.value < 0.05) {
        cat("Stationary (p < 0.05). Seasonal differencing removed the seasonal unit root.")
      } else {
        cat("Non-stationary. You may also need an ordinary difference (d=1).")
      }
    }, error = function(e) {
      cat("Execution Error: ", e$message)
    })
  })
  

  # output$DS1Stplot <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_St <- diff(tsData(), frequency)
  # 
  #   plot(D1_St, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel, type = 'l', lwd = 2)
  # })
  # 
  # 
  # output$DS1StACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_St <- diff(tsData(), frequency)
  # 
  #   plot(Acf(D1_St), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$DS1StPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_St <- diff(tsData(), frequency)
  # 
  #   plot(Pacf(D1_St), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$DS1StACFPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_St <- diff(tsData(), frequency)
  # 
  #   acf2(D1_St, lwd = 3, main = userData$mainTitle)
  # })
  # 
  # 
  # output$Ds1_ts_Display <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_St <- diff(tsData(), frequency)
  # 
  #   ggtsdisplay(D1_St, plot.type = input$plot_type, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
  # })
  # 
  # 
  # output$teststationariteDs1St <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_St <- diff(tsData(), frequency)
  #   helpADF()
  # 
  #   adf.test(D1_St, alternative =input$alternDs1St, k=input$LagOrderADFDs1St)
  # })



  
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
    req(diff1LogTsData(), input$alternd1LogSt, input$LagOrderADFd1LogSt)
    
    cat("--- ADF Test: First Difference of Log Series ---\n\n")
    
    tryCatch({
      res <- tseries::adf.test(
        diff1LogTsData(), 
        alternative = input$alternd1LogSt, 
        k = as.numeric(input$LagOrderADFd1LogSt)
      )
      print(res)
      
      cat("\nInterpretation: ")
      if(res$p.value < 0.05) {
        cat("Stationary (p < 0.05). Variance and trend stabilized.")
      } else {
        cat("Non-stationary. Consider further differencing or seasonal analysis.")
      }
    }, error = function(e) {
      cat("Execution Error: ", e$message)
    })
  })
  
  # output$plotd1Log <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_log_st <- diff(log(tsData()))
  #   
  #   plot(d1_log_st, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel, type = 'l', lwd = 2)
  # })
  # 
  # 
  # output$d1LogStACFa <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_log_st <- diff(log(tsData()))
  #   
  #   plot(Acf(d1_log_st), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$d1LogStPACFa <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_log_st <- diff(log(tsData()))
  #   
  #   plot(Pacf(d1_log_st), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$d1LogStACFPACFa <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_log_st <- diff(log(tsData()))
  #   
  #   acf2(d1_log_st, lwd = 3, main = userData$mainTitle)
  # })
  # 
  # 
  # output$d1_log_ts_Display <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_log_st <- diff(log(tsData()))
  #   
  #   ggtsdisplay(d1_log_st, plot.type = input$plot_type, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
  # })
  # 
  # 
  # output$teststationarited1LogSt <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   d1_log_st <- diff(log(tsData()))
  #   helpADF()
  #   
  #   adf.test(d1_log_st, alternative =input$alternd1LogSt, k=input$LagOrderADFd1LogSt)
  # })
  
  


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
    req(diffSeasonLogTsData(), input$alternDs1LogSt, input$LagOrderADFDs1LogSt)
    
    cat("--- ADF Test: Seasonal Difference of Log Series ---\n\n")
    
    tryCatch({
      res <- tseries::adf.test(
        diffSeasonLogTsData(), 
        alternative = input$alternDs1LogSt, 
        k = as.numeric(input$LagOrderADFDs1LogSt)
      )
      print(res)
      
      cat("\nInterpretation: ")
      if(res$p.value < 0.05) {
        cat("Stationary (p < 0.05). Both variance and seasonality appear stabilized.")
      } else {
        cat("Non-stationary. Consider adding an ordinary difference (d=1).")
      }
    }, error = function(e) {
      cat("Execution Error: ", e$message)
    })
  })

  # output$Dlogplot <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_log_St <- diff(log(tsData()), frequency)
  # 
  #   plot(D1_log_St, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel, type = 'l', lwd = 2)
  # })
  # 
  # 
  # output$DlogplotACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_log_St <- diff(log(tsData()), frequency)
  # 
  #   plot(Acf(D1_log_St), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # output$DlogplotPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_log_St <- diff(log(tsData()), frequency)
  # 
  #   plot(Pacf(D1_log_St), lwd = 2, main = userData$mainTitle)
  # })
  # 
  # 
  # output$DlogplotACFPACF <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_log_St <- diff(log(tsData()), frequency)
  # 
  #   acf2(D1_log_St , lwd = 3, main = userData$mainTitle)
  # })
  # 
  # 
  # output$Ds1_log_ts_Display <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_log_St <- diff(log(tsData()), frequency)
  # 
  #   ggtsdisplay(D1_log_St , plot.type = input$plot_type, main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
  # })
  # 
  # 
  # output$teststationariteDs1LogSt <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   frequency = as.numeric(currentFrequency())
  #   D1_log_St <- diff(log(tsData()), frequency)
  #   helpADF()
  # 
  #   adf.test(D1_log_St, alternative =input$alternDs1LogSt, k=input$LagOrderADFDs1LogSt)
  # })




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
    req(diff1LogSeasonTsData(), input$alternd1Ds1LogSt, input$LagOrderADFd1Ds1LogSt)
    
    cat("--- ADF Test: Double Difference of Log (d=1, D=1) ---\n\n")
    
    tryCatch({
      res <- tseries::adf.test(
        diff1LogSeasonTsData(), 
        alternative = input$alternd1Ds1LogSt, 
        k = as.numeric(input$LagOrderADFd1Ds1LogSt)
      )
      print(res)
      
      cat("\nInterpretation: ")
      if(res$p.value < 0.05) {
        cat("Stationary (p < 0.05). This is usually the final step before model fitting.")
      } else {
        cat("Non-stationary. Check if the series is over-differenced or needs a different transformation.")
      }
    }, error = function(e) {
      cat("Execution Error: ", e$message)
    })
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
  
  # 6. Choice Stationarity Test with Interpretation
  output$teststationarited2St <- renderPrint({
    req(myData_Choice(), input$alternd2St, input$LagOrderADFd2St)
    
    cat("--- ADF Test: Custom Transformation ---\n\n")
    helpADF2()
    
    tryCatch({
      res <- tseries::adf.test(
        myData_Choice(), 
        alternative = input$alternd2St, 
        k = as.numeric(input$LagOrderADFd2St)
      )
      print(res)
      
      cat("\nInterpretation: ")
      if(res$p.value < 0.05) {
        cat("Stationary (p-value =", round(res$p.value, 4), "< 0.05). \n")
        cat("   - The null hypothesis of a unit root is rejected. \n")
        cat("   - The current combination of transformations is sufficient. \n \n")
      } else {
        cat("Non-stationary (p-value =", round(res$p.value, 4), ">= 0.05). \n")
        cat("   - The null hypothesis cannot be rejected. \n")
        cat("   - You may need more differencing or a different transformation. \n \n")
      }
    }, error = function(e) {
      cat("Error: ", e$message)
    })
  })
  
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


  output$tsDisplay <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    ggtsdisplay(tsData(),plot.type = input$plot_type , main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
  })

  ########  ######## 
  # Plot the box plot
  ########  ######## 
  output$boxP <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    # Create a box plot with the data across cycles of the time series
    boxplot(as.numeric(tsData()) ~ cycle(tsData()), xlab = "Cycle", ylab = "Data", main = "Box Plot by Cycle")
  })
  
  # 1. The Dynamic UI Wrapper (Handles Width & Height)
  output$boxP_UI <- renderUI({
    #plotly::plotlyOutput("boxP2",width = userData$plotWidth, height = userData$plotHeight)
    plotly::plotlyOutput("boxP2",width = userData$plotWidth, height = userData$plotHeight)
  })
  
  # 2. The Plotly Render Function (Uses built-in TSstudio logic)
  output$boxP2 <- plotly::renderPlotly({
    req(tsData()) # Ensure data is loaded
    
    # TSstudio built-in seasonal boxplot is natively interactive (Plotly based)
    p <- TSstudio::ts_seasonal(tsData(), type = "box")
    
    # Return the plotly object to the UI
    p 
  })

  ########  ######## 
  # Plot the time series data
  ########  ######## 
  output$SubSeriesPlot <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    ggsubseriesplot(tsData())
  })

  
  # 1. The Dynamic UI Wrapper (Handles Width & Height)
  output$SubSeriesPlot_UI <- renderUI({
    plotly::plotlyOutput("SubSeriesPlot2", width = userData$plotWidth, height = userData$plotHeight)
  })
  
  # 2. The Plotly Render Function
  output$SubSeriesPlot2 <- plotly::renderPlotly({
    req(tsData()) # Ensure data is loaded
    
    # Create the ggplot object using forecast::ggsubseriesplot
    p <- ggsubseriesplot(tsData()) + 
      ggtitle("Seasonal Sub-series Plot") +
      theme_minimal()
    
    # Convert the ggplot object to an interactive Plotly object
    plotly::ggplotly(p)
  })
  
  ########  ######## 
  # Plot the seasonal
  ########  ######## 
  output$SeasonPlot <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    ggseasonplot(tsData())
  })

  # 1. The Dynamic UI Wrapper (Handles Width & Height)
  output$SeasonPlot_UI <- renderUI({
    plotly::plotlyOutput("SeasonPlot2", width = userData$plotWidth, height = userData$plotHeight)
  })
  
  # 2. The Plotly Render Function
  output$SeasonPlot2 <- plotly::renderPlotly({
    req(tsData()) # Ensure data is loaded
    
    # TSstudio built-in seasonal plot is natively interactive (Plotly based)
    # 'normal' type creates the classic overlapping seasonal lines
    p <- TSstudio::ts_seasonal(tsData(), type = "normal")
    
    # Return the plotly object
    p 
  })
  
  
  ########  ######## 
  # Plot the Polar
  ########  ######## 
  output$SeasonPlotPolar <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    ggseasonplot(tsData(), polar=TRUE)
  })
  
  
  ########  ######## 
  # Plot the lag plot
  ########  ######## 
  output$lagPlot <- renderPlot({
    req(tsData()) # Ensure tsData is not NULL
    gglagplot(tsData())
  })
  
  
  output$lagPlot_UI <- renderUI({
    req(userData$plotWidth, userData$plotHeight)
    plotly::plotlyOutput("lagPlot_Plotly", width = userData$plotWidth, height = userData$plotHeight)
  })
  
  # 2. The Plotly Render Function
  output$lagPlot_Plotly <- plotly::renderPlotly({
    req(tsData()) # Ensure data is loaded
    
    # Create the ggplot object using forecast::gglagplot
    # Note: we use suppressWarnings to avoid the 'fortify' message
    p <- suppressWarnings(gglagplot(tsData())) + 
      theme_minimal() +
      labs(title = paste("Lag Plots:", userData$mainTitle))
    
    # Convert the ggplot object to an interactive Plotly object
    # tooltip = "all" allows you to see the values for both axes on hover
    plotly::ggplotly(p)
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
  
  output$allPlot <- plotly::renderPlotly({
    req(tsData())
    
    # ts_seasonal with type="all" creates a 4-pane interactive dashboard:
    # Seasonal, Normal, Cycle, and Boxplot views combined.
    p <- TSstudio::ts_seasonal(tsData(), type = "all")
    
    # Just return the object p. Do NOT use print().
    p
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

  # output$decompose <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   myData <- tsData()
  # 
  #   decompose_time_Series <- decompose(myData,input$model1)
  #   plot(decompose_time_Series, lwd = 2)
  # })


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
  
  # output$dFactors <- renderPrint({
  #   req(tsData(), input$model1)
  #   
  #   myData %>% decompose(type=input$model1) -> fit
  #   cat(".......................................................\n")
  #   cat("\n")
  #   cat("            Coefficients saisonnier                    ")
  #   cat("\n")
  #   cat("\n")
  #   cat(".......................................................\n")
  #   cat("\n")
  #   cat("\n")
  # 
  #   cat(round(fit$figure, 2))
  #   # cat(format(fit$figure, nsmall = 2))
  # 
  #   cat("\n")
  #   cat("\n")
  # 
  # })

  
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
  

  # output$X11decompose <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   myData <- tsData()
  # 
  #   myData %>% seas(x11="") -> fit
  #   autoplot(fit) +
  #     ggtitle("X11 decomposition")
  # })
  # 
  # 
  # 
  # output$SEATSdecompose <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   myData <- tsData()
  # 
  #   myData %>% seas() %>%
  #     autoplot() +
  #     ggtitle("SEATS decomposition")
  # })
  # 
  # 
  # output$SEATSFactors <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   myData <- tsData()
  # 
  #   myData %>% seas() -> fit
  #   summary(fit)
  # })
  # 
  # output$X11Factors <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   myData <- tsData()
  # 
  #   myData %>% seas(x11="") -> fit
  #   summary(fit)
  # })
  # 
  # output$STLdecompose <- renderPlot({
  #   req(tsData()) # Ensure tsData is not NULL
  #   myData <- tsData()
  # 
  #   myData %>% stl(t.window=13, s.window="periodic", robust=TRUE) %>% autoplot()
  # })
  # 
  # 
  # output$STLFactors <- renderPrint({
  #   req(tsData()) # Ensure tsData is not NULL
  #   myData <- tsData()
  # 
  #   myData %>% stl(t.window=13, s.window="periodic", robust=TRUE) -> fit
  #   fit
  # })


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
  # output$autoForecast <- renderPrint({
  #   req(results())
  #   summary(results()$modelOutput)
  # })
  
  
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


# ########  ########  ########  ########  ########  ########  ########  ##########
# ########  ########  ########  ########  ########  ########  ########  ##########
# #
# #                      Auto ARIMA   &    H.W.
# #
# ########  ########  ########  ########  ########  ########  ########  ##########
# ########  ########  ########  ########  ########  ########  ########  ##########
# 
# 
#   # Cache for forecast results
#   forecastCache <- reactiveValues()
# 
# 
# 
#   # Function to create a unique key based on input parameters
#   createCacheKey <- function(file, col, model,  freq1) {
#     # Check if the file input is valid
#     if (is.null(file) || !is.data.frame(file) || nrow(file) == 0  ) {
#       return(NULL)
#     }
#     # Use the file name and last modification time to create a unique key
#     fileName <- file$name
#     fileModTime <- file$datapath  # or use file$size for an additional layer of uniqueness
#     # Ensure other inputs are non-NULL and convert them to strings
#     if (is.null(col) || is.null(model) ) {
#       return(NULL)
#     }
#     paste0(fileName, "_", fileModTime, "_", as.character(col), "_", as.character(model), "_",  as.character(freq1) )
#   }
# 
# 
# 
# 
#   # Reactive expression to compute or fetch the forecast
#   results <- reactive({
#     key <- createCacheKey(input$fileData, input$colNum,  input$Model,  currentFrequency())
#     if (!is.null(forecastCache[[key]])) {
#       forecastCache[[key]]
#     } else {
#       result <- modelisation(input$Model , input$length)
#       forecastCache[[key]] <- result
#       result
#     }
#   })
# 
# 
# ####################################################################################
# #
# #   modeling , the result is accessed via : results()$modelOutput
# #
# ####################################################################################
# 
#   modelisation <- function(  modelType,  forecastLength  ) {
# 
#     req(tsData())
# 
#     # the time series data
#     timeSeriesData <- tsData()
#     if (is.null(timeSeriesData)) {
#       stop("Error in processing time series data")
#     }
# 
#     # Choose the model based on input
#     fittedModel <- switch(modelType,
#                           "ARIMA" = auto.arima(timeSeriesData, trace = TRUE, allowdrift = TRUE),
#                           "Holt-Winters Additive" = hw(timeSeriesData, "additive", h = forecastLength)$model,
#                           "Holt-Winters Multiplicative" = hw(timeSeriesData, "multiplicative", h = forecastLength)$model,
#                           holt(timeSeriesData, h = forecastLength)$model)  # Default case
# 
#     # Forecasting to delete keep just fittedModel
#     forecastedValues <- forecast(fittedModel, level = c(80, 95), h = forecastLength)
#     plotForecast <- plot(forecastedValues, lwd = 2)
#     forecastDataFrame <- as.data.frame(forecastedValues)
#     forecastTable <- data.frame(date = row.names(forecastDataFrame), forecastDataFrame)
# 
#     modelResiduals <- fittedModel$resid
#     modelResidualsDataFrame <- as.data.frame(modelResiduals)
# 
# 
#     # Return a list of outputs
#     list(
#       modelOutput = fittedModel,
#       plot = plotForecast,
#       forecast = forecastedValues,
#       forecastTable = forecastTable,
#       modelResiduals = modelResidualsDataFrame
#     )
#   }
# 
# ####################################################################################
# ####################################################################################
# 
# 
#   # Render function to display ARIMA model summary
#   output$autoForecast <- renderPrint({
#     results()$modelOutput
#     })
# 
# 
#   output$autoForecast_plot <- renderPlot({
#     forecastedValues <- forecast(results()$modelOutput, level = c(80, 95), h = input$length)
#     plot(forecastedValues, lwd = 2)
#   })
# 
#   output$results_forecast <- renderPrint({
#     forecastedValues <- forecast(results()$modelOutput, level = c(80, 95), h = input$length)
#     forecastedValues
#   })
# 
#   output$results_forecastTable <- renderTable({
#     forecastedValues <- forecast(results()$modelOutput, level = c(80, 95), h = input$length)
#     forecastDataFrame <- as.data.frame(forecastedValues)
#     forecastTable <- data.frame(date = row.names(forecastDataFrame), forecastDataFrame)
#     forecastTable
#   })
# 
# 
# 
#   # Render function to display extracted ARIMA parameters
#   output$arimaParams <- renderPrint({
#     # get the model
#     fittedModel <- results()$modelOutput
#     # Extract ARIMA parameters
#     bm_p <- fittedModel$arma[1]
#     bm_d <- fittedModel$arma[6]
#     bm_q <- fittedModel$arma[2]
#     bm_P <- fittedModel$arma[3]
#     bm_D <- fittedModel$arma[7]
#     bm_Q <- fittedModel$arma[4]
#     bm_s <- fittedModel$arma[5]
#     bm_AICc <- fittedModel$aicc
# 
#     cat("ARIMA Parameters:\n",
#         "p:", bm_p, "\n",
#         "d:", bm_d, "\n",
#         "q:", bm_q, "\n",
#         "P:", bm_P, "\n",
#         "D:", bm_D, "\n",
#         "Q:", bm_Q, "\n",
#         "S:", bm_s, "\n",
#         "AICc:", bm_AICc, "\n")
#   })
# 
# 
# 
#   ########  ########  ########  ########  ########  ########  ########  ########
#   #
#   #             Output the SARIMA Coefficient
#   #
#   ########  ########  ########  ########  ########  ########  ########  ########
# 
# 
#   # Render the model details
#   output$modelDetails <- renderPrint({
# 
#     fittedModel <- results()$modelOutput
# 
#     # Extracting the components
#     arimaOrder <- fittedModel$arma
# 
#     # AR components
#     arOrder <- arimaOrder[1]
#     arParameters <- fittedModel$coef[1:arOrder]
# 
#     # MA components
#     maOrder <- arimaOrder[6]
#     maParameters <- fittedModel$coef[(arOrder + 1):(arOrder + maOrder)]
# 
#     # Seasonal components
#     sarOrder <- arimaOrder[2]
#     sarParameters <- fittedModel$coef[(arOrder + maOrder + 1):(arOrder + maOrder + sarOrder)]
#     smaOrder <- arimaOrder[7]
#     smaParameters <- fittedModel$coef[(arOrder + maOrder + sarOrder + 1):(arOrder + maOrder + sarOrder + smaOrder)]
# 
#     # Drift component
#     drift <- ifelse(fittedModel$arma[9] == 1, fittedModel$coef[length(fittedModel$coef)], "No drift")
# 
#     # Print the components
#     list(
#       AR_Order = arOrder,
#       AR_Parameters = arParameters,
#       MA_Order = maOrder,
#       MA_Parameters = maParameters,
#       SAR_Order = sarOrder,
#       SAR_Parameters = sarParameters,
#       SMA_Order = smaOrder,
#       SMA_Parameters = smaParameters,
#       Drift = drift
#     )
#   })
# 


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

  # ########  ########  ########  ########  ########  ########  ########  ########
  # #
  # #      Auto-Forecast             -  Tests   -
  # #
  # ########  ########  ########  ########  ########  ########  ########  ########
  # 
  # output$testTrendMK <- renderPrint({
  #   myData <- tsData()
  #   helpMK()
  # 
  #   MannKendall(myData)
  # })
  # 
  # 
  # output$test_ADF <- renderPrint({
  #   myData <- tsData()
  #   helpADF()
  # 
  #   adf.test(myData, alternative =input$altern, k=input$LagOrderADF)
  # })
  # 
  # 
  # output$test_KPSS <- renderPrint({
  #   myData <- tsData()
  #   helpKPSS()
  # 
  #   kpss.test(myData, null = "Trend")
  # })
  # 
  # 
  # output$testLBn <- renderPrint({
  #   # tsData <- tsData()
  #   fittedModel <- results()$modelOutput
  #   model_Residuals <- fittedModel$resid
  # 
  #   helpLjungBox()
  # 
  #   Box.test(model_Residuals, lag=input$lagorder, type=input$typeBoxTest)
  # 
  # })


  
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
  # output$tsdiag <- renderPlot({
  #   req(results()$modelOutput)
  #   # Note: ggtsdiag provides a cleaner ggplot version of diagnostics
  #   forecast::ggtsdiag(results()$modelOutput) 
  # }, 
  # 
  # )
  
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
  # output$unitCercle <- renderPlot({
  #   req(results()$modelOutput)
  #   fit <- results()$modelOutput
  #   
  #   # autoplot on an Arima object displays the unit circle roots
  #   autoplot(fit) + 
  #     labs(title = "Inverse AR/MA Roots", subtitle = "Roots must lie inside the unit circle for stability/invertibility") +
  #     apply_user_theme()
  # }, 
  # width = function() getPlotDim(userData$plotWidth),
  # height = function() getPlotDim(userData$plotHeight)
  # )
  
 
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
  
  

  
  
  
  # ########  ########  ########  ########  ########  ########  ########  ########
  # #
  # #   Auto-Forecast  -  Residuals Panels   ,  ACF  ,  PACF  ,  Unit Circle
  # #
  # ########  ########  ########  ########  ########  ########  ########  ########
  # 
  # 
  # output$chkRes <- renderPlot({
  #   fittedModel <- results()$modelOutput
  #   myData <- fittedModel
  #   checkresiduals(myData)
  # })
  # 
  # 
  # output$tsdiag <- renderPlot({
  #   fittedModel <- results()$modelOutput
  #   myData <- fittedModel
  #   ggtsdiag(myData)
  # })
  # 
  # 
  # output$plotACFRes <- renderPlot({
  #   fittedModel <- results()$modelOutput
  #   model_Residuals <- fittedModel$resid
  #   plot(Acf(model_Residuals), lwd = 2)
  # })
  # 
  # 
  # output$plotPACFRes <- renderPlot({
  #   fittedModel <- results()$modelOutput
  #   model_Residuals <- fittedModel$resid
  #   plot(Pacf(model_Residuals), lwd = 2)
  # })
  # 
  # 
  # output$plot_ACF_PACF_Res <- renderPlot({
  #   fittedModel <- results()$modelOutput
  #   model_Residuals <- fittedModel$resid
  #   acf2(model_Residuals, lwd = 3,main = userData$mainTitle)
  # })
  # 
  # 
  # output$unitCercle <- renderPlot({
  #   fittedModel <- results()$modelOutput
  # 
  #   plot(fittedModel)
  # 
  # })



  
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
    
  
  # output$Pslow <- renderPrint({
  #   req(tsData())
  #   
  #   # 1. Show Modal with an Animated 'Indeterminate' Progress Bar
  #   showModal(modalDialog(
  #     title = "Advanced Exhaustive Search in Progress",
  #     tags$div(
  #       style = "text-align: center;",
  #       tags$p("The algorithm is testing hundreds of ARIMA combinations using exact likelihood."),
  #       tags$p("This ensures the mathematically optimal model is found. Please wait..."),
  #       tags$div(class = "progress progress-striped active",
  #                tags$div(class = "progress-bar progress-bar-primary", 
  #                         style = "width: 100%;"))
  #     ),
  #     footer = NULL, 
  #     easyClose = FALSE
  #   ))
  #   
  #   # 2. Start Computation Logic
  #   result_text <- tryCatch({
  #     
  #     # 2a. Anomaly Detection
  #     outliers <- forecast::tsoutliers(tsData())
  #     
  #     # 2b. The Exhaustive Search (stepwise=FALSE, approx=FALSE)
  #     # We capture the trace to show the "Competition" between models
  #     trace_log <- capture.output({
  #       sarima_model <- forecast::auto.arima(
  #         tsData(),
  #         max.p = as.numeric(input$maxp), max.d = as.numeric(input$maxd), max.q = as.numeric(input$maxq),
  #         max.P = as.numeric(input$maxPs), max.D = as.numeric(input$maxDs), max.Q = as.numeric(input$maxQs),
  #         max.order = as.numeric(input$maxorder),
  #         stepwise = FALSE,      # Check all combinations
  #         approximation = FALSE, # Use exact MLE for precision
  #         trace = TRUE,          # Show the work
  #         allowdrift = TRUE
  #       )
  #     })
  #     
  #     # 2c. Statistical Validation Calculations
  #     resids <- residuals(sarima_model)
  #     lb_test <- Box.test(resids, lag = 24, type = "Ljung-Box")
  #     jb_test <- tseries::jarque.bera.test(resids)
  #     
  #     # 2d. Backtesting (Time Series Cross-Validation)
  #     cv_error <- tryCatch({
  #       mean(abs(forecast::tsCV(tsData(), forecast::Arima, 
  #                               order=sarima_model$arma[c(1,6,2)], 
  #                               seasonal=sarima_model$arma[c(3,7,4)])))
  #     }, error = function(e) return(NA))
  #     
  #     # 3. Construct the Unified Professional Report
  #     output_content <- capture.output({
  #       cat("================================================================\n")
  #       cat("                 I. DATA QUALITY & OUTLIER SCAN                 \n")
  #       cat("================================================================\n")
  #       if(length(outliers$index) > 0) {
  #         cat("ANOMALIES DETECTED: Significant outliers found at indices:", outliers$index, ".\n")
  #         cat("Interpretation: These points represent shocks or errors that deviate from\n")
  #         cat("the normal trend. The model has attempted to stay robust despite these points.\n")
  #       } else { 
  #         cat("The data quality check is complete: No significant outliers were detected.\n") 
  #       }
  #       
  #       cat("\n================================================================\n")
  #       cat("                 II. EXHAUSTIVE SEARCH HISTORY                  \n")
  #       cat("   [ Candidate Model ]                 [ Selection Metric (AICs) ]  \n")
  #       cat("================================================================\n")
  #       cat(paste(trace_log, collapse = "\n"))
  #       
  #       cat("\n\n================================================================\n")
  #       cat("                 III. MODEL SELECTION LOGIC                     \n")
  #       cat("================================================================\n")
  #       cat("BEST MODEL IDENTIFIED: ", forecast:::arima.string(sarima_model), "\n\n")
  #       cat("Why is this the optimal model?\n")
  #       cat("1. AICc Minimization: This model yielded the lowest Corrected Akaike\n")
  #       cat("   Information Criterion (AICc) among all candidates in the trace above.\n")
  #       cat("2. Information Balance: It provides the best fit to the historical data\n")
  #       cat("   while remaining simple enough to avoid 'overfitting' (Parsimony).\n")
  #       cat("3. Convergence: The model successfully reached a global maximum for\n")
  #       cat("   the log-likelihood function using exact MLE methods.\n")
  #       
  #       cat("\n================================================================\n")
  #       cat("                 IV. ESTIMATED MODEL COEFFICIENTS               \n")
  #       cat("================================================================\n")
  #       print(sarima_model$coef)
  #       cat("\nInterpretation: These coefficients represent the mathematical weights of the\n")
  #       cat("Auto-Regressive (AR) and Moving Average (MA) lags. If a coefficient is close\n")
  #       cat("to zero, that specific lag has minimal predictive power for the series.\n")
  #       
  #       cat("\n================================================================\n")
  #       cat("                 V. GOODNESS-OF-FIT METRICS                     \n")
  #       cat("================================================================\n")
  #       cat("Sigma^2 (Residual Variance): ", round(sarima_model$sigma2, 4), "\n")
  #       cat("Log Likelihood:             ", round(sarima_model$loglik, 2), "\n")
  #       cat("AIC (Akaike):               ", round(sarima_model$aic, 2), "\n")
  #       cat("AICc (AIC Corrected):       ", round(sarima_model$aicc, 2), "\n")
  #       cat("BIC (Bayesian):             ", round(sarima_model$bic, 2), "\n")
  #       
  #       cat("\nInterpretation:\n")
  #       cat("- Sigma^2: Measures the variance of the errors; lower is more precise.\n")
  #       cat("- AIC/BIC: Information criteria used to compare models. Smaller values\n")
  #       cat("           indicate a better balance between accuracy and simplicity.\n")
  #       
  #       cat("\n================================================================\n")
  #       cat("                 VI. STATISTICAL VALIDATION TESTS               \n")
  #       cat("================================================================\n")
  #       
  #       # Ljung-Box Analysis
  #       cat("[1] LJUNG-BOX TEST FOR INDEPENDENCE\n")
  #       cat("    Purpose: To confirm the residuals (errors) are purely random.\n")
  #       cat("    H0 (Null): Residuals are White Noise (No remaining patterns).\n")
  #       cat("    Result: The test returned a p-value of", round(lb_test$p.value, 4), ".\n")
  #       if(lb_test$p.value > 0.05) {
  #         cat("    Decision: We fail to reject the null hypothesis. The residuals are\n")
  #         cat("              independent; the model has successfully captured all data structure.\n")
  #       } else {
  #         cat("    Decision: We reject the null hypothesis. Autocorrelation remains in the\n")
  #         cat("              residuals, meaning the model is missing key information.\n")
  #       }
  #       
  #       # Jarque-Bera Analysis
  #       cat("\n[2] JARQUE-BERA TEST FOR NORMALITY\n")
  #       cat("    Purpose: To check if the error distribution is symmetric (Bell Curve).\n")
  #       cat("    H0 (Null): Residuals follow a Normal Distribution.\n")
  #       cat("    Result: The test returned a p-value of", round(jb_test$p.value, 4), ".\n")
  #       if(jb_test$p.value > 0.05) {
  #         cat("    Decision: We fail to reject the null hypothesis. Residuals are normal,\n")
  #         cat("              validating the reliability of our forecast confidence intervals.\n")
  #       } else {
  #         cat("    Decision: We reject the null hypothesis. Residuals are non-normal;\n")
  #         cat("              prediction intervals may be biased or overly optimistic.\n")
  #       }
  #       
  #       cat("\n================================================================\n")
  #       cat("                 VII. PREDICTIVE ACCURACY (BACKTEST)            \n")
  #       cat("================================================================\n")
  #       if(!is.na(cv_error)) {
  #         cat("Mean Absolute Error (CV): ", round(cv_error, 4), "\n")
  #         cat("Interpretation: This error rate was calculated via rolling-window cross-validation,\n")
  #         cat("                simulating how the model performs on 'unseen' future data.\n")
  #       }
  #       
  #       cat("\n================================================================\n")
  #       cat("                 VIII. FINAL MODEL VERDICT                      \n")
  #       cat("================================================================\n")
  #       if(lb_test$p.value > 0.05 && jb_test$p.value > 0.05) {
  #         cat("FINAL VERDICT: Statistically Excellent. The model is fully validated\n")
  #         cat("and ready for production-level forecasting.\n")
  #       } else if (lb_test$p.value > 0.05) {
  #         cat("FINAL VERDICT: Statistically Acceptable. All information is captured,\n")
  #         cat("               though the error distribution is non-standard.\n")
  #       } else {
  #         cat("FINAL VERDICT: Statistically Sub-optimal. High risk of forecast error\n")
  #         cat("               due to remaining patterns in the residuals.\n \n \n")
  #       }
  #     })
  #     
  #     removeModal() 
  #     output_content
  #     
  #   }, error = function(e) {
  #     removeModal()
  #     return(paste("Critical error during analysis:", e$message))
  #   })
  #   
  #   # Print the final result to the renderPrint output
  #   cat(paste(result_text, collapse = "\n"))
  # })
  
  # output$Pslow <- renderPrint({
  #   req(tsData())
  #   
  #   # Use Shiny progress notification since stepwise=FALSE can take significant time
  #   withProgress(message = 'Performing Exhaustive Search...', 
  #                detail = 'This may take a few minutes for complex data...', value = 0, {
  #                  
  #                  # Capture the trace output to display it in the UI
  #                  trace_output <- capture.output({
  #                    sarima_model <- forecast::auto.arima(
  #                      tsData(),
  #                      max.p = as.numeric(input$maxp),
  #                      max.d = as.numeric(input$maxd),
  #                      max.q = as.numeric(input$maxq),
  #                      max.P = as.numeric(input$maxPs),
  #                      max.D = as.numeric(input$maxDs),
  #                      max.Q = as.numeric(input$maxQs),
  #                      max.order = as.numeric(input$maxorder),
  #                      stepwise = FALSE,         # Exhaustive search
  #                      approximation = FALSE,    # Use exact likelihood
  #                      trace = TRUE,             # Capture search steps
  #                      allowdrift = TRUE,
  #                      test = "kpss"             # Standard unit root test
  #                    )
  #                  })
  #                  
  #                  setProgress(1) # Complete progress
  #                  
  #                  # Print the trace first, then the final model summary
  #                  cat("--- SEARCH TRACE (Exhaustive) ---\n")
  #                  cat(paste(trace_output, collapse = "\n"))
  #                  cat("\n\n--- FINAL OPTIMAL MODEL ---\n")
  #                  print(sarima_model)
  #                  cat("\n")
  #                  summary(sarima_model)
  #                })
  # })
  
  # ########  ########  ########  ########  ########  ########  ########  ########
  # ########  ########  ########  ########  ########  ########  ########  ########
  # #
  # #                     Slow   ARIMA
  # #
  # ########  ########  ########  ########  ########  ########  ########  ########
  # ########  ########  ########  ########  ########  ########  ########  ########
  # 
  # output$Pslow <- renderPrint({
  #     myData <- tsData()
  # 
  #     sarima_model <- auto.arima(myData,
  #                             max.p = input$maxp,
  #                             max.d = input$maxd,
  #                             max.q = input$maxq,
  #                             max.P = input$maxPs,
  #                             max.D = input$maxDs,
  #                             max.Q = input$maxQs,
  #                             max.order = input$maxorder,
  #                             stepwise=FALSE,
  #                             approximation=FALSE,
  #                             trace=TRUE,
  #                             allowdrift=TRUE,
  #                             test = c("kpss", "adf", "pp")
  #     )
  # 
  #     sarima_model
  #   })


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
  

 #  ########  ########  ########  ########  ########  ########  ########  ########
 #  ########  ########  ########  ########  ########  ########  ########  ########
 #  #
 #  #            ARIMA (p, d, q) (P, D, Q) [S]
 #  #
 #  ########  ########  ########  ########  ########  ########  ########  ########
 #  ########  ########  ########  ########  ########  ########  ########  ########
 # 
 # 
 #  # Cache for forecast results
 #  forecastCache_autoARIMA <- reactiveValues()
 # 
 #  # Function to create a unique key based on input parameters
 #  createCacheKey_autoARIMA <- function(file, freq, model, col,  v_p, v_d, v_q, vP, vD, VQ, drift ) {
 #    # Check if the file input is valid
 #    if (is.null(file) || !is.data.frame(file) || nrow(file) == 0  ) {
 #      return(NULL)
 #    }
 #    # Use the file name and last modification time to create a unique key
 #    fileName <- file$name
 #    fileModTime <- file$datapath  # or use file$size for an additional layer of uniqueness
 #    # Ensure other inputs are non-NULL and convert them to strings
 #    if (is.null(col) || is.null(model)) {
 #      return(NULL)
 #    }
 # 
 # paste0(fileName, "_", fileModTime, "_",
 #        as.character(col), "_", as.character(model),
 #        "_", as.character(freq), "_", as.character(v_p),
 #        as.character(v_d), as.character(v_q), as.character(vP),
 #        as.character(vD), as.character(VQ), as.character(drift))  }
 # 
 # 
 #  # Reactive expression to compute or fetch the forecast
 #  results_ARIMA_pdPD_drift <- reactive({
 #    key <- createCacheKey_autoARIMA(input$fileData, currentFrequency() , input$Model,  input$colNum, input$ARIMAp, input$ARIMAd, input$ARIMAq, input$ARIMAps, input$ARIMAds, input$ARIMAqs, input$driftYN )
 #    if (!is.null(forecastCache_autoARIMA[[key]])) {
 #      forecastCache_autoARIMA[[key]]
 #    } else {
 #      result <- modelisation_Auto_SARIMA(input$Model , input$length)
 #      forecastCache_autoARIMA[[key]] <- result
 #      result
 #    }
 #  })
 # 
 # 
 # 
 #  modelisation_Auto_SARIMA <- function(  modelType,  forecastLength  ) {
 # 
 #    req(tsData())
 # 
 #    # the time series data
 #    timeSeriesData <- tsData()
 #    if (is.null(timeSeriesData)) {
 #      stop("Error in processing time series data")
 #    }
 # 
 #    if (input$driftYN == "TRUE") {
 #      driftConsideration =TRUE
 #    }
 #    else {
 #      driftConsideration =FALSE
 #    }
 # 
 #    myData <- tsData()
 # 
 #    fittedModel<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)
 # 
 #    list(
 #      modelOutput = fittedModel
 #    )
 #  }
 # 
 # 
 # 
 # 




  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################


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


  output$model_ARIMApdq_p_values <- renderPrint({
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

    cat("............................................................................\n")
    cat("                     Testing the coefficients values                        \n")
    cat("............................................................................\n")
    cat(" H0 : the coefficient = 0                                                   \n")
    cat(" Ha : the coefficient is different from 0                                   \n")
    cat("............................................................................\n")
    cat(" p-value < 0.05 indicates that the corresponding coefficient is             \n")
    cat("                significantly different from 0                              \n")
    cat("............................................................................\n")
    coeftest(sarima_model)
  })


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


  output$timeSeriesPlot_SARIMA <- renderPlot({
    req(tsData())
    ts_data <- tsData()  #  time series data
    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput   #  SARIMA model
    forecasted_Data <- forecast(sarima_model,h=input$length)


    fitted_values <- fitted(sarima_model)


    #Plot the time series data
    plot(forecasted_Data, type = "l", lwd = 2, col = "red4", main = userData$mainTitle, xlab = userData$xLabel, ylab = userData$yLabel)
    lines(fitted_values, col = "firebrick3", type = "l", lwd = 2)
    # Add the fitted values from the SARIMA model
    # fitted_values <- fitted(sarima_model)
    lines(ts_data, col = "black", type = "l", lwd = 2)  # 'type = "p"' plots the fitted values as points

    # plot(sarima_model)
  })



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

  output$testTrendMK2 <- renderPrint({
    req(tsData())
    myData <- tsData()

    helpMK()

    MannKendall(myData)
  })


  output$teststationariteARIMApdq <- renderPrint({
    req(tsData())
    myData <- tsData()

    helpADF()

    adf.test(myData, alternative =input$altern2, k=input$LagOrderADF2)
  })


  output$kpssTest2 <- renderPrint({
    req(tsData())
    myData <- tsData()

    helpKPSS()

    kpss.test(myData, null = "Trend")
  })


  output$test_DFGLS <- renderPrint({
    req(tsData())
    myData <- tsData()

    cat(".........................................................................................\n")
    cat("     DF-GLS Unit Root Test                                                               \n")
    cat(".........................................................................................\n")
    cat("                                                                                         \n")

    summary(ur.ers(myData,  model = "trend",lag.max = 4))
  })



  output$testLBnARIMApdq <- renderPrint({
    req(tsData())


    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # sarima_model <- Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

    helpLjungBox()

    myDataResiduals <- sarima_model$resid

    Box.test(myDataResiduals, lag=input$lagorder1, type=input$typeBoxTest1)

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


  output$tsdiag2 <- renderPlot({
    req(tsData())

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)

     qqnorm(resid(sarima_model), main = "Normal Q-Q Plot, Residual", col = "darkgrey")
    qqline(resid(sarima_model), col = "dodgerblue", lwd = 2)

  })


  output$ShapiroTest <- renderPrint({
    req(tsData())
    myData <- tsData()

    if (input$driftYN == "TRUE") {
      driftConsideration =TRUE
    }
    else {
      driftConsideration =FALSE
    }

    sarima_model <- results_ARIMA_pdPD_drift()$modelOutput
    # fit<-Arima(myData, order=c(input$ARIMAp,input$ARIMAd,input$ARIMAq),seasonal = c(input$ARIMAps,input$ARIMAds,input$ARIMAqs), include.drift = driftConsideration)


    cat("..........................................................................\n")
    cat(" The Shapiro-Wilk test is a statistical test used to check if             \n")
    cat(" a continuous variable follows a normal distribution.                     \n")
    cat("..........................................................................\n")
    cat(" (H0) states that the variable is normally distributed.                   \n")
    cat(" (H1) states that the variable is NOT normally distributed.               \n")
    cat("..........................................................................\n")
    cat("Decision Rule:                                                            \n")
    cat(" If p ≤ 0.05: Reject the null hypothesis.                                 \n")
    cat("              (i.e. the data is NOT normally distributed).                \n")
    cat(" If p > 0.05: Fail to reject the null hypothesis.                         \n")
    cat("              (i.e. the data MAY BE normally distributed).                \n")
    cat("..........................................................................\n")

    ResudialData = resid(sarima_model)
    shapiro.test(ResudialData)
  })

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



    # req(input$fileData)
    # req(input$colNum)
    # req(currentFrequency())
    # req(input$dateCol)
    # req(input$Model)
    #
    #
    # df <- read_excel(input$fileData$datapath)
    # date_col <- as.Date(df[[input$dateCol]])
    # starting_date <- min(date_col, na.rm = TRUE)
    #
    # df <- data()
    #
    # df[[input$dateCol]] <- as.Date(df[[input$dateCol]])
    # #df[[input$dateCol]] <- format(as.Date(df[[input$dateCol]]), "%d/%m/%Y")
    #
    # date_col <- df[[input$dateCol]]
    #
    # starting_date <- min(date_col, na.rm = TRUE)
    #
    # start_day <- day(starting_date)
    # start_month <- month(starting_date)
    # start_year <- year(starting_date)
    #
    # model <- input$Model

    # tsData()

    # cat(model)


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
    cat("               Augmented Dickey-Fuller Test                            \n")
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
    cat("                                                                                                    \n")
    cat("  (H0) : There is no trend in the series                                                            \n")
    cat("  (Ha) : There is a trend in the series                                                             \n")
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
    cat("                                                                                                    \n")
    cat("     Mann-Kendall trend Test                                                                        \n")
    cat("                                                                                                    \n")


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

