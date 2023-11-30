################################################################################
### LIBRARIES ####
################################################################################

packages = c("shiny", "shinythemes", "data.table", "ggplot2", "lubridate", 
             "urca", "summarytools", "dplyr", "fpp2", "forecast", "stats", 
             "Kendall", "lmtest", "vtable","shinyalert", "mathjaxr", "psych",
             "tseries", "seasonal", "xts", "astsa", "ggfortify",  "pastecs",
             "tsibble", "feasts", "readxl", "TSstudio", "latex2exp", "Hmisc", 
             "foreign","shinyWidgets","hrbrthemes", "shinyjs")  

################################################################################
# Now load or install & load LIBRARIES
################################################################################

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)



################################################################################
#                             Main
################################################################################

shinyUI(
  fluidPage(

    tags$head(
      tags$script(type = "text/javascript", 
                  src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
    ),
    
    # # Include JavaScript for custom binding
    # tags$head(
    #   tags$script(HTML("
    #   Shiny.inputBindings.register({
    #     find: function(scope) {
    #       return $(scope).find('.color-input');
    #     },
    #     getValue: function(el) {
    #       return $(el).val();
    #     },
    #     subscribe: function(el, callback) {
    #       $(el).on('input.colorInputBinding', function(e) {
    #         callback();
    #       });
    #     },
    #     unsubscribe: function(el) {
    #       $(el).off('.colorInputBinding');
    #     }
    #   });
    # "))
    # ),
    
################################################################################
#                             Theme
################################################################################
  theme = shinytheme("spacelab"),  
  #
  # Replace " ******* " with your preferred theme. Valid themes are:
  #
  #     cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, 
  #     readable, sandstone, simplex, slate, spacelab, superhero, united, yeti
  #       
  
  #useShinyalert(force = TRUE),


  ##############################################################################

  # Include custom CSS
  tags$head(
    tags$style(HTML("
              .custom-label {
                  color: red; /* Change this to your desired color */
              }
          "))
  ),


  ##############################################################################

  titlePanel("SARIMA & H.W."),
  
  useShinyjs(),  # Initialize shinyjs
  
  sidebarLayout(
    
  ##############################################################################
  #        Side Panel
  ############################################################################## 
    
    sidebarPanel(width=2,
      fileInput("fileData", "Choose File (xlsx, csv, txt ,sav)", accept = c(".xlsx", ".csv", ".txt", ".sav")),
      uiOutput("dateColUI"),
      uiOutput("colNumUI"),
      uiOutput("monthUI"),
      uiOutput("frequencyInputUI"),
      uiOutput("customInput"),
      uiOutput("modelSelectUI"),
      uiOutput("lengthInputUI"),
      textOutput("forecastModel"),
      uiOutput("graphTypeUI"),
      br(),
      uiOutput("conditionalButtons"),
    ),
    
    
  ##############################################################################
  #        Right Panels
  ##############################################################################   
    
    mainPanel(
      tabsetPanel(
        
        
        tabPanel("Stats.",
                 tabsetPanel(
                   tabPanel("Data", tableOutput("dataPrint")),
                   tabPanel("Statistics 1", tableOutput("data_StatisticsText1_Table")),
                   tabPanel("Statistics 2", verbatimTextOutput("data_StatisticsText2")),
                 )),
        
        tabPanel("Time.Series",
                 tabsetPanel(
                   tabPanel("S(t)",
                            tabsetPanel(
                              tabPanel("St", plotOutput("tsPlot",width=900,height = 630 )),
                              tabPanel("ACF", plotOutput("StACF",width=800,height = 500)),
                              tabPanel("PACF", plotOutput("StPACF",width=800,height = 500)),
                              tabPanel("ACF + PACF", plotOutput("StACFPACF",width=620,height = 570)),
                              tabPanel("Ts Display", plotOutput("tsDisplay2",width=900,height = 630 )),
                              tabPanel("stationary [ADF]", 
                                       sidebarLayout(
                                         sidebarPanel(width=3,
                                            selectInput("alternSt", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                            numericInput("LagOrderADFSt", label = "Lag",  min=0, value=12),
                                         ),
                                         tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationariteSt")),
                                       )),
                            )),
                   
                   
                   tabPanel("log(St)",
                            tabsetPanel(
                              tabPanel("log(St)",plotOutput("plotLogSt",width=900,height = 630)),                              
                              tabPanel("ACF", plotOutput("logStACF",width=800,height = 500)),
                              tabPanel("PACF", plotOutput("logStPACF",width=800,height = 500)),
                              tabPanel("ACF + PACF", plotOutput("logStACFPACF",width=620,height = 570)),
                              tabPanel("Ts Display", plotOutput("log_ts_Display",width=900,height = 630 )),
                              tabPanel("stationary [ADF]", 
                                       sidebarLayout(
                                         sidebarPanel(width=3,
                                            selectInput("alternLogSt", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                            numericInput("LagOrderADFLogSt", label = "Lag", min=0, value=12),
                                         ),
                                         tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationariteLogSt")),
                                       )),
                              )),
                   
                   
                   tabPanel("d1(St)",
                            tabsetPanel(
                              tabPanel("d[1](St)",plotOutput("difference1",width=900,height = 630)),
                              tabPanel("ACF", plotOutput("d1StACF",width=800,height = 500)),
                              tabPanel("PACF", plotOutput("d1StPACF",width=800,height = 500)),
                              tabPanel("ACF + PACF", plotOutput("d1StACFPACF",width=620,height = 570)),
                              tabPanel("Ts Display", plotOutput("d1_ts_Display",width=900,height = 630 )),
                              tabPanel("stationary [ADF]", 
                                       sidebarLayout(
                                         sidebarPanel(width=3,
                                            selectInput("alternd1St", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                            numericInput("LagOrderADFd1St", label = "Lag",  min=0, value=12),
                                         ),
                                         tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationarited1St")),
                                       )),
                            )),
                   
                   
                   tabPanel("D1(St)",
                            tabsetPanel(
                              tabPanel("D[1](St)",plotOutput("DS1Stplot",width=900,height = 630)),
                              tabPanel("ACF", plotOutput("DS1StACF",width=800,height = 500)),
                              tabPanel("PACF", plotOutput("DS1StPACF",width=800,height = 500)),
                              tabPanel("ACF + PACF", plotOutput("DS1StACFPACF",width=620,height = 570)),
                              tabPanel("Ts Display", plotOutput("Ds1_ts_Display",width=900,height = 630 )),
                              tabPanel("stationary [ADF]", 
                                       sidebarLayout(
                                         sidebarPanel(width=3,
                                            selectInput("alternDs1St", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                            numericInput("LagOrderADFDs1St", label = "Lag",  min=0, value=12),
                                         ),
                                         tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationariteDs1St")),
                                       )),
                            )),
                   
                   
                   tabPanel("d1(log(St))", 
                            tabsetPanel(
                              tabPanel("d[1](log(St))",plotOutput("plotd1Log",width=900,height = 630)),
                              tabPanel("ACF", plotOutput("d1LogStACFa",width=800,height = 500)),
                              tabPanel("PACF", plotOutput("d1LogStPACFa",width=800,height = 500)),
                              tabPanel("ACF + PACF", plotOutput("d1LogStACFPACFa",width=620,height = 570)),
                              tabPanel("Ts Display", plotOutput("d1_log_ts_Display",width=900,height = 630 )),
                              tabPanel("stationary [ADF]", 
                                       sidebarLayout(
                                         sidebarPanel(width=3,
                                                      selectInput("alternd1LogSt", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                                      numericInput("LagOrderADFd1LogSt", label = "Lag",  min=0, value=12),
                                         ),
                                         tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationarited1LogSt")),
                                       )),
                            )), 
                   
                   
                   tabPanel("D1(log(St))", 
                            tabsetPanel(
                              tabPanel("D[1](log(St))",plotOutput("Dlogplot",width=900,height = 630)),
                              tabPanel("ACF", plotOutput("DlogplotACF",width=800,height = 500)),
                              tabPanel("PACF", plotOutput("DlogplotPACF",width=800,height = 500)),
                              tabPanel("ACF + PACF", plotOutput("DlogplotACFPACF",width=620,height = 570)),
                              tabPanel("Ts Display", plotOutput("Ds1_log_ts_Display",width=900,height = 630 )),
                              tabPanel("stationary [ADF]", 
                                       sidebarLayout(
                                         sidebarPanel(width=3,
                                            selectInput("alternDs1LogSt", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                            numericInput("LagOrderADFDs1LogSt", label = "Lag",  min=0, value=12),
                                         ),
                                         tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationariteDs1LogSt")),
                                       )),
                            )), 
                   

                   tabPanel("d1D1(log(St))", 
                            tabsetPanel(
                              tabPanel("d[1](D[1](log(St)))",plotOutput("dDlogplot",width=900,height = 630)),
                              tabPanel("ACF", plotOutput("dDlogplotACF",width=800,height = 500)),
                              tabPanel("PACF", plotOutput("dDlogplotPACF",width=800,height = 500)),
                              tabPanel("ACF + PACF", plotOutput("dDlogplotACFPACF",width=620,height = 570)),
                              tabPanel("Ts Display", plotOutput("d1_Ds1_log_ts_Display",width=900,height = 630 )),
                              tabPanel("stationary [ADF]", 
                                       sidebarLayout(
                                         sidebarPanel(width=3,
                                            selectInput("alternd1Ds1LogSt", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                            numericInput("LagOrderADFd1Ds1LogSt", label = "Lag",  min=0, value=12),
                                       ),
                                         tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationarited1Ds1LogSt")),
                                       )),
                            )), 
                   

                   tabPanel("d?D?(log?(St))", br(),
                            sidebarLayout(
                              sidebarPanel(width=2,
                                           br(),br(),
                                           numericInput("d_n", label = "d :", min=0, value=0),
                                           numericInput("DS_n", label = "D :",min=0,  value=0),
                                           # selectInput("islog", label = "log", choices=c("Yes","No"),selected="No"),
                                           br(),
                                           checkboxInput("check_box", HTML("<b>log(S(t))</b>"), value = FALSE),
                                           br(),br(),
                                           conditionalPanel(
                                             condition = "input.tabs == 'Plot (*)'",
                                             numericInput("tickSize", label = HTML("<span style='color:red;'>Tick size :</span>"), min=1,  value=12)
                                           )
                              ),
                              mainPanel(width=10,
                                        tabsetPanel(id = "tabs",
                                                    tabPanel("d?D?log?(St) (*)", uiOutput("d_D_Log_ts_Choice_UI")), 
                                                    # tabPanel("d?D?log?(St)", plotOutput("d_D_Log_ts_Choice",width=750,height = 500 )),
                                                    tabPanel("Plot (*)", uiOutput("tsPlot_Choice_UI")),
                                                    # tabPanel("Plot",plotOutput("tsPlot_Choice",width=750,height = 500)),
                                                    tabPanel("ACF", plotOutput("difference2ACF",width=750,height = 500)),
                                                    tabPanel("PACF", plotOutput("difference2PACF",width=750,height = 500)),
                                                    tabPanel("ACF+PACF", plotOutput("difference2ACFPACF",width=620,height = 500)),
                                                    tabPanel("stationary[ADF]", 
                                                             sidebarLayout(
                                                               sidebarPanel(width=3,
                                                                            selectInput("alternd2St", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                                                            numericInput("LagOrderADFd2St", label = "Lag",  min=0, value=12),
                                                                            #submitButton("Submit ==>"),
                                                               ),
                                                               tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationarited2St")),
                                                             )),
                                                    tabPanel("ARIMA", verbatimTextOutput("ARIMA_d_D_log" )),
                                        )
                              )
                            )),
                   
             
                 )),

        
        tabPanel("Stat.Plots",
                 fluidPage(
                   tabsetPanel(
                     tabPanel("Ts Display", plotOutput("tsDisplay",width=900,height = 630 )),
                     tabPanel("Box-Plot", plotOutput("boxP",width=900,height = 630 )),
                     tabPanel("Sous-séries", plotOutput("SubSeriesPlot",width=900,height = 630 )),
                     tabPanel("Graphs Sais.", plotOutput("SeasonPlot",width=900,height = 630 )),
                     tabPanel("Graphs Sais. Polaire", plotOutput("SeasonPlotPolar",width=900,height = 630 )),    
                     tabPanel("lag Plots", plotOutput("lagPlot",width=900,height = 630 )),
                   ))),
        
        tabPanel("Decomposition",
                 fluidPage(
                   tabsetPanel(
                     
                     tabPanel("Classical model", br(),
                              sidebarLayout(
                                sidebarPanel(width=3,
                                             selectInput("model1",label = "model", choices=c("additive","multiplicative"),selected="additive"),
                                ),
                                
                                mainPanel(width=1100,
                                          tabsetPanel(
                                            tabPanel("Classical", plotOutput("decompose",width=800,height = 700 )),
                                            tabPanel("Classical", plotOutput("decompose2",width=800,height = 700 )),
                                            tabPanel("Coefficients saisonnier", verbatimTextOutput("dFactors" )),
                                          ))
                              )),

                     tabPanel("SEATS", 
                              tabsetPanel(
                                tabPanel("SEATS", plotOutput("SEATSdecompose",width=800,height = 700 )),
                                tabPanel("SEATS Seas.F.", verbatimTextOutput("SEATSFactors" )),
                              )), 

                     tabPanel("X11", 
                              tabsetPanel(
                                tabPanel("X11", plotOutput("X11decompose",width=800,height = 700 )),
                                tabPanel("X11 Seas.F.", verbatimTextOutput("X11Factors" )),
                              )),   
                     
                     tabPanel("STL", 
                              tabsetPanel(
                                tabPanel("STL", plotOutput("STLdecompose",width=800,height = 700 )),
                                tabPanel("STL Seas.F.", verbatimTextOutput("STLFactors")),
                              )),  
                   ))),
        

        tabPanel("Auto Forecast", width=1200,
                 fluidPage(
                   tabsetPanel(

                   tabPanel("Model", verbatimTextOutput("autoForecast"), class="span7"),
                  
                   tabPanel("Model Equation",
                            tabsetPanel(
                              tabPanel("Model SARIMA", uiOutput("auto_SARIMA_symbolic")),
                              # tabPanel("Model HW Equation", uiOutput("equationOutput_HW"))
                            )),

                   tabPanel("Forecaste", 
                            tabsetPanel(
                              tabPanel("Forecaste Values", tableOutput("results_forecastTable")),
                              tabPanel("Forecaste Plot", plotOutput("autoForecast_plot",width=900,height = 630)),
                            )), 
                   

                   
                   tabPanel("Tests", 
                            tabsetPanel(
                              tabPanel("Trend [Mann-Kendall]", verbatimTextOutput("testTrendMK")),
                              
                              tabPanel("stationary [Augmented Dickey-Fuller]", 
                                       sidebarLayout(
                                         sidebarPanel(width=3,
                                                      selectInput("altern", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                                      numericInput("LagOrderADF", label = "Lag",  min=0, value=12),
                                         ),
                                         tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("test_ADF")),
                                       )),
                              
                              
                              tabPanel("KPSS", verbatimTextOutput("test_KPSS")),
                              
                              tabPanel("Ljung-Box lag(n)", br(),
                                       sidebarLayout(
                                         sidebarPanel(width=2,
                                                      numericInput("lagorder", label = "Lag order for test:", min=0, value=5),
                                                      selectInput("typeBoxTest", label = "type", choices=c("Box-Pierce","Ljung-Box"),selected="Ljung-Box"),
                                         ),
                                         tabPanel("Ljung-Box lag(n) [Auto-corrélation des Erreurs]", verbatimTextOutput("testLBn")),
                                       )),
                            )), 
                     

                     
                   tabPanel("Residuals", 
                            tabsetPanel(
                              tabPanel("Res.", plotOutput("chkRes",width=830,height = 600)),
                              tabPanel("Diag.", plotOutput("tsdiag",width=700,height = 600)),
                            )), 
                     
                     
                   tabPanel("ACF & PACF Res.", 
                            tabsetPanel(
                                tabPanel("ACF Residuals", plotOutput("plotACFRes",width=700,height = 500)),
                                tabPanel("PACF Residuals", plotOutput("plotPACFRes",width=700,height = 500)),
                                tabPanel("ACF & PACF Residuals", plotOutput("plot_ACF_PACF_Res",width=700,height = 600)),
                            )), 
                   
                     
                   tabPanel("unit Cercle", plotOutput("unitCercle", width=750, height = 580)),
                   
                   
                   tabPanel("ARIMA Slow", 
                            tabsetPanel(
                              tabPanel("ARIMA Slow Model (P.S. Take Times to display Results)", 
                                  sidebarLayout(
                                    sidebarPanel(width=3,
                                                 numericInput("maxp", label = "max.p", min=0, value=5),
                                                 numericInput("maxd", label = "max.d",min=0,  value=2),
                                                 numericInput("maxq", label = "max.q", min=0, value=5),
                                                 numericInput("maxPs", label = "max.P",min=0,  value=2),
                                                 numericInput("maxDs", label = "max.D",min=0,  value=1),
                                                 numericInput("maxQs", label = "max.Q", min=0, value=2),
                                                 numericInput("maxorder", label = "max.order[p+q+P+Q]", min=0, value=8),
                                    ),
                                    tabPanel("ARIMA Slow Model(Wait)", verbatimTextOutput("Pslow"), class="span7"),
                                  )),
                              
                    )), 
              ))),
        
      
        
        tabPanel("ARIMA[p,d,q][P,D,Q][s]",
                 fluidPage(
                   tabsetPanel(
                     
                     tabPanel("ARIMA pdq", br(),
                              sidebarLayout(
                                sidebarPanel(width=2,
                                             numericInput("ARIMAp", label = "p:", min=0, value=0),
                                             numericInput("ARIMAd", label = "d:",min=0,  value=0),
                                             numericInput("ARIMAq", label = "q:", min=0, value=0),
                                             numericInput("ARIMAps", label = "P:",min=0,  value=0),
                                             numericInput("ARIMAds", label = "D:",min=0,  value=0),
                                             numericInput("ARIMAqs", label = "Q:", min=0, value=0),
                                             selectInput("driftYN", label = "drift", choices=c("TRUE","FALSE"),selected="FALSE"),
                                ),
                                
                                mainPanel(width=10,
                                          tabsetPanel(
                                            tabPanel("ARIMA (*)", uiOutput("Previsions_Plot_pdq_UI")),  
                                            tabPanel("Model & Equation", 
                                                     tabsetPanel(
                                                       tabPanel("Model", verbatimTextOutput("model_ARIMApdq")),
                                                       tabPanel("Model Equation", uiOutput("sarima_eq_render_numerical_1")),
                                                     )), 
                                            tabPanel("Model with p-values", verbatimTextOutput("model_ARIMApdq_p_values")), 
                                            tabPanel("ACF+PACF", plotOutput("plot_ACF_PACF_Res_pdq", width=600, height = 550)),
                                            tabPanel("unit Cercle", plotOutput("unit_Circle_pdq", width=750, height = 580)),
                                            tabPanel("Plot", plotOutput("timeSeriesPlot_SARIMA", width=750, height = 580)),
                                          ))
                              )),
                     

                     tabPanel("tests", 
                              tabsetPanel(
                                tabPanel("Trend [Mann-Kendall]", verbatimTextOutput("testTrendMK2")),
                                tabPanel("stationary [Augmented Dickey-Fuller]", 
                                         sidebarLayout(
                                           sidebarPanel(width=3,
                                                        selectInput("altern2", label = "stationary or explosive", choices=c("stationary","explosive"),selected="stationary"),
                                                        numericInput("LagOrderADF2", label = "Lag",  min=0, value=12),
                                           ),
                                           tabPanel("stationary [Augmented Dickey-Fuller]", verbatimTextOutput("teststationariteARIMApdq")),
                                         )),
                                tabPanel("KPSS", verbatimTextOutput("kpssTest2")),
                                tabPanel("DF-GLS", verbatimTextOutput("test_DFGLS")),
                                tabPanel("Ljung-Box lag(n)", br(),
                                         sidebarLayout(
                                           sidebarPanel(width=2,
                                                        numericInput("lagorder1", label = "Lag order for test:", min=0, value=5),
                                                        selectInput("typeBoxTest1", label = "type", choices=c("Box-Pierce","Ljung-Box"),selected="Ljung-Box"),
                                           ),
                                           tabPanel("Ljung-Box lag(n)", verbatimTextOutput("testLBnARIMApdq")),
                                         )),                             
                              )), 
                     
                     tabPanel("Residuals", 
                              tabsetPanel(
                                tabPanel("Res. (*)", uiOutput("chkResARIMApdq_UI")),  
                                tabPanel("Diag. (*)", uiOutput("tsdiagARIMApdq_UI")),
                                tabPanel("Diag 2.", plotOutput("tsdiag2",width=700,height = 600)),
                                tabPanel("Shapiro-Wilk", verbatimTextOutput("ShapiroTest")),
                              )),  

                     tabPanel("Forecaste", tableOutput("forecast_ARIMA_pdq")),
                     
                     tabPanel("Forc.Plot [80%,95%](*)", value = "forecast_tab_80", uiOutput("ForecastePlotUI")),  
                     
                     tabPanel("Model.Eq.", 
                              tabsetPanel(
                                tabPanel("Model Equation", uiOutput("sarima_eq_render_numerical")),
                                tabPanel("Model Equation (1)", uiOutput("sarima_eq_render_numerical_one")),
                              )),  
                     

                     tabPanel("S(t)+model+Prev (*)", br(),
                              sidebarLayout(

                                sidebarPanel(width=3,
                                             uiOutput("dynamicInputs")  # Placeholder for dynamic inputs
                                             # ... other sidebar elements if any ...
                                ),
                                
                                mainPanel(
                                  uiOutput("plotAll_UI"),
                                )
                              )
                     )
                     
                     ))),  
        
      
        
        tabPanel("-?-",
                 fluidPage(
                   tabsetPanel(
                     tabPanel("Help", verbatimTextOutput("AboutAng")),
                     tabPanel("Aide", verbatimTextOutput("AboutFr")),   
                     # tabPanel("debugOutput", verbatimTextOutput("debugOutput")),
                   ))
        ),

        
      )
    )
  )
))
