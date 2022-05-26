
packages = c("shiny", "shinythemes","data.table", "ggplot2",
             "dplyr","fpp2","forecast","stats", "Kendall",
             "tseries","seasonal","ggfortify", "xts", "astsa",
             "tsibble", "feasts", "readxl")


## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)




shinyUI(pageWithSidebar (
  headerPanel( "SARIMA"),
  sidebarPanel(width=3,
    fileInput('file1', 'Choose file [.txt, .csv or .xlsx] :',
              accept = c('.txt', '.csv', '.tsv', ".xlsx" )),
    
    numericInput("col", label = "My Data is at Column:", value=2,min=1,max=100),
    numericInput("year", label = "Enter the starting year", value=2000),
    selectInput("time", label = "Frequecy of Data [ Seasonality ]", choices=c("Daily","Monthly","Quarterly","Yearly"),selected="Monthly"),
    selectInput("month", label = "Enter the starting day/month/quarter", choices=as.numeric(c(1:366)), selected=as.numeric(1)),
    selectInput("Model",label = "Select the Model", choices=c("ARIMA","Holt-Winters Additive","Holt-Winters Multiplicative","HOLT's Exponential Smoothing"),selected="ARIMA"),
    numericInput("length",label="Enter the length of forecast",value=12, min=1, max=200),
    
    # selectInput("model",label = "Additive or multiplicative Time series: ", choices=c("additive","multiplicative"),selected="additive"),
    # sliderInput("LJung", "lag (for ACF,PACF,LJungBox & diff):", min = 1, max = 50, value = 24, step = 1),
    
    
    #conditionalPanel("output.fileUploaded",
    #                 downloadButton('downloadData', 'Forecast'),
    #                 downloadButton('downloadPlot', 'PlotHD'),
    #                 downloadButton('downloadPlot2', 'Plot4K'))
    
    # selectInput("model1",label = "Additive or Multiplicative model ?", choices=c("additive","multiplicative"),selected="additive"),
    

    br(),
    
    submitButton("Submit ==>"),
    
    br(),
    
  ),
  
  
  
  mainPanel (
    
    navbarPage("ARIMA", selected = "Time Series", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
               
        tabPanel("Time Series",
                 fluidPage(
                    tabsetPanel(
                      tabPanel("data", tableOutput("dataPrint")), 
                      
                      tabPanel("St", 
                               tabsetPanel(
                                 tabPanel("St", plotOutput("tsPlot",width=900,height = 630 )),
                                 tabPanel("ACF", plotOutput("StACF",width=800,height = 500)),
                                 tabPanel("PACF", plotOutput("StPACF",width=800,height = 500)),
                               )),  
                      
                      tabPanel("log(St)", 
                               tabsetPanel(
                                 tabPanel("log(St)",plotOutput("plotLogSt",width=900,height = 630)),
                                 tabPanel("ACF", plotOutput("logStACF",width=800,height = 500)),
                                 tabPanel("PACF", plotOutput("logStPACF",width=800,height = 500)),
                               )), 
                      
                      tabPanel("diff [1] (St)", 
                               tabsetPanel(
                                 tabPanel("d[1](St)",plotOutput("difference1",width=900,height = 630)),
                                 tabPanel("ACF", plotOutput("d1StACF",width=800,height = 500)),
                                 tabPanel("PACF", plotOutput("d1StPACF",width=800,height = 500)),
                               )), 
                      
                      
                      tabPanel("diff[1] (log(St))", 
                               tabsetPanel(
                                 tabPanel("d[1](log(St))",plotOutput("plot10a",width=900,height = 630)),
                                 tabPanel("ACF", plotOutput("d1LogStACFa",width=800,height = 500)),
                                 tabPanel("PACF", plotOutput("d1LogStPACFa",width=800,height = 500)),
                               )), 
                      

                ))),
        
        
        tabPanel("differences",
                 fluidPage(
                   tabsetPanel(
                    
                     tabPanel("d[1](St)", 
                              tabsetPanel(
                                tabPanel("d[1](St)",plotOutput("difference12",width=900,height = 630)),
                                tabPanel("ACF", plotOutput("d1StACF2",width=800,height = 500)),
                                tabPanel("PACF", plotOutput("d1StPACF2",width=800,height = 500)),
                              )), 
                     
                     
                     tabPanel("D[1](St)", 
                              tabsetPanel(
                                tabPanel("D[1](St)",plotOutput("DS1Stplot",width=900,height = 630)),
                                tabPanel("ACF", plotOutput("DS1StACF",width=800,height = 500)),
                                tabPanel("PACF", plotOutput("DS1StPACF",width=800,height = 500)),
                              )),  
                     
                     
                     tabPanel("d[1](log(St))", 
                              tabsetPanel(
                                tabPanel("d[1](log(St))",plotOutput("plot10",width=900,height = 630)),
                                tabPanel("ACF", plotOutput("d1LogStACF",width=800,height = 500)),
                                tabPanel("PACF", plotOutput("d1LogStPACF",width=800,height = 500)),
                              )),  
                     
                     
                     
                     tabPanel("D[1](log(St))", 
                              tabsetPanel(
                                tabPanel("D[1](log(St))",plotOutput("Dlogplot",width=900,height = 630)),
                                tabPanel("ACF", plotOutput("DlogplotACF",width=800,height = 500)),
                                tabPanel("PACF", plotOutput("DlogplotPACF",width=800,height = 500)),
                              )),  
                     
                     
                     
                     tabPanel("d[1](D[1](St))", 
                              tabsetPanel(
                                tabPanel("d[1](D[1](St))",plotOutput("ddsplot",width=900,height = 630)),
                                tabPanel("ACF", plotOutput("ddsplotACF",width=800,height = 500)),
                                tabPanel("PACF", plotOutput("ddsplotPACF",width=800,height = 500)),
                              )),  
                     
                     
                     
                     tabPanel("d[1](D[1](log(St)))", 
                              tabsetPanel(
                                tabPanel("d[1](D[1](log(St)))",plotOutput("dDlogplot",width=900,height = 630)),
                                tabPanel("ACF", plotOutput("dDlogplotACF",width=800,height = 500)),
                                tabPanel("PACF", plotOutput("dDlogplotPACF",width=800,height = 500)),
                              )),  
                     
                     
                     
                     tabPanel("d[2](St)", 
                              tabsetPanel(
                                tabPanel("d[2](St)",plotOutput("difference2",width=900,height = 630)),
                                tabPanel("ACF", plotOutput("difference2ACF",width=800,height = 500)),
                                tabPanel("PACF", plotOutput("difference2PACF",width=800,height = 500)),
                              )), 
                     
                     
                     
                     #tabPanel("d[n](St)", 
                     #         tabsetPanel(
                     #          tabPanel("d[n](St)",plotOutput("diffLagnplot",width=900,height = 630)),
                                #tabPanel("ACF", plotOutput("diffLagnplotACF",width=800,height = 500)),
                                #tabPanel("PACF", plotOutput("diffLagnplotPACF",width=800,height = 500)),
                     #         )),
                     
                   ))),
        
        
        
        tabPanel("Stat. Plots",
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
                                             submitButton("Submit"),
                                             
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
        
        
        
        tabPanel("Forecast",
                 fluidPage(
                   tabsetPanel(
                     tabPanel("Forecasted Plot", plotOutput("M",width=900,height = 630)),
                     
                     tabPanel("Model", 
                              tabsetPanel(
                                 tabPanel("Model", verbatimTextOutput("P"), class="span7"),
                                 tabPanel("Slow Model", verbatimTextOutput("Pslow"), class="span7"),
                                
                              )),  
                     
                     
                     tabPanel("Tests", 
                              tabsetPanel(
                                tabPanel("Stationarité [Augmented Dickey-Fuller]", verbatimTextOutput("teststationarite")),
                                tabPanel("Trend [Mann-Kendall]", verbatimTextOutput("testTrend")),
                              
                                tabPanel("Ljung-Box lag(n)", br(),
                                         sidebarLayout(
                                           sidebarPanel(width=2,
                                                        numericInput("lagorder", label = "Lag order for test:", min=0, value=5),
                                                        submitButton("Submit"),
                                           ),
                                           
                                           tabPanel("Ljung-Box lag(n) [Auto-corrélation des Erreurs]", verbatimTextOutput("testLBn")),
                                           
                                         )),

                              )), 
                     
                     
                     tabPanel("Forecasted Values", tableOutput("F")),
                     tabPanel("Residuals", 
                              tabsetPanel(
                                tabPanel("Res.", plotOutput("chkRes",width=830,height = 600)),
                                tabPanel("Diag.", plotOutput("tsdiag",width=700,height = 600)),
                              )), 
                     
                     ###########-----------------------------------------------
                     
                     tabPanel("ACF Res.", plotOutput("plotACFRes",width=700,height = 500)),
                     
                     tabPanel("PACF Res.", plotOutput("plotPACFRes",width=700,height = 500)),
                     
                     ###########-----------------------------------------------
                     
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
                                             submitButton("Submit"),
                                             
                                ),
                                mainPanel(
                                  
                                  tabsetPanel(
                                    tabPanel("ARIMA", plotOutput("PrevisionsPlotpdq", width=750, height = 580)),
                                    tabPanel("Modèle", verbatimTextOutput("textARIMApdq")),  
                                    tabPanel("ACF.R", plotOutput("plotACFRespdq", width=650, height = 500)),
                                    tabPanel("PACF.R", plotOutput("plotPACFRespdq", width=650, height = 500)),
                                    tabPanel("ACF+PACF Res", plotOutput("plotACFPACFRespdq", width=600, height = 550)),
                                    tabPanel("SARIMA", plotOutput("SARIMAplot", width=600, height = 550)),
                                    
                                     # tabPanel("ndiffs(St)", verbatimTextOutput("ndiffsts" )),
                                    
                                  ))
                                
                              )),
                     
                     # tabPanel("ARIMA pdq", plotOutput("PrevisionsPlotpdq",width=800,height = 1050)),
                     
                     
                     
                     #
                     
                     tabPanel("tests", 
                              tabsetPanel(
                                  tabPanel("Stationarité", verbatimTextOutput("teststationariteARIMApdq")),
                                 
                                  
                                  tabPanel("Ljung-Box lag(n)", br(),
                                           sidebarLayout(
                                             sidebarPanel(width=2,
                                                          numericInput("lagorder1", label = "Lag order for test:", min=0, value=5),
                                                          submitButton("Submit"),
                                             ),
                                             
                                             tabPanel("Ljung-Box lag(n)", verbatimTextOutput("testLBnARIMApdq")),

                                           )),                             
                                  
                                   )), 
                     

                     
                     
                     tabPanel("Residuals", 
                              tabsetPanel(
                                tabPanel("Res.", plotOutput("chkResARIMApdq",width=830,height = 600)),
                                tabPanel("Diag.", plotOutput("tsdiagARIMApdq",width=700,height = 600)),
                              )),  
                     
                     
                     tabPanel("Forecaste", tableOutput("FARIMApdq")),
                     
                     
                     tabPanel("SARIMA", 
                              tabsetPanel(
                                 tabPanel("Forcast", plotOutput("SARIMAforcastplot", width=830, height = 600)),
                                 tabPanel("SARIMA", plotOutput("SARIMAplot2", width=750, height = 600)),
                                 
                              )),  
                     
                     
                     # tabPanel("Forecaste", verbatimTextOutput("FARIMApdq")),
                   ))),  
        
        
        
        
        
        tabPanel("About",
                 fluidPage(
                   tabsetPanel(
                     tabPanel("Help", verbatimTextOutput("AboutAng")),
                     tabPanel("Aide", verbatimTextOutput("AboutFr")),                     
                     
                   ))),

     )
  
  )
  
  
  
))
