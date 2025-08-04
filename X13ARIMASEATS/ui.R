library(shiny)
library(shinythemes)
library(bslib)
library(plotly)
library(shinydashboard)
library(lubridate)

anios <- as.list(c(1950:year(Sys.Date()))); names(anios) <- as.character(1950:year(Sys.Date()))

# Define UI for application
fluidPage(
  navbarPage("X-13 ARIMA-SEATS", theme = shinytheme("lumen"),
             tabPanel("", icon = icon("house"),
                      fluidRow(style = "background-image: url(b9.jpg); background-size: contain;",
                         fluidRow(style = "margin: 1em;",
                           column(7,
                                  includeHTML("html/bienvenida.html")
                           ),
                           column(5,
                             navset_bar(
                               nav_panel("SPC",
                                         card(downloadButton("download1", label = "DESCARGAR")),
                                         card(verbatimTextOutput("spc_home"), height = '580px')
                               ),
                               nav_panel("DAT",
                                         card(downloadButton("download2", label = "DESCARGAR")),
                                         card(verbatimTextOutput("dat_home"), height = '580px')
                               ),
                               nav_panel("CONVERTIDOR",
                                         fluidRow(style = "background-color: #FFFFFF; align-items: center; justify-content: center;",
                                           column(4,
                                                  selectInput("periodo", label = h4("Periodicidad"), 
                                                              choices = list("Mensual" = 12, "Trimestral" = 4), selected = 1)
                                           ),
                                           column(4,
                                                  selectInput("anio", label = h4("Año"), 
                                                              choices = anios, selected = 1998)
                                           ),
                                           column(4,
                                                  uiOutput("mestrim")
                                           )
                                         ),
                                         fluidRow(style = "background-color: #FFFFFF; align-items: center; justify-content: center;",
                                                  column(12,
                                                         helpText("Para su consideración:"),
                                                         helpText("-- En aras de evitar un conjunto de datos corto, el programa está diseñado para
                                                                  asimilar series de tiempo con una duración mínima de 5 años (60 meses o 
                                                                  20 trimestres)."),
                                                         helpText("-- Cree un archivo con sus datos en Excel (solo se admite este formato), justo 
                                                                  como se muestra en la imagen de abajo. Consulte el PRONTUARIO.")
                                                  )
                                         ),
                                         fluidRow(style = "background-color: #FFFFFF; align-items: center; justify-content: center;",
                                                  column(4,
                                                         fileInput("datos_conv", label = h4("Seleccione su archivo de datos Excel (.xlsx)"), 
                                                                   width = '100%', accept = ".xlsx", buttonLabel = "BUSCAR")
                                                  ),
                                                  column(5,
                                                         textInput("dat_descarga", label = h4("Ingrese la ubicación de descarga"), value = NULL, width = '100%')
                                                  ),
                                                  column(3,
                                                         br(),br(),
                                                         actionButton("conv", label = "Generar DAT", width = '100%', icon = icon("code-compare"))
                                                  )
                                         ),
                                         fluidRow(style = "background-color: #FFFFFF; align-items: center; justify-content: center;",
                                                  HTML('<h5 style="color: #DCDCDC; text-align: center;"><em>Ejemplo de archivo de datos</em></h5>'),
                                                  img(src = 'ejemplo_datos.jpg', align = "centre", width = "100%")
                                         )
                               )
                             )
                           )
                         ),
                         fluidRow(style = "margin: 1em;",
                           column(7,
                                  includeHTML("html/prontuario.html"),
                                  card(height = "100%", style = "background-color: #FFFFFF; align-items: center; justify-content: center;",
                                       radioButtons("radio", label = NULL,
                                                    choices = list("CONVERTIDOR" = 1, "INICIO" = 2, "MODELO ARIMA" = 3), selected = 1),
                                       br()
                                  ),
                                  uiOutput("resena2")
                           ),
                           column(5,
                                  uiOutput("resena")
                           )
                         )
                      ),
                      fluidRow(
                         column(12,
                                card(height = "100%", style = "background-color: #FFFFFF; align-items: center; justify-content: center;",
                                     HTML('<h5 style="color: #DCDCDC; text-align: center;"><em>Ajuste estacional en X-13 ARIMA SEATS,
                                  desarrollado en R Studio. Por Mauricio de los Santos Hernández.</em></h5>')
                                )
                         )
                      )
             ),
             
             tabPanel( "INICIO",
                       sidebarLayout(
                         sidebarPanel( titlePanel("SERIE"),
                                       fileInput("SPC_file", label = "Seleccione archivo SPC", width = '100%', accept = ".spc", buttonLabel = "BUSCAR"),
                                       fileInput("DAT_file", label = "Seleccione archivo DAT", width = '100%', accept = ".dat", buttonLabel = "BUSCAR"),
                                       br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), hr(),
                                       a(href = "https://en.www.inegi.org.mx/app/indicadores/?tm=0", target = "_blank",
                                         img(src = 'logo.jpg', align = "centre", width = "100%")),
                                       
                                       width = 3
                         ),
                         
                         mainPanel(
                           card(
                             h2("Serie original"),
                             plotlyOutput("DAT_graph")
                           ),
                           card(
                             h2("Especificaciones del modelo"),
                             tableOutput("contenido")
                           ),
                           width = 9
                         )
                       )
             ),
             
             tabPanel("MODELO ARIMA",
                      fluidPage(
                        fluidRow(
                          column(4,
                            fluidRow(column(6, actionButton("mant", label = "Modelo Inicial", width = '100%', icon = icon("wand-sparkles"))),
                                     column(6, actionButton("run", label = "RUN", class = "btn-primary", width = '100%', icon = icon("ghost")))),
                            textAreaInput("SPC_real", label = NULL, value = NULL, width = '100%', height = '580px')
                          ),
                          column(8,
                                 navset_bar(
                                   nav_panel("OUTPUT",
                                             card(verbatimTextOutput("out"), height = "530px")
                                   ),
                                   nav_panel("GRÁFICAS",
                                             navset_tab(
                                               nav_panel("ACF - PACF",
                                                         fluidRow(hr()),
                                                         fluidRow(
                                                           column(6, plotlyOutput("graf1")),
                                                           column(6, plotlyOutput("graf2"))
                                                         )
                                               ),
                                               nav_panel("Seasonal Factors",
                                                         fluidRow(hr()),
                                                         plotlyOutput("graf3")
                                               ),
                                               nav_panel("Spectrum Irr y SAdj",
                                                         fluidRow(hr()),
                                                         fluidRow(
                                                           column(6, plotlyOutput("graf4")),
                                                           column(6, plotlyOutput("graf5"))
                                                         )
                                               ),
                                               nav_panel("Spectrum Rsd y AdjOri",
                                                         fluidRow(hr()),
                                                         fluidRow(
                                                           column(6, plotlyOutput("graf6")),
                                                           column(6, plotlyOutput("graf7"))
                                                         )
                                               ),
                                               nav_panel("Overlay Graph",
                                                         fluidRow(hr()),
                                                         plotlyOutput("graf8")
                                               ),
                                               nav_panel("Forecasts and Logs",
                                                         fluidRow(hr()),
                                                         fluidRow(
                                                           column(6, plotlyOutput("graf9")),
                                                           column(6, plotlyOutput("graf10"))
                                                         )
                                               )
                                             )
                                   ),
                                   nav_panel("DESCARGAR SALIDA",
                                             fluidRow(
                                               column(4, 
                                                      textInput("nombre", label = h4("DENOMINACIÓN"), value = "", width = '100%')),
                                               column(5, 
                                                      textInput("out_descarga", label = h4("UBICACIÓN"), value = "", width = '100%')),
                                               column(3, br(), br(),
                                                      actionButton("salida", label = "DESCARGA", width = '100%', icon = icon("atom")))
                                             ),
                                             fluidRow(style = "background-color: #FCFCFC; align-items: center; justify-content: center;",
                                                      HTML('<h5 style="font-weight: bold; text-align: center;"><em>NOMENCLATURA</em></h5>')),
                                             fluidRow(style = "background-color: #FCFCFC;",
                                                      column(4, includeHTML("html/nomenclatura1.html")),
                                                      column(4, includeHTML("html/nomenclatura2.html")),
                                                      column(4, includeHTML("html/nomenclatura3.html"))
                                             ),
                                             fluidRow(style = "background-color: #FCFCFC; align-items: center; justify-content: center;",
                                                      HTML('<p style="color: ##B3B3B3; font-size: 11px;"><em><sup>1</sup>Modelos de regresión con 
                                                           errores ARIMA, en los cuales la media de la serie de tiempo está descrita por una 
                                                           combinación lineal de los regresores, y la estructura de covarianza de la serie es la 
                                                           misma de un proceso ARIMA. Si no se emplean regresores, se asume que la media es 0, 
                                                           entonces el modelo regARIMA se reduce a un modelo ARIMA.</em></p>'))
                                   )
                                 )
                          )
                        ),
                        fluidRow(hr()),
                        fluidRow(uiOutput("error_modelo")),
                        fluidRow(
                          navset_tab(
                            nav_panel("General",
                                      card(dataTableOutput("diagnostico1"), height = "100%")
                            ),
                            nav_panel("Model Info",
                                      card(dataTableOutput("diagnostico2"), height = "100%")
                            ),
                            nav_panel("Model Diagnostics",
                                      card(dataTableOutput("diagnostico3"), height = "100%")
                            ),
                            nav_panel("x11",
                                      card(dataTableOutput("diagnostico4"), height = "100%")
                            ),
                            nav_panel("Spectrum & QS",
                                      card(dataTableOutput("diagnostico5"), height = "100%")
                            )
                          )
                        )
                      )
             )
  )
)
