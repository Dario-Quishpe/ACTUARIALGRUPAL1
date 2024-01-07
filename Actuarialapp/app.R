library(shiny) #holi
library(shinydashboard)
library(FinCal)
library(tidyverse)
library(FinancialMath)
library(bslib)
library(writexl)
suppressWarnings(library(data.table))
suppressWarnings(library(shiny))
suppressWarnings(library(readxl))
suppressWarnings(library(writexl))
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(stringr))
suppressWarnings(library(bit64))
suppressWarnings(library(highcharter))
suppressWarnings(library(rjson))
suppressWarnings(library(httr))
suppressWarnings(library(readr))
suppressWarnings(library(tidyr))
suppressWarnings(library(xts))
suppressWarnings(library(lubridate))
suppressWarnings(library(formattable))
suppressWarnings(library(kableExtra))
suppressWarnings(library(sparkline))
suppressWarnings(library(htmlwidgets))
suppressWarnings(library(shinyauthr))
suppressWarnings(library(sads))
suppressWarnings(library(caret))
suppressWarnings(library(diagram))

ui <- fluidPage(
  br(),
  theme = bs_theme(bootswatch = "flatly"),
  fluidRow(column(width=2,align="center",style="background:#DEE9F9",img(src="https://cem.epn.edu.ec/imagenes/logos_institucionales/big_png/BUHO_EPN_big.png", width="110px", height="125px")), # Logo página principal
           column(width=8,style="background:black", h1("MATEMATICA ACTUARIAL - TRABAJO GRUPAL 1 ", 
                                                       style = "background:#F9EDE9 ;text-align:center;align-items:center;color:'black';padding:30px;font-size:2.2em")),
           column(width=2,align="center",style="background:#DEE9F9",img(src="https://cem.epn.edu.ec/imagenes/logos_institucionales/big_png/BUHO_EPN_big.png", width="110px", height="125px"))
  ),
  br(),
 
  navbarPage(
    title = ("Actuarial"),
    tabPanel("Amortización",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sistema", "Sistema de amortización:", choices = c("Alemán", "Francés","Americano")),
                 numericInput("monto", "Monto solicitado en dólares:", value = 1000, min = 0),
                 selectInput("frecuencia", "Periodos de amortización:", choices = c("Mensual", "Trimestral", "Semestral", "Anual")),
                 checkboxInput("encaje", "¿Hay encaje?", FALSE),
                 conditionalPanel(
                   condition = "input.encaje == true",
                   numericInput("deposito_inicial", "Depósito inicial (cuota de encaje)", value = 10, min = 0)
                 ),
                 numericInput("tasa", "Tasa de interés efectiva anual (%):", value = 10, min = 0, step=1),
                 numericInput("periodos", "Número de periodos de amortización:", value = 12, min = 1),
                 downloadButton("des","Descargar Tabla de Amortización")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Resumen", tableOutput("resumen")),
                   conditionalPanel(
                     condition = "input.encaje == true",
                     tabPanel("TIR", tableOutput("tir"))
                   ),
                   tabPanel("Tabla de Amortización", tableOutput("tabla"),style = "height:500px; overflow-y: scroll;")
                 )
               )
             )
    )
  )
)

server <- function(input, output) {
  interes_superiodal <- function(i, m) {
    (1 + i)^(1/m) - 1
  }
  tabla <- reactive({
    tasa <- switch(input$frecuencia,
                   "Mensual" = interes_superiodal(input$tasa/100, 12),
                   "Trimestral" = interes_superiodal(input$tasa/100, 4),
                   "Semestral" = interes_superiodal(input$tasa/100, 2),
                   "Anual" = interes_superiodal(input$tasa/100, 1)
    )
    datos <- list(
      monto = input$monto,
      #encaje = encaje,
      tasa = tasa,
      periodos = input$periodos,
      frecuencia = input$frecuencia,
      sistema = input$sistema
    )
    if (input$sistema == "Alemán"){
      amortizacion <- datos$monto / datos$periodos
      interes <- rep(datos$tasa, datos$periodos)
      cuota <- numeric(datos$periodos)
      interes <- numeric(datos$periodos)
      saldo <- numeric(datos$periodos)
      interes[1] <- datos$monto*datos$tasa
      cuota[1]<-interes[1]+amortizacion
      saldo[1]<-datos$monto-amortizacion
      
      for (i in 2:datos$periodos) {
        interes[i] <- saldo[i-1]*datos$tasa
        cuota[i]<-interes[i]+amortizacion
        saldo[i] <- saldo[i-1] - amortizacion
      }
      
    }
    else if (input$sistema == "Francés"){
      interes <- rep(datos$tasa, datos$periodos)
      cuota <- datos$monto/((1-(1+datos$tasa)^(-datos$periodos))/datos$tasa)
      cuota1 <- rep(cuota, datos$periodos)  
      amortizacion <- numeric(datos$periodos)
      interes <- numeric(datos$periodos)
      saldo <- numeric(datos$periodos)
      amortizacion[1] <- cuota*(1+datos$tasa)^(-datos$periodos)
      interes[1] <- cuota-amortizacion[1]
      saldo[1] <- datos$monto-amortizacion[1]
      
      for (i in 2:datos$periodos) {
        amortizacion[i] <- cuota*(1+datos$tasa)^(-datos$periodos-1+i)
        interes[i] <- cuota-amortizacion[i]
        saldo[i] <- saldo[i-1] - amortizacion[i]
      }
    }
    else if (input$sistema == "Americano"){
      amortizacion <- 0 
      interes <- rep(datos$tasa, datos$periodos) 
      cuota <- numeric(datos$periodos)
      saldo <- numeric(datos$periodos)
      interes[1] <- datos$monto * datos$tasa
      cuota[1] <- interes[1] + amortizacion 
      saldo[1] <- datos$monto - amortizacion 
      
      for (i in 2:datos$periodos) {
        interes[i] <- saldo[i-1] * datos$tasa 
        cuota[i] <- interes[i] + amortizacion 
        saldo[i] <- saldo[i-1] - amortizacion 
      }
      
      cuota[datos$periodos] <- cuota[datos$periodos] + datos$monto
    }
    
    data.table(
      Período = 1:datos$periodos,
      Amort.Capital = amortizacion,
      Interés = interes,
      Cuota = cuota,
      Saldo = saldo
    )
  })
  
  output$tabla <- renderTable(tabla(),striped = TRUE)
  output$des <- downloadHandler(
    filename = function(){"Tabla De Amortizacion.xlsx"},
    content = function(file){write_xlsx(tabla(), path = file)}
  )
  
  deposito_inicial <- reactive({
    if (input$encaje == "Si") {
      as.numeric(input$deposito_inicial)
    } else {
      0
    }
  })
  tir <- reactive({
    deposito_inicial <- deposito_inicial()
    tasa <- switch(input$frecuencia,
                   "Mensual" = interes_superiodal(input$tasa/100, 12),
                   "Trimestral" = interes_superiodal(input$tasa/100, 4),
                   "Semestral" = interes_superiodal(input$tasa/100, 2),
                   "Anual" = interes_superiodal(input$tasa/100, 1)
    )
    datos <- list(
      monto = input$monto,
      tasa = tasa,
      periodos = input$periodos,
      frecuencia = input$frecuencia,
      sistema = input$sistema
    )
    cf0 <- deposito_inicial
    tabla_actual <- tabla() # Get the current value of tabla
    tabla_actual$Cuota <- as.numeric(tabla_actual$Cuota)
    tabla_actual$Período <- as.numeric(tabla_actual$Período)
    
    cf <- tabla_actual$Cuota
    times <- tabla_actual$Período
    tir1 <- IRR(cf0, cf, times)
    tir2<- max(0, min(tir1))
    if (input$frecuencia == "Mensual") {
      tir_anual <- (1 + tir2) ^ 12 - 1
    } else if (input$frecuencia == "Trimestral") {
      tir_anual <- (1 + tir2) ^ 4 - 1
    } else if (input$frecuencia == "Semestral") {
      tir_anual <- (1 + tir2) ^ 2 - 1
    } else {
      tir_anual <- tir2
    }
    
    data.frame(
      "TIR" = tir2,
      "TIR Anual" = tir_anual
    )
  })
  output$tir <- renderTable(tir(), striped = TRUE)
  
  resumen <- reactive({
    tasa <- switch(input$frecuencia,
                   "Mensual" = interes_superiodal(input$tasa/100, 12),
                   "Trimestral" = interes_superiodal(input$tasa/100, 4),
                   "Semestral" = interes_superiodal(input$tasa/100, 2),
                   "Anual" = interes_superiodal(input$tasa/100, 1)
    )
    datos <- list(
      monto = input$monto,
      #encaje = encaje,
      tasa = tasa,
      periodos = input$periodos,
      frecuencia = input$frecuencia,
      sistema = input$sistema
    )
    total_intereses <- tabla() %>% pull(Interés) %>% sum()
    total_pagar <- datos$monto + total_intereses
    data.frame(
      "Monto Solicitado" = datos$monto,
      "Total de Intereses" = total_intereses,
      "Total a Pagar" = total_pagar
    )
  })
  output$resumen <- renderTable(resumen(), striped = TRUE)
  

}
shinyApp(ui = ui, server = server)