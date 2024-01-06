library(shiny)
library(shinydashboard)
library(FinCal)
library(tidyverse)
library(FinancialMath)
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Calculadora de Amortización",titleWidth = 300),
                    dashboardSidebar(width=300,
                                     sidebarMenu(
                                       menuItem("Amortización", tabName = "amortizacion", icon = icon("table"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "amortizacion",
                                fluidRow(
                                  box(title = "Parámetros",width = 5, solidHeader = TRUE, status = "primary",
                                      selectInput("sistema", "Sistema de amortización:", choices = c("Alemán", "Francés")),
                                      numericInput("monto", "Monto solicitado en dólares:", value = 1000, min = 0),
                                      selectInput("frecuencia", "Periodos de amortización:", choices = c("Mensual", "Trimestral", "Semestral", "Anual")),
                                      #selectInput("encaje", "Depósito inicial:", choices = c("Sí", "No")),
                                      conditionalPanel(
                                        condition = "input.encaje == true",
                                        numericInput("deposito_inicial", "Depósito inicial (cuota de encaje)", value = 10, min = 0)
                                      ),
                                      checkboxInput("encaje", "¿Hay encaje?", FALSE),
                                      
                                      numericInput("tasa", "Tasa de interés efectiva anual (%):", value = 10, min = 0, step=1),
                                      numericInput("periodos", "Número de periodos de amortización:", value = 12, min = 1)
                                  ),
                                  box(title = "Resumen", width = 5, solidHeader = TRUE, status = "primary",
                                      tableOutput("resumen")
                                  ),
                                  conditionalPanel(
                                    condition = "input.encaje == true",
                                    box(title = "TIR", width = 5, solidHeader = TRUE, status = "primary",
                                        tableOutput("tir")
                                    )
                                  ),
                                  box(title = "Tabla de Amortización", width = 5, solidHeader = TRUE, status = "primary",
                                      tableOutput("tabla")
                                  )
                                )
                        )
                      )
                    )
)

server <- function(input, output) {
  output$tabla <- renderTable({
    interes_superiodal <- function(i, m) {
      (1 + i)^(1/m) - 1
    }
    deposito_inicial <- if (input$encaje) as.numeric(input$deposito_inicial) else 0
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
    
    tabla<<-data.frame(
      Período = 1:datos$periodos,
      Amort.Capital = amortizacion,
      Interés = interes,
      Cuota = cuota,
      Saldo = saldo
    )
    # Cálculos para Resumen
    total_intereses <- tabla %>% pull(Interés) %>% sum()
    total_pagar <- datos$monto + total_intereses
    resumen_df <- data.frame(
      "Monto Solicitado" = datos$monto,
      "Total de Intereses" = total_intereses,
      "Total a Pagar" = total_pagar
    )
    
    #Cálculos para la TIR
    cf0 <- deposito_inicial
    tabla$Cuota <- as.numeric(tabla$Cuota)
    tabla$Período <- as.numeric(tabla$Período)
    
    cf <- tabla$Cuota
    times <- tabla$Período
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
    
    tir_df <- data.frame(
      "TIR" = tir2,
      "TIR Anual" = tir_anual
    )
    
    output$resumen <- renderTable(resumen_df)
    output$tir <- renderTable(tir_df)
    return(tabla)
  })
  
}
shinyApp(ui = ui, server = server)