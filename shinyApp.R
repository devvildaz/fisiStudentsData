library(shiny)
library(readxl)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  # dashboard title
  dashboardHeader(title="FISI-DATA"),
  # dashboard sidebar
  dashboardSidebar(
    # sidebar item
    sidebarMenu(
      menuItem("Repitencias",tabName="rep_tab", icon=icon("dashboard")),
      menuItem("Calificaciones",tabName="rep_calf", icon=icon("dashboard")),
      menuItem("Tutorias",tabName="rep_tuto", icon=icon("dashboard"))
    )
  ),
  # dashboard body
  dashboardBody(
    tabItems(
      tabItem(
        tabName="rep_tab",
        fluidRow(box(uiOutput("semester_sel"))),
        fluidRow(box(uiOutput("num_rep_slider"))),
        fluidRow(box(plotlyOutput("rep_barplot")),width=12)
        #fluidRow(box(dataTableOutput("rep_dtable")),width=12)
      )
    )
  )
)

countByAsignature <- function(semesterValue) {
  data <- readRDS('fisiRepData.rds') %>%
    filter(cod_semestre==semesterValue) %>%
    select(asignatura) %>%
    count(asignatura) %>%
    arrange(n)
  return (data)
}

server <- function(input, output, session) {
  
  fisiRepData <- reactive({
    readRDS('fisiRepData.rds')
  })
  
  repNumRep <- reactive({
    countByAsignature(input$semester_sel)
  })
  
  output$semester_sel <- renderUI({
    choices <- fisiRepData() %>% select(cod_semestre) %>% distinct() %>% append("any")
    selectInput(
      inputId = "semester_sel",
      label = "Semestre",
      selected = choices[0],
      choices = choices
    )
  })
  
  output$num_rep_slider <- renderUI({
    data <- repNumRep()
    min <- data
    sliderInput(
      inputId = "num_rep_slider",
      label = "Numero de repitencias por semestre",
      min = min(data$n),
      max = max(data$n),
      value = min(data$n)
    )
  })
  
  output$rep_dtable <- renderDataTable({
    fisiRepData() %>% filter(cod_semestre==input$semester_sel)
  })
  
  output$rep_barplot <- renderPlotly({
    test_data <- repNumRep() 
    actual_length <- nrow(test_data)
    test_data <- test_data %>% filter(n >= input$num_rep_slider)
    filter_length <- nrow(test_data)
    iniHeight <- 3000
    fig <- plot_ly(
      x=test_data$n, 
      y=test_data$asignatura,
      type='bar',
      orientation='h',
      sizes = c(10, 400)
    )
    iniHeight <- iniHeight * (filter_length/actual_length)
    if(iniHeight < 400 ) {
      iniHeight <- 400
    }
    fig <- fig %>% layout(height = iniHeight)
    fig
  })
}

shinyApp(ui, server)