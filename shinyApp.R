library(shiny)
library(readxl)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title="FISI-DATA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Repitencias",tabName="rep_tab", icon=icon("dashboard")),
      menuItem("Calificaciones",tabName="calf_tab", icon=icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="rep_tab",
        fluidRow(box(uiOutput("semester_sel"), width = 12, column(align = "center", width = 12))),
        fluidRow(box(DT::dataTableOutput("rep_dtable"), width = 12, column(align = "center", width = 12))),
        fluidRow(box(plotlyOutput("rep_barplot"), style='overflow-y:scroll;', width=12))
      ),
      tabItem(
        tabName="calf_tab",
        fluidRow(box(uiOutput("school_sel"), width = 12, column(align = "center", width = 12))),
        fluidRow(box(DT::dataTableOutput("calf_dtable"), width = 12, column(align = "center", width = 12))),
        fluidRow(box(plotlyOutput("calf_barplot"), style='overflow-y:scroll;', width=12))
      )
    )
  )
)

server <- function(input, output) {
  fisiRepData <- reactive({
    data <- readRDS('fisiRepData.rds')
    data
  })
  
  fisiCalfData <- reactive({
    data1 <- readRDS('fisiCalifData.rds')
    data1
  })
  
  output$semester_sel <- renderUI({
    choices <- fisiRepData() %>% select(cod_semestre) %>% distinct()
    selectInput(inputId = "semester_sel",
                label = "Semestre",
                selected=choices[0],
                choices = choices)
  })
  
  output$school_sel <- renderUI({
    choices <- fisiCalfData() %>% select(cod_escuela) %>% distinct()
    selectInput(inputId = "school_sel",
                label = "Escuela",
                selected=choices[0],
                choices = choices)
  })
  
  output$calf_dtable <- renderDataTable({
    fisiCalfData() %>% filter(cod_escuela==input$school_sel)
  })
  
  output$rep_dtable <- renderDataTable({
    fisiRepData() %>% filter(cod_semestre==input$semester_sel)
  })
  output$rep_barplot <- renderPlotly({
    test_data <- fisiRepData() %>% 
      filter(cod_semestre==input$semester_sel) %>%
      select(asignatura, num_rep) %>%
      group_by(asignatura) %>%
      summarise(num_rep = sum(num_rep)) %>%
      arrange(num_rep)
    fig <- plot_ly(x=test_data$num_rep, y=test_data$asignatura,type='bar',orientation='h',sizes = c(10, 300))
    fig <- fig %>% layout(height = 2000)
    fig
  })
  output$calf_barplot <- renderPlotly({
    test_data <- fisiCalfData() %>% 
      filter(cod_escuela==input$school_sel) %>%
      select(cod_asignatura, val_calific_final) %>%
      group_by(cod_asignatura) %>%
      summarise(val_calific_final=mean(val_calific_final)) %>%
      arrange(val_calific_final)
      # summarise(val_calific_final = sum(val_calific_final)) %>%
      # arrange(val_calific_final)
    fig <- plot_ly(x=test_data$val_calific_final, y=test_data$cod_asignatura,type='bar',orientation='h',sizes = c(10, 300))
    fig <- fig %>% layout(height = 2000)
    fig
  })
}

shinyApp(ui, server)