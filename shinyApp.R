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
      menuItem("Repitencias",tabName="rep_tab", icon=icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="rep_tab",
        fluidRow(box(uiOutput("semester_sel"), width = 12, column(align = "center", width = 12))),
        fluidRow(box(DT::dataTableOutput("rep_dtable"), width = 12, column(align = "center", width = 12))),
        fluidRow(box(plotlyOutput("rep_barplot"), style='overflow-y:scroll;', width=12))
      )
    )
  )
)

server <- function(input, output) {
  fisiRepData <- reactive({
    data <- readRDS('fisiRepData.rds')
    data
  })
  
  output$semester_sel <- renderUI({
    choices <- fisiRepData() %>% select(cod_semestre) %>% distinct()
    selectInput(inputId = "semester_sel",
                label = "Semestre",
                selected=choices[0],
                choices = choices)
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
}

shinyApp(ui, server)