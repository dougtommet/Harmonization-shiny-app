library(shiny)
library(shinythemes)

ui <- navbarPage(
  "BASIL harmonization",
  theme = shinytheme("simplex"),
  tabPanel(
    "Home",
    h2("Crosswalk for Delirium Severity Measures"),
    p("Linking between the CAM-S, DRS-R-98, and MDAS instruments."),
    span("A"),
    div(uiOutput("ins_1_ui", inline = T), style = "display: inline-block;"),
    span("score of"),
    div(uiOutput("ins_1_score_ui", inline = T), style = "display: inline-block;"),
    span("is equal to a"),
    div(uiOutput("ins_2_ui", inline = T), style = "display: inline-block;"),
    span("score of"),
    div(uiOutput("ins_2_score", inline = T), style = "display: inline-block; font-size: 20px; font-weight: bold;"),
    span("."),
    
    h3("Notes:"),
    p("This application provides a conversion tool for delirium severity scores as measured by the following commonly used instruments: the Memorial Delirium Assessment Scale (MDAS), the Delirium Rating Scale-Revised-98 (DRS-R-98), and the Confusion Assessment Method-Severity (CAM-S). Appropriate for research and clinical use, this app utilizes crosswalks developed by the NIDUS Measurement and Harmonization Core to map equivalent scores for these three co-calibrated instruments."),
    p(" "),
    p("For example, a clinician might use this tool if they were trying to assess if a patient’s delirium had improved over time but only had scores measured with two different instruments for comparison."),
    p(" ")
    # h4("Reference:"),
    # p(em("1.	Gross AL, Tommet D, D’Aquila M, Schmitt E, Marcantonio ER, Helfand B, Inouye SK, Jones RN (2018). Harmonization of Delirium Severity Instruments: A Comparison of the DRS-R-98, MDAS, and CAM-S Using Item Response Theory. Submitted."))
  )
)

server <- function(input, output, session) {
  library(tidyverse)
  
  dt <- read_csv("shiny.table.csv")
  dt <- round(dt, 1)
  dt$instrument <- factor(dt$instrument, 1:4, names(dt)[-5])
  form_labels <- names(dt)[-5]
  names(form_labels) <- c("CAM-S (long-form)", "MDAS", "DRS-R-98", "CAM-S (Short-form)")
  
  output$ins_1_ui <- renderUI({
    selectInput("ins_1", NULL, choice = form_labels, width = "175px")
  })
  
  selected_dt <- reactive({
    filter(dt, instrument == input$ins_1)
  })
  
  value_range <- reactive({
    range(selected_dt()[input$ins_1])
  })
  
  output$ins_1_score_ui <- renderUI({
    req(input$ins_1)
    textInput("ins_1_score", NULL, width = "150px",
              value = "", placeholder = paste(
                "Range from", value_range()[1], "to", value_range()[2]
              ))
    # numericInput("ins_1_score", NULL, 
    #              value = value_range[1], 
    #              min = value_range[1], 
    #              max = value_range[2])
  })
  
  output$ins_2_ui <- renderUI({
    req(input$ins_1)
    form_labels2 <- form_labels[form_labels != input$ins_1]
    selectInput("ins_2", NULL, choice = form_labels2, width = "175px")
  })
  
  ins_2_score <- reactive({
    req(input$ins_1, input$ins_1_score, input$ins_2)
    selected_ins_1 <- selected_dt()[[input$ins_1]]
    selected_ins_2 <- selected_dt()[[input$ins_2]]
    # gaps <- abs(selected_ins_1 - as.numeric(input$ins_1_score))
    # selected_row_index <- which(gaps == min(gaps))
    return(selected_ins_2[selected_ins_1 == input$ins_1_score])
  })
  
  output$ins_2_score <- renderText({
    req(input$ins_1,input$ins_2)
    if(input$ins_1_score == "") return("_______")
    ins_1_s <- as.numeric(input$ins_1_score)
    if (is.na(ins_1_s)) return("(Error: Non-numeric Input)")
    if (ins_1_s < value_range()[1] | ins_1_s > value_range()[2] ) {
      return("(Error: Input out of range)")
    }
    return(ins_2_score())
  })
}

shinyApp(ui, server)