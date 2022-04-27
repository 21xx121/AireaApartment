Apartment<-read.csv("BUILDING.csv", header=TRUE)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(dplyr) 
library("DT")
button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"



# Define UI
ui <- fluidPage(
  #Navbar structure for UI
  navbarPage("The AIREA Apartment Finder", theme = shinytheme("flatly"),
             tabPanel("Selection", fluid = TRUE,
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel(div(column(width = 5, "Desired Home"),
                                         column(width = 7, img(height = 67.5, width = 400, src = "ossc_logo.jpg"))
                          )),
                          #shinythemes::themeSelector(),
                          
                          
                          fluidRow(actionButton("selectall", label="Select/Deselect all")),
                          
                          fluidRow(
                            column(3,
                                   
                                   # Select Area
                                   checkboxGroupInput(inputId ="selected_OP",
                                                      label = "OP:",
                                                      choices =unique(Apartment$OP),
                                                      selected = "yes")
                            ),
                            column(3,
                                   checkboxGroupInput(inputId ="selected_location",
                                                      label = "Select Location(s):",
                                                      choices = unique(Apartment$Area),
                                                      selected = "NJ")
                            ),
                            column(3,
                                   checkboxGroupInput(inputId ="selected_laundry",
                                                      label = "Laundry in Unit:",
                                                      choices =unique(Apartment$Laundry),
                                                      selected = "yes")
                            ),
                            column(3,
                                   checkboxGroupInput(inputId ="selected_Window",
                                                      label = "Floor to Ceiling Window:",
                                                      choices =unique(Apartment$floor.to.ceilling.Windows),
                                                      selected = "yes")
                            )
                          ),
                          
                          fluidRow(column(3, 
                                          checkboxGroupInput(inputId ="selected_balcony",
                                                             label = "Have balcony units:",
                                                             choices =unique(Apartment$units.with.balcony),
                                                             selected = "yes")
                          ),
                          column(6, 
                                 sliderInput('selected_year', 'Year Built', min = 1907, max = 2021, 
                                             step = 1, value = c(2000, 2021), sep = '')
                          )
                          
                          
                          ),
                          
                          width = 12),
                        
                        mainPanel(
                          fluidRow(column(5,
                                          withSpinner(verbatimTextOutput("apartments_list"))
                          ),
                          
                          ),
                          width = 12),
                        
                      )
             ),
             tabPanel("Details", fluid = TRUE,
                      tags$style(button_color_css),
                      
                      mainPanel(
                        fluidRow(
                          withSpinner(dataTableOutput("apartments_table"))
                        ))
                      
             )
  )
)


library("tidyverse")


server <- function(input, output, session) {
  
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0) {
        updateCheckboxGroupInput(session=session, inputId="selected_OP",
                                 choices = list("yes" = "yes",
                                                "no" = "no"),
                                 selected = c(unique(Apartment$OP)))
        updateCheckboxGroupInput(session=session, inputId="selected_location",
                                 choices = list("ktown" = "ktown",
                                                "TImeSquare" = "TImeSquare",
                                                "FIDI" = "FIDI",
                                                "Midtown West"= "Midtown West",
                                                "Midtown East"= "Midtown East",
                                                "LIC" = "LIC",
                                                "NJ" ="NJ"),
                                 selected = c(unique(Apartment$Area)))
        updateCheckboxGroupInput(session=session, inputId="selected_laundry",
                                 choices = list("yes" = "yes",
                                                "no" = "no",
                                                "partial" = "partial"),
                                 selected = c(unique(Apartment$Laundry)))
        updateCheckboxGroupInput(session=session, inputId="selected_Window",
                                 choices = list("yes" = "yes",
                                                "no" = "no"),
                                 selected = c(unique(Apartment$floor.to.ceilling.Windows)))
        updateCheckboxGroupInput(session=session, inputId="selected_balcony",
                                 choices = list("yes" = "yes",
                                                "no" = "no"),
                                 selected = c(unique(Apartment$units.with.balcony)))
        updateSliderInput(session=session, inputId = "selected_year",
                          min=1907, max = 2021, value= c(1907, 2021))
        
      }
      else{
        updateCheckboxGroupInput(session=session, inputId="selected_OP",
                                 choices = list("yes" = "yes",
                                                "no" = "no"),
                                 selected = c())
        updateCheckboxGroupInput(session=session, inputId="selected_location",
                                 choices = list("ktown" = "ktown",
                                                "TImeSquare" = "TImeSquare",
                                                "FIDI" = "FIDI",
                                                "Midtown West"= "Midtown West",
                                                "Midtown East"= "Midtown East",
                                                "LIC" = "LIC",
                                                "NJ" ="NJ"),
                                 selected = c())
        updateCheckboxGroupInput(session=session, inputId="selected_laundry",
                                 choices = list("yes" = "yes",
                                                "no" = "no",
                                                "partial" = "partial"),
                                 selected = c())
        updateCheckboxGroupInput(session=session, inputId="selected_Window",
                                 choices = list("yes" = "yes",
                                                "no" = "no"),
                                 selected = c())
        updateCheckboxGroupInput(session=session, inputId="selected_balcony",
                                 choices = list("yes" = "yes",
                                                "no" = "no"),
                                 selected = c())
      }}
  })
  
  
  myapartment <- reactive({
    subset(Apartment, OP %in% input$selected_OP &
             Area %in% input$selected_location &
             Laundry %in% input$selected_laundry &
             floor.to.ceilling.Windows %in% input$selected_Window &
             units.with.balcony %in% input$selected_balcony & 
             built.year >= input$selected_year[1] & 
             built.year <= input$selected_year[2], select = c("Building"))
    
  })
  output$apartments_list <-renderPrint(myapartment())
  
  
  output$apartments_table <- renderDataTable({
    a<- Apartment %>% 
      filter( OP %in% input$selected_OP) %>%
      filter( Area %in% input$selected_location) %>%
      filter( Laundry %in% input$selected_laundry) %>%
      filter( floor.to.ceilling.Windows %in% input$selected_Window) %>%
      filter( units.with.balcony %in% input$selected_balcony) %>%
      filter(built.year >= input$selected_year[1], built.year <= input$selected_year[2])
  })
  
}


shinyApp(ui = ui, server = server)