#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinymanager)
library(RSQLite)
library(dplyr)
library(bslib)
library(DT)
library(stringi)
library(shinyalert)
library(bsicons)
library(rclipboard)
source("headers.R")
mainTheme<-bslib::bs_theme(
  bootswatch = "flatly"
)

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(
    navbarPage(
      collapsible = TRUE,
      title = "Приём биоматериала",
      theme = mainTheme,
      tabPanel("Бактериологические исследования",
               AcceptBactUI(mainTheme)),
      tabPanel("Бактериология: посевы",
               InoculateBactUI(mainTheme)),
      tabPanel("Серологические исследования",
               SerolAcceptUi(mainTheme))
    )
  )
)

#ui<-secure_app(theme = mainTheme, ui)

# Define server logic required to draw a histogram
server <- function(input, output) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  AcceptBactServer(mainTheme)
  InoculateBactServer(mainTheme)
  SerolAcceptServer(mainTheme)
}

# Run the application 
shinyApp(ui = ui, server = server)
