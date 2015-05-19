library(shiny)
library(DT)
shinyUI(fluidPage(DT::dataTableOutput('tbl')))
