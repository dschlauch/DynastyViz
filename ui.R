
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
    
  tags$head(includeScript("googanal.js")),
  # Application title
  titlePanel("Wounded Duck Dynasty"),

  uiOutput("plot.ui", width=3200),
  
  hr(),
  fluidRow(
      column(3,
             h4("Filter"),
             selectInput(
                 'owner', 'Team Owner', choices = c("All","Adam","Alex","Billy", "Brad","Dan","Brendan","Decker","Derek","Fischetti","Jono","Justin","Nathan","Pealer","Rob")  ,
                 selectize = FALSE
             ),
             selectInput(
                 'pos', 'Position', choices = c("All","QB","RB","WR","TE", "D","K","Long-Snapper")  ,
                 selectize = FALSE
             )
      ),
      column(3,
             checkboxInput('displayAll', "Display All", value = T, width = NULL)
        )
  )
 
))
