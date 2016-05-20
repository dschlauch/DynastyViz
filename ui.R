
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Wounded Duck Dynasty"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        selectInput(
            'owner', 'Team Owner', choices = c("All","Adam","Alex","Billy", "Brad","Dan","Brendan","Decker","Derek","Fischetti","Jono","Justin","Nathan","Pealer","Rob")  ,
            selectize = FALSE
        )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
