
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
    
  tags$head(includeScript("googanal.js")),
  # Application title
  titlePanel("Wounded Duck Dynasty: Salary vs VORP"),

  
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
             radioButtons('displayAll', "Display:",
                          c("All"="all","Filter"="filter")),
             radioButtons("sortBy", "Sort by:",
                          c("2016 Salary" = "salary",
                            "Value over replacement" = "vorp"))
        )
  ),
  
  uiOutput("plot.ui", width=1200)
))
