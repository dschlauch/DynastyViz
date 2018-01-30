
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
    
  tags$head(includeScript("googanal.js")),
  # Application title
  titlePanel("wekf89700wegewg"),

  
  hr(),
  tabsetPanel(
    tabPanel("Players",
             fluidRow(
               column(12,
                      dataTableOutput('table')
               ))
    ),
    tabPanel("Salary vs VORP",
             fluidRow(
               column(3,
                      h4("Filter"),
                      selectInput(
                        'owner', 'Team Owner', choices = c('All','Adam','AMac','Bard','Billy','Brendan','Dan','Decker','Derek','Fishy','Jono','Justin','Nate','Pard','Rob')  ,
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
                                   c("2017 Salary" = "salary",
                                     "Value over replacement" = "vorp"))
               )
             ),
             
             uiOutput("plot.ui", width=1200)
           ),
    tabPanel("Valuations",
             actionButton("value_button", "View valuations")
    )
  )
))
