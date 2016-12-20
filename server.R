library(dplyr)
library(data.table)
library(ggplot2)
library(scales)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

playerSalaries <- readRDS("./data/playerSalaries.RDS")
owners <- c("Adam","Alex","Billy", "Brad","Dan","Brendan","Decker","Derek","Fischetti","Jono","Justin","Nathan","Pealer","Rob")
positions <- c("QB","RB","WR","TE", "D","K","NA")

shinyServer(function(input, output) {

    output$plot.ui <- renderUI({
            if(input$owner=="All"){
                ownerFilter <- T
            } else {
                ownerFilter <- playerSalaries$Owner %in% input$owner
            }
            if(input$pos=="All"){
                positionsFilter <- T
            } else {
                positionsFilter <- playerSalaries$Position %in% input$pos
            }
            displayAll <- input$displayAll
            if(!displayAll){
                playerSalaries <- playerSalaries[positionsFilter&ownerFilter]
            }
            plotOutput("distPlot", width=800, height = nrow(playerSalaries)*8+100)
        
        
    })
  output$distPlot <- renderPlot({
      
      if(input$owner=="All"){
          ownerFilter <- T
      } else {
          ownerFilter <- playerSalaries$Owner %in% input$owner
      }
      if(input$pos=="All"){
          positionsFilter <- T
      } else {
          positionsFilter <- playerSalaries$Position %in% input$pos
      }
      displayAll <- input$displayAll
      if(!displayAll){
          playerSalaries <- playerSalaries[positionsFilter&ownerFilter]
          highlightNames <- rep(T,nrow(playerSalaries))
      } else {
          highlightNames <- rep(T,nrow(playerSalaries))&(ownerFilter & positionsFilter)
      }
      # if(owner=="All"){
      ggplot(playerSalaries, aes(x=reorder(Player, Salary2016),y=Salary2016)) + geom_point(aes(col=Owner)) + scale_x_discrete(expand=c(0, 1)) + 
          scale_y_continuous(limits=c(0,155),labels=dollar,sec.axis = dup_axis(),breaks=pretty_breaks(n=10)) + 
          geom_text(aes(label=Player, alpha=.05+.95*highlightNames),hjust=-.1,angle=0, size=3)  + 
          xlab("Player") + ylab("2016 Salary") +
          theme_bw() +theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(), legend.position="none") + coord_flip()
          
      # } else {
      #     ggplot(playerSalaries, aes(x=Player,y=Salary2016)) + geom_point(aes(col=(Owner%in%owner)))  +scale_x_discrete(expand=c(0, 5)) + scale_y_continuous(limits=c(0,155)) + 
      #         geom_text(aes(label=Player, alpha=.05+.95*(Owner==owner)),hjust=-.1,angle=45, size=3) +
      #         theme_bw() +theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position="none")
      # }
  })

})

# pdf("dynasty.pdf", width=40)
# ggplot(playerSalaries, aes(x=Player,y=Salary2016)) + geom_point(aes(col=Owner))  +scale_x_discrete(expand=c(0, 5)) + scale_y_continuous(limits=c(0,125)) + 
#     geom_text(aes(label=Player),hjust=-.1,angle=45, size=3) +
#     theme_bw() +theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# dev.off()
