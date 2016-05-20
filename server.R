library(dplyr)
library(data.table)
library(ggplot2)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

playerSalaries <- readRDS("./data/playerSalaries.RDS")

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
      owner <- input$owner
      if(owner=="All"){
          ggplot(playerSalaries, aes(x=Player,y=Salary2016)) + geom_point(aes(col=Owner))  +scale_x_discrete(expand=c(0, 5)) + scale_y_continuous(limits=c(0,125)) + 
              geom_text(aes(label=Player),hjust=-.1,angle=45, size=3) +
              theme_bw() +theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
          
      } else {
      ggplot(playerSalaries, aes(x=Player,y=Salary2016)) + geom_point(aes(col=(Owner==owner)))  +scale_x_discrete(expand=c(0, 5)) + scale_y_continuous(limits=c(0,120)) + 
          geom_text(aes(label=Player, alpha=.05+.95*(Owner==owner)),hjust=-.1,angle=45, size=3) +
          theme_bw() +theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position="none")
      }
  })

})

pdf("dynasty.pdf", width=40)
ggplot(playerSalaries, aes(x=Player,y=Salary2016)) + geom_point(aes(col=Owner))  +scale_x_discrete(expand=c(0, 5)) + scale_y_continuous(limits=c(0,125)) + 
    geom_text(aes(label=Player),hjust=-.1,angle=45, size=3) +
    theme_bw() +theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
dev.off()
