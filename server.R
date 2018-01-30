library(dplyr)
library(data.table)
library(ggplot2)
library(scales)

library(shiny)
library(DT)

playerDataMerged <- readRDS("./data/playerDataMerged.RDS")
# playerDataMerged$VORP[is.na(playerDataMerged$VORP)] <- 0
playerDataMerged$VORP_adj <- playerDataMerged$VORP*120/max(playerDataMerged$VORP, na.rm = T)
owners <- unique(playerDataMerged$Owner) %>% sort
positions <- c("QB","RB","WR","TE", "D","K","NA")

shinyServer(function(input, output) {
    observeEvent(input$value_button, {
      showNotification("Go fuck yourself", duration = 1)
    })
    output$plot.ui <- renderUI({
            if(input$owner=="All"){
                ownerFilter <- T
            } else {
                ownerFilter <- playerDataMerged$Owner %in% input$owner
            }
            if(input$pos=="All"){
                positionsFilter <- T
            } else {
                positionsFilter <- playerDataMerged$Position %in% input$pos
            }
            displayAll <- input$displayAll=="all"
            if(!displayAll){
                playerDataMerged <- playerDataMerged[positionsFilter&ownerFilter]
            }
            plotOutput("distPlot", width=1200, height = nrow(playerDataMerged)*8+160)
    }) 
  output$table <- DT::renderDataTable(playerDataMerged[order(VORP,decreasing = T),.(
    Player=factor(Player), 
    Owner=factor(Owner), 
    Salary=as.integer(Salary2017), 
    Contract=factor(contract), 
    Expires=as.integer(2013+contract_end)
    )],
    filter = 'top')
  
  output$distPlot <- renderPlot({
      
      if(input$owner=="All"){
          ownerFilter <- T
      } else {
          ownerFilter <- playerDataMerged$Owner %in% input$owner
      }
      if(input$pos=="All"){
          positionsFilter <- T
      } else {
          positionsFilter <- playerDataMerged$Position %in% input$pos
      }
      displayAll <- input$displayAll=="all"
      if(!displayAll){
          playerDataMerged <- playerDataMerged[positionsFilter&ownerFilter]
          highlightNames <- rep(T,nrow(playerDataMerged))
      } else {
          highlightNames <- rep(T,nrow(playerDataMerged))&(ownerFilter & positionsFilter)
      }
      if(input$sortBy=="salary"){
          sortBy <- "Salary2017"
      } else {
          sortBy <- "VORP_adj"
      }
      
      playerDataMerged$Player <- factor(as.character(playerDataMerged$Player), levels=as.character(playerDataMerged[order(playerDataMerged[,sortBy,with=F],na.last=F)]$Player))
      playerDataMerged$shape<-"circle"
      ggplot(playerDataMerged, aes(x=Player)) + 
          geom_point(aes(col=Owner,y=Salary2017),size=3) + 
          geom_point(aes(y=VORP_adj),shape=18,  size=3, alpha=.05+.95*highlightNames) +
          geom_segment(aes(y = Salary2017, xend = Player, yend = VORP_adj), alpha=.1+.4*highlightNames) +
          scale_x_discrete(expand=c(0, 1)) + 
          scale_y_continuous(limits=c(0,155),labels=dollar,sec.axis = dup_axis(),breaks=pretty_breaks(n=10)) + 
          geom_text(aes_string(y=sortBy, label="Player"), alpha=.05+.95*highlightNames, hjust=-.1,angle=0, size=3)  + 
          xlab("Player") + ylab("2017 Salary") +
          labs(colour = "Salary:") + guides(col = guide_legend(nrow = 1, keyheight = 1))+
          theme_bw() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(), legend.position = "top") + coord_flip()
          
  })
})

