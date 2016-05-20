devtools::install_git("https://github.com/jennybc/googlesheets.git")
library(googlesheets)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggrepel)

gap <- gs_url("https://docs.google.com/spreadsheets/d/1Tni3kzz4kfVTNorAesZWorT-vyt8fBcPyOasTR3qBFs")

wsnames <- gs_ws_ls(gap)[2:15]

allsheets <- list()

while(length(allsheets)<14){
    try({for(sheet in wsnames[!wsnames%in%names(allsheets)]){
        allsheets[[sheet]] <- gs_read(gap,ws = sheet)
    } }, silent = FALSE)
    
}
names(allsheets)

playerSalaries <- data.table(do.call(rbind,sapply(names(allsheets),function(x){
    do.call(cbind,allsheets[[x]][1:6]) %>% cbind("Owner"=x)
})))
names(playerSalaries)[1:6] <- c("Position","Player","Salary2014","Salary2015","Salary2016","Salary2017")
keywords <- c("TOTAL","NET CASH","SPENT","$298","CAP","$300","CAP SPACE")
playerSalaries <- playerSalaries[!Player%in%keywords & !is.na(Player)]
playerSalaries[,3:6] <- lapply(playerSalaries[,3:6, with=F], function(x){as.numeric(gsub("\\$", "", x))})

300*14-sum(playerSalaries$Salary2016)
playerSalaries <- playerSalaries[order(-Salary2016)]
playerSalaries$Player <- factor(playerSalaries$Player, levels = playerSalaries$Player)

saveRDS(playerSalaries,"./playerSalaries.RDS")
