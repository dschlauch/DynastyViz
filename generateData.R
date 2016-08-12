# devtools::install_git("https://github.com/jennybc/googlesheets.git")
library(googlesheets)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggrepel)

gap <- gs_title("Official Dynasty Football Roster Sheet")
# gap <- gs_url("https://docs.google.com/spreadsheets/d/1Tni3kzz4kfVTNorAesZWorT-vyt8fBcPyOasTR3qBFs")
# 
# key <- extract_key_from_url("https://docs.google.com/spreadsheets/d/1Tni3kzz4kfVTNorAesZWorT-vyt8fBcPyOasTR3qBFs")
# ss <- gs_key(key, lookup = FALSE)



wsnames <- gs_ws_ls(gap)[2:15]

allsheets <- list()

starttime <- Sys.time()
while(length(allsheets)<14){
    try({for(sheet in wsnames[!wsnames%in%names(allsheets)]){
        allsheets[[sheet]] <- gs_read(gap,ws = sheet)
    } }, silent = FALSE)
    if(Sys.time()-starttime>300){break} # Give up after 4 minutes
}
names(allsheets)

playerSalaries <- data.table(do.call(rbind,sapply(names(allsheets),function(x){
    do.call(cbind,allsheets[[x]][1:7]) %>% cbind("Owner"=x)
})))
names(playerSalaries)[1:7] <- c("RosterSpot","Player","Position","Salary2014","Salary2015","Salary2016","Salary2017")
keywords <- c("TOTAL","NET CASH","SPENT","$298","CAP","$300","CAP SPACE")
playerSalaries <- playerSalaries[!Player%in%keywords & !is.na(Player)]
salaryColumns <- 4:7
playerSalaries[,salaryColumns] <- lapply(playerSalaries[,salaryColumns, with=F], function(x){as.numeric(gsub("\\$", "", x))})

300*14-sum(playerSalaries$Salary2016)
playerSalaries <- playerSalaries[order(-Salary2016)]
playerSalaries$Player <- factor(playerSalaries$Player, levels = playerSalaries$Player)

saveRDS(playerSalaries,"./data/playerSalaries.RDS")

# write.table(playerSalaries, "dynasty_data.csv", sep=",", row.names=F)
