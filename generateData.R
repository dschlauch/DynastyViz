# devtools::install_git("https://github.com/jennybc/googlesheets.git")
library(googlesheets)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(rvest)
library(data.table)


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

points_url <- "https://www.fantasypros.com/nfl/reports/leaders/ppr.php?year=2016&week=1:17"
fp <- as.data.table(read_html(points_url) %>% html_table() %>% .[[1]])

intersect(fp$Player, playerSalaries$Player)

mingames <- 5
RB_replace_rank <- 40
WR_replace_rank <- 80
QB_replace_rank <- 28
TE_replace_rank <- 28
K_replace_rank <- 14
DST_replace_rank <- 20

rb_replace <- fp[Position=="RB"&Games>mingames][order(-Avg)][RB_replace_rank,Avg]
wr_replace <- fp[Position=="WR"&Games>mingames][order(-Avg)][WR_replace_rank,Avg]
qb_replace <- fp[Position=="QB"&Games>mingames][order(-Avg)][QB_replace_rank,Avg]
te_replace <- fp[Position=="TE"&Games>mingames][order(-Avg)][TE_replace_rank,Avg]
k_replace <- fp[Position=="K"&Games>mingames][order(-Avg)][K_replace_rank,Avg]
dst_replace <- fp[Position=="DST"&Games>mingames][order(-Avg)][DST_replace_rank,Avg]

replacements <- c("RB"=rb_replace,"WR"=wr_replace,"QB"=qb_replace,"TE"=te_replace,"K"=k_replace,"DST"=dst_replace)

fp$VORP <- (fp$Avg-replacements[fp$Position])*fp$Games
fp$VORP[fp$VORP<0] <- 0
fp[order(-VORP)][1:100]

playerDataMerged <- merge(fp,playerSalaries,by=c("Player","Position"), all.y = T)

# setnames(playerSalaries, "Position.x", "Position")

saveRDS(fp,"./data/fpData.RDS")
saveRDS(playerSalaries,"./data/playerSalaries.RDS")
saveRDS(playerDataMerged,"./data/playerDataMerged.RDS")

# write.table(playerSalaries, "dynasty_data.csv", sep=",", row.names=F)
