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

#################################################################
###### Set these parameters each year to update   ###############
#################################################################
first_year <- 2014
contracts_through <- 2021
current_year <- 2017

mingames <- 5
RB_replace_rank <- 40
WR_replace_rank <- 80
QB_replace_rank <- 22
TE_replace_rank <- 35
K_replace_rank <- 14
DST_replace_rank <- 20

wsnames <- gs_ws_ls(gap)[1:14]


year_number <- current_year - first_year + 1

allsheets <- list()


# Google seems to be really resistant to scraping sheets...
# So keep trying until Google lets us in.
starttime <- Sys.time()
while(length(allsheets)<14){
    try({for(sheet in wsnames[!wsnames%in%names(allsheets)]){
        allsheets[[sheet]] <- gs_read(gap,ws = sheet)
    } }, silent = FALSE)
    if(Sys.time()-starttime>300){break} # Give up after 4 minutes
}
names(allsheets)

columns <-c("RosterSpot","Player","Position",paste0("Salary",first_year:contracts_through))
column_count <- length(columns)
playerSalaries <- do.call(rbind,names(allsheets) %>% sapply(function(x){
    do.call(cbind,allsheets[[x]][1:column_count]) %>% cbind("Owner"=x)
})) %>% 
  data.table
names(playerSalaries)[1:column_count] <- columns
keywords <- c("TOTAL","NET CASH","SPENT","$298","CAP","$300","CAP SPACE")
playerSalaries <- playerSalaries[!Player%in%keywords & !is.na(Player)]
salaryColumns <- which(grepl("Salary",columns))
playerSalaries[,salaryColumns] <- lapply(playerSalaries[,salaryColumns, with=F], function(x){as.numeric(gsub("\\$", "", x))})

playerSalaries <- playerSalaries[order(-get(paste0("Salary",current_year)))]
playerSalaries$Player <- factor(playerSalaries$Player, levels = playerSalaries$Player)

points_url <- "https://www.fantasypros.com/nfl/reports/leaders/?year=2017&week=1:17"
fp <- as.data.table(read_html(points_url) %>% html_table() %>% .[[1]])

intersect(fp$Player, playerSalaries$Player)

# Correct Team names
fp$Player[fp$Position=="DST"] <- paste(unlist(lapply(strsplit(fp[Position=="DST",Player],split=" "),tail, 1)), "D/ST")
fp$Position[fp$Position=="DST"] <- "D"

# Correct Michael Thomas
playerSalaries[grepl("thomas la",Player,ignore.case = T)]$Player <- "Mike Thomas"
playerSalaries[grepl("thomas no",Player,ignore.case = T)]$Player <- "Michael Thomas"

rb_replace <- fp[Position=="RB"&Games>mingames][order(-Avg)][RB_replace_rank,Avg]
wr_replace <- fp[Position=="WR"&Games>mingames][order(-Avg)][WR_replace_rank,Avg]
qb_replace <- fp[Position=="QB"&Games>mingames][order(-Avg)][QB_replace_rank,Avg]
te_replace <- fp[Position=="TE"&Games>mingames][order(-Avg)][TE_replace_rank,Avg]
k_replace <- fp[Position=="K"&Games>mingames][order(-Avg)][K_replace_rank,Avg]
dst_replace <- fp[Position=="D"&Games>mingames][order(-Avg)][DST_replace_rank,Avg]

replacements <- c("RB"=rb_replace,"WR"=wr_replace,"QB"=qb_replace,"TE"=te_replace,"K"=k_replace,"D"=dst_replace)

fp$VORP <- (fp$Avg-replacements[fp$Position])*fp$Games
fp$VORP[fp$VORP<0] <- 0
fp[order(-VORP)][1:100]
fp <- fp[!duplicated(Player)]


playerDataMerged <- merge(fp,playerSalaries,by=c("Player"), all.y = T)
playerDataMerged[,Position:=ifelse(is.na(Position.y),Position.x,Position.y)]
# setnames(playerSalaries, "Position.x", "Position")

# Infer contracts
contract_matrix <- playerDataMerged[,grepl("Salary",colnames(playerDataMerged)),with=F] %>% as.matrix
contract_matrix[contract_matrix==0] <- NA
playerDataMerged$contract_years <- apply(contract_matrix, 1, function(x){sum(!is.na(x))})
playerDataMerged$contract_start <- apply(contract_matrix, 1, function(x){min(which(!is.na(x)))})
playerDataMerged$contract_end <- apply(contract_matrix, 1, function(x){max(which(!is.na(x)))})

# Inferring contracts
playerDataMerged$contract <- NA
playerDataMerged[,
                 contract:=ifelse(
                   contract_years==1 & contract_start==year_number,
                   "First year contract",contract
                 )]     
playerDataMerged[,
                 contract:=ifelse(
                   contract_years==7,
                   "4-year extension",contract
                 )]
playerDataMerged[,
                 contract:=ifelse(
                   contract_years==6,
                   "3-year extension",contract
                 )]
playerDataMerged[,
                 contract:=ifelse(
                   contract_years==5,
                   "2-year extension",contract
                 )]
playerDataMerged[,
                 contract:=ifelse(
                   contract_years==4,
                   "RFA Tender",contract
                 )]
playerDataMerged[,
                 contract:=ifelse(
                   contract_years==3,
                   "Initial player contract",contract
                 )]

saveRDS(fp,"./data/fpData.RDS")
saveRDS(playerSalaries,"./data/playerSalaries.RDS")
saveRDS(playerDataMerged,"./data/playerDataMerged.RDS")

# write.table(playerSalaries, "dynasty_data.csv", sep=",", row.names=F)

# Trying to calc next year values
playerDataMerged[!is.na(Salary2018),est2018:=Salary2018]
playerDataMerged[is.na(Salary2016),est2018:=Salary2017+ifelse(Salary2017<10,1,round(.1*Salary2017))]
