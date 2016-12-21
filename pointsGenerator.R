library(dplyr)
library(rvest)
library(data.table)

points_url <- "https://www.fantasypros.com/nfl/reports/leaders/ppr.php?year=2016&week=1:17"
fp <- as.data.table(read_html(points_url) %>% html_table() %>% .[[1]])

intersect(fp$Player, playerSalaries$Player)

mingames <- 5
RB_replace_rank <- 40
WR_replace_rank <- 80
QB_replace_rank <- 20
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

merge(fp,playerSalaries,by="Player")
