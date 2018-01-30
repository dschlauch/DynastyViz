setwd("/srv/shiny-server/DynastyViz/")
library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(shiny)


playerDataMerged <- readRDS("./data/playerDataMerged.RDS")
# playerDataMerged$VORP[is.na(playerDataMerged$VORP)] <- 0
playerDataMerged$VORP_adj <- playerDataMerged$VORP*150/max(playerDataMerged$VORP, na.rm = T)
owners <- unique(playerDataMerged$Owner) %>% sort
positions <- c("QB","RB","WR","TE", "D","K","NA")

contract_matrix <- playerDataMerged[,grepl("Salary",colnames(playerDataMerged)),with=F] %>% as.matrix
contract_matrix[contract_matrix==0] <- NA
playerDataMerged$contract_years <- apply(contract_matrix, 1, function(x){sum(!is.na(x))})
playerDataMerged$contract_start <- apply(contract_matrix, 1, function(x){min(which(!is.na(x)))})
playerDataMerged$contract_end <- apply(contract_matrix, 1, function(x){max(which(!is.na(x)))})

current_year <- 4

rowSums(contract_matrix)
# Inferring contracts
playerDataMerged$contract <- NA
playerDataMerged[,
                 contract:=ifelse(
                   contract_years==1 & contract_start==current_year,
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

