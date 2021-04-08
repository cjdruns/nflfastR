library(nflfastR)
library(dplyr)
library(ggplot2)

df <- load_pbp(1999:2020)

passrun <- df %>% select( season, passing_yards, pass_touchdown, rushing_yards, rush_touchdown) %>% 
      group_by(season)
average_season <- aggregate(passing_yards~season, data = passrun, sum)

average_per_team <- function(passing_yards){passing_yards/32}

avperteam <- aggregate(passing_yards~season, data= average_season, average_per_team)
head(avperteam)

passtd <- aggregate(pass_touchdown~season, data = passrun, sum)

avpasstd <- function(passtd){passtd/32}

avpass_td <- aggregate(pass_touchdown~season, data = passtd, avpasstd)

plot( x = avpass_td$season, y = avpass_td$pass_touchdown, type = 'l')


totrunyds <- aggregate(rushing_yards~season, data= passrun, sum)

avrunyards <- function(totrunyds){totrunyds/32}

season_rushyds <- aggregate(rushing_yards~season, data = totrunyds, avrunyards)

totrushtd <- aggregate(rush_touchdown~season, data = passrun, sum) 

avrushtd <- function(totrushtd){totrushtd/32}

season_rushtd <- aggregate(rush_touchdown~season, data = totrushtd, avrushtd)
