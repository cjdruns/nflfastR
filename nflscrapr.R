# package ‘globals’ successfully unpacked and MD5 sums checked
# package ‘listenv’ successfully unpacked and MD5 sums checked
# package ‘parallelly’ successfully unpacked and MD5 sums checked
# package ‘snakecase’ successfully unpacked and MD5 sums checked
# package ‘fastrmodels’ successfully unpacked and MD5 sums checked
# package ‘furrr’ successfully unpacked and MD5 sums checked
# package ‘future’ successfully unpacked and MD5 sums checked
# package ‘janitor’ successfully unpacked and MD5 sums checked
# package ‘progressr’ successfully unpacked and MD5 sums checked
# package ‘tidyr’ successfully unpacked and MD5 sums checked
# package ‘xgboost’ successfully unpacked and MD5 sums checked
# package ‘nflfastR’ successfully unpacked and MD5 sums checked
# 
# The downloaded binary packages are in
# C:\Users\Carl\AppData\Local\Temp\RtmpcDq6GP\downloaded_packages
library(nflfastR)
library(dplyr)
library(ggplot2)

df <- load_pbp(1999:2020)


####### Creates a table of who has most penalty and penalty yards on the Bills in 2020
penalty_df <- select(df, posteam, season, home_team, away_team, penalty_yards, penalty_type, penalty_team, penalty_player_name, series_result)

BUF_pen <- penalty_df %>% filter(home_team =="BUF"| away_team=='BUF')%>%
      filter(penalty_team=='BUF')

BUF_pen_2020 <- BUF_pen %>%
      filter(season=='2020')
BUF_pen_2020 <- BUF_pen_2020[complete.cases(BUF_pen_2020$penalty_type),]
pl_most_pen <- BUF_pen_2020 %>% group_by(penalty_player_name) %>%
      count(penalty_player_name)

most_pen<- pl_most_pen[order(pl_most_pen$n, decreasing=T),c(1,2)]

pl_most_yards <- BUF_pen_2020 %>% group_by(penalty_player_name)
pen_yards<-aggregate(penalty_yards~penalty_player_name,data= pl_most_yards, sum )

most_pen_yards <- pen_yards[order(pen_yards$penalty_yards, decreasing=T),c(1,2)]
stats <- merge(pl_most_pen, pen_yards, 'penalty_player_name')
pen_stats <- stats[order(stats$n, decreasing = T),]
##########


##player with most passing yards per pass in a single game
passing_stats <- df %>% select(game_id, season, season_type, play_type, home_team, away_team, posteam, week, game_date,
                               yards_gained, passer_player_name, pass_attempt, incomplete_pass, complete_pass) %>%
      filter(season_type == 'REG') %>% filter(play_type == 'pass') %>% group_by(game_id)
home_team_pass <- passing_stats %>% filter(home_team == posteam)
total_pass_yards_home <- aggregate(home_team_pass$yards_gained, home_team_pass[names(home_team_pass)[c(1, 11)]], sum)
total_pass_att_home <- aggregate(home_team_pass$pass_attempt, home_team_pass[names(home_team_pass)[c(1,11)]], sum)

pass_home <- merge(total_pass_yards_home,total_pass_att_home, by=c('game_id','passer_player_name'))
pass_home1<- pass_home %>% filter(x.y >= 10)

away_team_pass <- passing_stats %>% filter(away_team == posteam)
total_pass_yards_away <- aggregate(away_team_pass$yards_gained, away_team_pass[names(away_team_pass)[c(1, 11)]], sum)
total_pass_att_away <- aggregate(away_team_pass$pass_attempt, away_team_pass[names(away_team_pass)[c(1,11)]], sum)

pass_away <- merge(total_pass_yards_away,total_pass_att_away, by=c('game_id','passer_player_name'))
pass_away1 <- pass_away %>% filter(x.y >= 10)

passing_stats <- rbind(pass_home1, pass_away1)
yrds_att <- passing_stats$x.x/passing_stats$x.y
passing <- cbind(passing_stats, yrds_att)

head(passing[order(passing$yrds_att, decreasing=T),])

head(passing[order(passing$x.x, decreasing=T),])

head(passing[order(passing$x.y, decreasing=T),])


###############   total epa 2020

epa_pass <- df%>% select(season, epa, play_type, td_player_name, 
                       receiver_player_name, rusher_player_name) %>%
      filter(season==2020)%>%
      filter(play_type=='pass')%>%
      group_by(receiver_player_name)
reciever_epa <- aggregate(epa~receiver_player_name, data=epa_pass, sum)
reciever_epa[order(reciever_epa$epa, decreasing=T),]
reciever_epa[order(reciever_epa$epa, decreasing=F),]

epa_run <- df%>% select(season, epa, play_type, td_player_name, 
                         receiver_player_name, rusher_player_name) %>%
      filter(season==2020)%>%
      filter(play_type=='run')

runner_epa <- aggregate(epa~rusher_player_name, data= epa_run, sum)
head(runner_epa[order(runner_epa$epa, decreasing=T), ], 32)

###########


##########  epa vs targets 

rec <- df %>% select(season, play_type, receiver_player_name, epa, td_player_name) %>%
      filter(season==c('2019', '2020'))%>%
      filter(play_type=='pass')
rec_2019 <- rec %>% filter(season=='2019') 

targ19 <- rec_2019 %>% group_by(receiver_player_name) %>%
      count(receiver_player_name)
targ19 <- targ19 %>% filter(n>=25)
targ19[order(targ19$n, decreasing=T),]

rec_2020 <- rec %>% filter(season=='2020')
targ_2020 <- rec_2020 %>% group_by(receiver_player_name)%>%
      count(receiver_player_name)
targ_2020 <- targ_2020 %>% filter(n>='2020')
targ_2020[order(targ_2020$n, decreasing=T),]


nas <- rec %>% filter(is.na(receiver_player_name ))
head(nas)
