library(nflfastR)
library(ggplot2)
library(dplyr)

df <- load_pbp(2020)

data1 <- df %>% select(week, season_type, play_type, yards_gained, td_player_name, 
                        td_player_id, receiver_player_name, receiver_player_id,
                        rusher_player_id, rusher_player_name)
data2 <- data1 %>% filter(season_type== 'REG')
plus40 <- data2 %>% filter(yards_gained>=40)

runplus40 <- plus40 %>% filter(play_type =='run')%>%
    group_by(rusher_player_id)%>%
    count(rusher_player_id)
names(runplus40)<- c('player_id', 'runs')
runtd <- plus40 %>% filter(play_type== 'run')%>%
    group_by(td_player_id)%>%
                 count(td_player_id)
runtd<- runtd[complete.cases(runtd),]
names(runtd) <- c('player_id', 'rush_td')

recplus40 <- plus40 %>% filter(play_type =='pass')%>%
    group_by(receiver_player_id)%>%
    count(receiver_player_id)
recplustd <- plus40 %>% filter(play_type =='pass')%>%
    group_by(td_player_id)%>%
    count(td_player_id)
names(recplus40) <- c('player_id', 'rec')
rectd <- recplustd[complete.cases(recplustd),]
names(rectd)<- c("player_id",'rec_td')

recnames <- plus40 %>% select(receiver_player_id, receiver_player_name)
names(recnames) <- c('player_id', 'player_name')

rushname <- plus40 %>% select(rusher_player_id, rusher_player_name)
names(rushname)<- c('player_id', 'player_name')
rushdt <- merge(runplus40, runtd,by = 'player_id' , all = T)
rushtable <- merge(rushdt, rushname, 'player_id')
rushtable <- unique(rushtable)

rush_table <- rushtable[order(rushtable$runs, decreasing = T),]
write.csv(rush_table,"Rush_table")

recdt <- merge(rectd, recplus40, 'player_id' ,all = T)
rectable <- merge(recdt, recnames, 'player_id', all=T)
rectable<- unique(rectable)
rec_table<- rectable[order(rectable$rec, decreasing = T),]
write.csv(rec_table, 'Rec_table')
