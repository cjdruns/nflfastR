library(nflfastR)
library(ggplot2)
library(dplyr)


df <- load_player_stats()

season2020 <- df %>% filter(season == '2020'|season=='2019') %>%
    filter(week>=1 | week <=17)%>%
    select(player_id, player_name, season, attempts, passing_yards, passing_tds, rushing_yards, 
           rushing_tds, targets, receiving_yards, fantasy_points, fantasy_points_ppr)%>%
        group_by(season)

pass_td <- aggregate(passing_tds~season+player_name, data = season2020, sum)
tds <- pass_td %>% filter(passing_tds>15)

hist(tds$passing_tds, breaks = 8, col = 'blue')




tds[order(tds$passing_tds, decreasing = T),]

round(c(var(tds$passing_tds), var(tds$passing_tds)/length(tds$passing_tds),sd(tds$passing_tds),sd(tds$passing_tds)/sqrt(length(tds$passing_tds))),2)


plot(, col = alpha('red',.1))
text(season~passing_tds, data = tds,)
