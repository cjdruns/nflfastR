library(nflfastR)
library(ggplot2)
library(dplyr)


df <- load_player_stats()

season2020 <- df %>% filter(season == '2020') %>%
    select(player_id, player_name, season, attempts, passing_yards, passing_tds, rushing_yards, 
           rushing_tds, targets, receiving_yards, fantasy_points, fantasy_points_ppr)
player <- season2020 %>% select(player_id, player_name)

att <- aggregate(attempts~player_id, data = season2020, sum)
rush <- aggregate(rushing_yards~player_id, data = season2020, sum)
fp <- aggregate(fantasy_points~player_id, data = season2020, sum)
ppr <- aggregate(fantasy_points_ppr~player_id, season2020, sum)


merge1 <- merge(player, att, by = 'player_id')
merge2 <- merge(merge1, rush, by = 'player_id' )
merge3 <- merge(merge2, fp, by = 'player_id')
player_stats <- unique(merge(merge3, ppr, by = 'player_id'))

player_stats$attempts <- as.numeric(player_stats$attempts)
player_stats$rushing_yards <- as.numeric(player_stats$rushing_yards)

names(player_stats) <- c('player_id', 'player_name', 'att', 'ry', 'fantasy_points', 'fantasy_points_ppr')
 
wrstats <- player_stats %>% filter(att < 5) %>%
    filter(ry > 200 ) %>%
    filter(fantasy_points > 100)


plot(fantasy_points~fantasy_points_ppr, data = wrstats, col = alpha('red', .1))
text(fantasy_points~fantasy_points_ppr, labels = wrstats$player_name, data = wrstats)


recpoints <- wrstats$fantasy_points_ppr-wrstats$fantasy_points
rbstats <- cbind(wrstats, recpoints)

plot(recpoints~fantasy_points, data = rbstats, col = alpha('red', .1))
text(recpoints~fantasy_points, labels = rbstats$player_name, data = rbstats)
