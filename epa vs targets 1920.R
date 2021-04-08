library(nflfastR)
library(ggplot2)
library(dplyr)

df <- load_player_stats()
names(df)

targ_epa <- df %>% select(season, player_name, targets, receiving_epa) %>%
    filter(season == c('2019', '2020')) 
total_targets <- aggregate(targets~player_name, data= targ_epa, sum)
total_epa <- aggregate(receiving_epa~player_name, data = targ_epa, sum)

name_targ_epa <- merge(total_targets, total_epa, by = 'player_name')

qualrec <- name_targ_epa %>% filter(targets > 125)

qualrec <- qualrec[order(qualrec$targets, decreasing = T),]

mod <- lm(receiving_epa~targets, data= qualrec)
plot(receiving_epa~targets, col='blue', data = qualrec, main = 'EPA vs Targets 19-20')
abline(mod, col='red', lwd = 2)
text(receiving_epa~targets, labels = qualrec$player_name, data= qualrec )


