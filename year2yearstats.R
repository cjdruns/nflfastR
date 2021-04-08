library(nflfastR)
library(ggplot2)
library(dplyr)

df <- load_player_stats()
names(df)

targ_epa <- df %>% select(season, player_name, targets, receiving_epa) %>%
    filter(season == '2019') 
total_targets <- aggregate(targets~player_name, data= targ_epa, sum)
total_epa <- aggregate(receiving_epa~player_name, data = targ_epa, sum)

name_targ_epa <- merge(total_targets, total_epa, by = 'player_name')

qualrec <- name_targ_epa %>% filter(targets > 50)

qualrec <- qualrec[order(qualrec$targets, decreasing = T),]
epavtarg <- qualrec$receiving_epa/qualrec$targets
qualrec <- cbind(qualrec, epavtarg)

#########


targ_epa2 <- df %>% select(season, player_name, targets, receiving_epa) %>%
    filter(season == '2020') 
total_targets2 <- aggregate(targets~player_name, data= targ_epa2, sum)
total_epa2 <- aggregate(receiving_epa~player_name, data = targ_epa2, sum)

name_targ_epa2 <- merge(total_targets2, total_epa2, by = 'player_name')

qualrec2 <- name_targ_epa2 %>% filter(targets > 50)


epavtarg2 <- qualrec2$receiving_epa/qualrec2$targets
qualrec2 <- cbind(qualrec2, epavtarg2)

both_seasons <- merge(qualrec, qualrec2, by = 'player_name')
both_seasons <- both_seasons[complete.cases(both_seasons), ]

names(both_seasons)[4] <- 'y'
names(both_seasons)[7] <- 'x'

mod <- lm(y~x, data = both_seasons)
plot(y~x, data = both_seasons, main = 'epa/target 2019 vs 2020', 
     xlab= '2020 epa/targets', ylab = '2019 epa/targets', col= alpha('black', .1))
abline(0,1, col = 'red', lwd = 2) 
text(y~x,  labels = both_seasons$player_name, data = both_seasons)

mod <- lm(receiving_epa~targets, data= qualrec)
plot(receiving_epa~targets, col='blue', data = qualrec, main = 'EPA vs Targets 19-20')
abline(mod, col='red', lwd = 2)
text(receiving_epa~targets, labels = qualrec$player_name, data= qualrec )



targline <- lm(targets.y~targets.x, data = both_seasons)
plot(targets.y~targets.x, data = both_seasons, col= alpha("red", .1), xlab = "2019 Targets", ylab = '2020 Targets', main= '2019 vs 2020 targets')
text(targets.y~targets.x, labels= both_seasons$player_name, data = both_seasons)
abline(0,1, col= 'red')
