library(tidyverse)
library(ggplot2)
shot_type_stats <- charting_m_stats_ShotTypes
shot_type_stats <- shot_type_stats %>% separate(match_id, c('match_date', 'gender', 'location', 'round', 'Player1', 'Player2'), sep = "-")

shot_type_stats_f_1 <- shot_type_stats %>%
  select(-c('match_date', 'gender', 'location', 'round', 'Player2', 'shots_in_pts_lost')) %>% filter(player == 1) %>% group_by(Player1)

shot_type_stats_f_2 <- shot_type_stats %>%
  select(-c('match_date', 'gender', 'location', 'round','Player1', 'shots_in_pts_lost')) %>% filter(player == 1) %>%
  group_by(Player2) %>% rename(Player1 = Player2)

shot_type_stats_t <- rbind(shot_type_stats_f_1, shot_type_stats_f_2) %>% group_by(Player1) %>% aggregate(.~Player1 +row, FUN=sum) %>%
  select(-c('player'))

shot_type_stats_s <- shot_type_stats_t %>% filter(row == 'Total' | row == 'Base' | row == 'Gs') %>% group_by(Player1) %>%
  summarise(
    tshts = shots[row == 'Total'],
    tBase = shots[row == 'Base'],
    tGs = shots[row == 'Gs'],
    Base_pct = tBase*100 / tshts,
    Gs_pct = tGs*100 / tshts,
    Net_pct = 100 - Base_pct
  ) %>% filter(tshts>3500)

shot_type_stats_s %>% ggplot(aes(x = Base_pct, y = Gs_pct)) + geom_point()
