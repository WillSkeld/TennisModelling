library(tidyverse)
serve_stats <- charting_m_stats_ServeBasics
serve_stats <- serve_stats %>% separate(match_id, c('match_date', 'gender', 'location', 'round', 'Player1', 'Player2'), sep = "-")

serve_stats_f_1 <- serve_stats %>% filter(row == '1 Total') %>%
  select(-c('match_date', 'gender', 'location', 'round','Player2')) %>% group_by(Player1)

serve_stats_f_2 <- serve_stats %>% filter(row == '2 Total') %>%
  select(-c('match_date', 'gender', 'location', 'round','Player1')) %>% group_by(Player2) %>% rename(Player1 = Player2)

serve_stats_t <- rbind(serve_stats_f_1, serve_stats_f_2) %>% group_by(Player1) %>%
  summarise(
    tpts = sum(pts),
    tpts_won = sum(pts_won),
    taces = sum(aces),
    tunret = sum(unret),
    tforced_err = sum(forced_err),
    tpts_won_lte_3_shots = sum(pts_won_lte_3_shots),
    twide = sum(wide),
    tbody = sum(body),
    ace_pct = taces*100/tpts_won) %>% filter(tpts>=1000)
serve_stats_f<- serve_stats_f %>% group_by(Player1, row)

