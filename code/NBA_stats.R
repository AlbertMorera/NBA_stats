

library(tidyverse)
library(rvest)

e_conf <- "https://www.basketball-reference.com/leagues/NBA_2022.html" %>%
  read_html %>%
  html_node("#all_confs_standings_E") %>% 
  html_table() %>%
  mutate(conference = "E") %>%
  rename(team = `Eastern Conference`)

w_conf <- "https://www.basketball-reference.com/leagues/NBA_2022.html" %>%
  read_html %>%
  html_node("#all_confs_standings_W") %>% 
  html_table() %>%
  mutate(conference = "W") %>%
  rename(team = `Western Conference`)

e_w_conf <-bind_rows(e_conf, w_conf) %>%
  mutate(team = gsub("\\s\\(\\d+\\)", "", team),
         conference = factor(conference, levels=c("W", "E")))


for(month in c("october", "november", "december", "january")){
  
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_2022_games-", month, ".html")
  
  monthly_schedule <- url %>%
    read_html %>%
    html_node("#schedule") %>% 
    html_table()
  
  ifelse(month == "october",
         schedule <- monthly_schedule,
         schedule <- rbind(schedule, monthly_schedule))
  
}


visitor <- schedule %>%
  select(3,4) %>%
  rename(visitor = `Visitor/Neutral`, pts_v = PTS)

home <- schedule %>%
  select(5,6) %>%
  rename(home = `Home/Neutral`, pts_h = PTS)

results <- bind_cols(visitor, home) %>%
  mutate(winner = ifelse(pts_v > pts_h, visitor, home),
         looser = ifelse(pts_v > pts_h, home, visitor))

for(team in unique(results$visitor)){
  
  results_team <- results %>%
    filter(looser == team | winner == team) %>%
    mutate(w = ifelse(winner == team, 1, 0)) %>%
    mutate(w = cumsum(w)) %>%
    mutate(l = ifelse(winner != team, 1, 0)) %>%
    mutate(l = cumsum(l)) %>%
    mutate(team = team) %>%
    select(team, w, l)
  
  ifelse(team == unique(results$visitor)[1],
         WL <- results_team,
         WL <- rbind(WL, results_team))
}

WL <- WL %>%
  left_join(e_w_conf %>% select(team, conference), by="team") %>%
  mutate(team = tolower(team),
         team = gsub(" ", "-", team))

WL_final <- WL %>%
  mutate(games = w + l) %>%
  group_by(team) %>%
  filter(games == max(games)) %>%
  select(-games) %>%
  mutate(name = team) %>%
  mutate(name = replace(name, name == "los-angeles-clippers", "la-clippers"),
         name = replace(name, name == "minnesota-timberwolves", "timberwolves"),
         name = replace(name, name == "new-orleans-pelicans", "pelicans"),
         name = replace(name, name == "philadelphia-76ers", "76ers"),
         name = replace(name, name == "san-antonio-spurs", "spurs"),
         name = replace(name, name == "golden-state-warriors", "warriors"))

img <- WL_final %>%
  bind_cols(url = paste0("https://logotyp.us/files/", unique(WL_final$name), ".svg"))

img <- img %>%
  mutate(ratio = w/(l+w)) %>%
  arrange(conference, ratio) %>%
  bind_cols(position = rep(15:1,2))

WL <- WL %>%
  left_join(img %>% select(team, position), by="team")

conf.labs <- c("Eastern Conference", "Western Conference")
names(conf.labs) <- c("E", "W")

mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Spectral"))(15)

sysfonts::font_add_google("Graduate")
showtext::showtext_auto()

p <- ggplot(WL)+
  geom_abline(intercept = 0, slope = 1, lty="dotted", alpha=.25)+
  geom_line(aes(l, w, group=team), alpha=.2) +
  geom_point(aes(l, w), col="white") +
  geom_point(aes(l, w, col=as.factor(position)), alpha=.5) +
  scale_color_manual(values = mycolors) +
  ggimage::geom_image(data = img, aes(l, w, image = url), size = 0.07)+
  labs(x = "Games lost", y = "Games won",
       caption = "Chart by @_AlbertMorera")+
  coord_equal(x=c(1,40), y=c(1,40))+
  facet_wrap(~conference, labeller = labeller(conference = conf.labs)) +
  theme_minimal() + 
  theme(panel.border = element_rect(color="black", fill=NA),
        legend.position = "none",
        text = element_text(family = "Graduate", size = 50),
        plot.subtitle = element_text(margin = margin(20,0,20,0)),
        strip.text = element_text(size=90, margin = margin(20,0,20,0)),
        plot.caption = element_text(family = "mono", size = 30))


ggsave("figures/NBA.png", p, width = 15, height = 8, units = "in", dpi = 300)

