library(worldfootballR)
library(tidyverse)

team_balances <- tm_team_transfer_balances(country_name = "England", start_year = 2017)
dplyr::glimpse(team_balances)

write.csv(team_balances, file = "premierleague_transfers_2017.csv", row.names = FALSE)

team_balances %>%
  mutate(net_transfer_income = income_euros - expenditure_euros) %>%
  mutate(blue = net_transfer_income > 0) %>%
  ggplot(aes(x = net_transfer_income, y = squad, fill = blue))+
  geom_col()+
  scale_fill_manual(values = c("firebrick", "darkblue"), name = "Lucrou?")+  
  scale_x_continuous(labels = scales::dollar_format(suffix = "€", prefix = ""), name = "Saldo de transferências")+
  labs(title = "Premier League: Saldo de transferências dos clubes na temporada 2017-2018",
       subtitle = "Na segunda temporada com Guardiola na Premier League, o Manchester City novamente foi o líder nos gastos de transferências",
       caption = c("Fonte: transfermarkt.com\n Inspirado por dontblamethedata.com","@rafaelf_lima"))+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "darkslategrey"),
        panel.grid.major.x = element_line(colour = "white", linetype = 9), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=28, colour = "lightyellow", face="bold"),
        plot.subtitle = element_text(size = 15, colour = "lightyellow", face = "bold"),
        plot.caption = element_text(size = 10, colour = "lightyellow", hjust =c(1,0)),
        plot.title.position = "plot", plot.caption.position = "plot",
        axis.title.x = element_text(colour = "lightyellow", size=14, face = "bold"),
        axis.text.x = element_text(size = 10, colour = "lightyellow"),
        axis.title.y = element_blank(), 
        axis.text = element_text(colour = "lightyellow", size = 14),
        legend.position = "none")

        

  
        