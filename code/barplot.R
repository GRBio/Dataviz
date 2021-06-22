load("DatosHM.RData")

library(ggplot2)
library(doBy)
library(dplyr)

summby <- summaryBy(uci_dias ~ edad, d_patients, FUN = median, na.rm = TRUE) %>% 
  filter(!is.nan(uci_dias.median)) 

windows()
ggplot(summby, aes(y=uci_dias.median, x=edad)) + 
  geom_bar(stat="identity", width = 0.7, position = "dodge", fill = "orange2") +
  coord_flip() +
  labs(y="Median ICU days", x ="Age") + 
  scale_x_continuous(limit = c(25,90), breaks = c(30,35,40,45,50,55,60,65,70,75,80,85,90)) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        rect = element_rect(fill = "transparent")) +
  theme(axis.text = element_text(face = "bold")) 

ggsave('Boxplot_ICUdays_vs_age_transp.png', bg = "transparent", dpi=600)
