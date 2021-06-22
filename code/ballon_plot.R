setwd('/Users/dfmartinez/Dropbox/MyMemberships/GRBIO/01.Divulga/01.Infography/02.Multivariate/Dades')
load('copy2_of_datos.RData')

library(vcd)
library(ggplot2)
library(dplyr)

d_patients$Outcome <- factor(ifelse(d_patients$motivo_alta=='Fallecimiento','Fallecimiento',
                                    ifelse(d_patients$motivo_alta %in% c('Domicilio','Alta Voluntaria'),'Alta',
                                           ifelse(d_patients$motivo_alta %in% 
                                                    c('Traslado a un Centro Sociosanitario','Traslado al Hospital'),
                                                  'Traslado',NA))),
                             levels=c('Fallecimiento','Alta','Traslado'))

d_patients$espec_brief <- as.factor(ifelse(d_patients$especialidad=='Medicina General','Medicina General',
       ifelse(d_patients$especialidad=='Medicina Interna','Medicina Interna',
              ifelse(d_patients$especialidad=='Medicina de Urgencias','Medicina de Urgencias','Others'))))

d_patients$diag_urg_brief <- as.factor(#ifelse(d_patients$diag_urg=='MALESTAR GENERAL','Malestar General',
                                       ifelse(d_patients$diag_urg=='TOS','Tos',
                                       ifelse(d_patients$diag_urg=='CUADRO CATARRAL','Cuadro catarral',
                                       ifelse(d_patients$diag_urg=='FIEBRE','Fiebre',
                                       ifelse(d_patients$diag_urg=='DIFICULTAD RESPIRATORIA','Dif. Respirat','Others')))))#)                                             
table(d_patients$motivo_alta)
table(d_patients$Outcome)
table(d_patients$sexo)
table(d_patients$espec_brief)
table(d_patients$diag_urg_brief)


library(ggpubr) #ggballon
library(ggtext) #color in totals
library(glue) #color in totals

highlight <- function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}

# 2 variables categoriques

df <- d_patients %>% select(c(Outcome,diag_urg_brief))

(crosstab <- table(df))
crosstabs_margin <- addmargins(crosstab)

rownames(crosstabs_margin) <- c("Death", "Discharge","Transfer","Total Diagnosis")
colnames(crosstabs_margin) <- c("Cold Stage","Breathing Diff.","Fever",
                                   "Others","Cough","Total Status")
#sorted the columns
crosstabs_margin <- crosstabs_margin[,c("Cold Stage","Breathing Diff.","Fever","Cough",
                    "Others","Total Status")]
#sorted the rows
crosstabs_margin <- crosstabs_margin[c("Total Diagnosis","Death", "Discharge","Transfer"),]


crosstabs_margin_df <- as.data.frame(crosstabs_margin)

sp <- ggballoonplot(crosstabs_margin_df, 
              x = "diag_urg_brief", y = "Outcome", size = "Freq",
              fill = "value",
              ggtheme = theme_bw() )+ #, show.label = TRUE)  +
  scale_fill_viridis_c(option = "C", direction=-1) +
theme(panel.background=element_blank(), 
      panel.border = element_rect(colour = "blue", fill=NA, size=1)) +
  ggtitle("Status vs Diagnosis in Emergency Room") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Diagnosis in Emergency Room") +
  ylab("Status") + # Add Frequency Values Next to the circles
  #geom_text(aes(label=Freq), alpha=1.0, size=4, nudge_y = 0.40) 
  guides(size = FALSE) +
  scale_size_area(max_size=30) +
  scale_x_discrete(labels=function(x) highlight(x, "Total Status", "black")) +
  theme(axis.text.x=element_markdown(size=8)) +
  scale_y_discrete(labels=function(x) highlight(x, "Total Diagnosis", "black")) +
  theme(axis.text.y=element_markdown(size=8)) 
  

##adding lines
sp2<- sp + geom_vline(xintercept = 5.35, linetype="solid", color = "blue", size=0.5)
sp2 + geom_hline(yintercept=1.5, linetype="solid", color = "blue", size=0.5) +
  theme(rect = element_rect(fill = "transparent")) +
  ggsave('balloon_transp.png', bg = "transparent", dpi=1000, 
         width = 7, height = 5)
  ggsave('balloon.png', dpi=1000, width = 7, height = 5)


