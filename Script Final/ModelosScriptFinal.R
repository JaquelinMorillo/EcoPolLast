rm(list=ls())
require(here)
aqui <- here()

library(wnominate) # Poole et al 2011
library(MCMCpack) # Quinn et al 2011
library(pscl)  # Jackman 2012
#library(anominate) # Lo et al 2013
#library(oc) # Poole et al 2012
library(xtable)
library(htmlTable)
library(ggplot2)
library(htmlTable)
library(kableExtra)
library(magick)
library(ellipse) 
library(dplyr)
library(ggplot2) 
library(ddplot)
library(lessR)

#########################################
# Carga de datos 
#########################################

BASE_DESCRIPTIVA <- read_excel("~/Documents/GitHub/EcoPolLast/Bases finales/BASE_DESCRIPTIVA.xlsx")
p1 <- read_excel("~/Documents/GitHub/EcoPolLast/Bases finales/p1b.xlsx")
p2 <- read_excel("~/Documents/GitHub/EcoPolLast/Bases finales/p2.xlsx")
p3 <- read_excel("~/Documents/GitHub/EcoPolLast/Bases finales/p3.xlsx")
Partidos <- read_excel("~/Documents/GitHub/EcoPolLast/Bases finales/Partidos.xlsx")


#########################################
# Descriptivos
#########################################
#PARTIDOS
graph_partidos<-ggplot(data=Partidos, aes(x=Partidos$partido, y=Partidos$cantidad, fill=Partidos$cantidad)) + 
  geom_bar(width=.8, stat="identity") +
  guides(fill=FALSE) +
  xlab("Partidos") + ylab("Diputados") +
  ggtitle("Distribución del congreso por partidos - 2018")


########################################2######################################
#Modelo P1
###############################################################################

#Preparo la base para la tabla

lista_p1<-p1[,1:2]


#P1 Modelo nominate

nombres1 <- p1[,2]
p1<- p1[,3:NCOL(p1)]
rc_p1 <- rollcall(p1,             
                      yea=1, # reduce los valores a dos grupos yea/nay
                      nay=0,
                      missing=NA, # todos los otros datos quedan como missing
                      notInLegis=NULL, # vector de ausentes en que seccion
                      legis.names=nombres1,
                      legis.data=NULL,
                      desc="periodo pre estallido")
result_p1 <- wnominate(rc_p1, dims=1, polarity=c(135))
summary(result_p1) # el objeto results contiene la estimacion
plot(result_p1)

#tabla ## Anexo
lista_p1table<-lista_p1 [,2]

tabla_p1 <-data.frame(lista_p1table,result_p1$legislators, lista_p1$Surnames)
tabla_p1 <-tabla_p1[,c(1,6,7,8)]
tabla_p1  <-tabla_p1[order(tabla_p1$coord1D), c(1,2,3,4)]
tabla_p1 $rank <-rank(tabla_p1$coord1D)

tabla1<-xtable(tabla_p1, caption = "Ideological estimation pre-social outburst period")
tabla1$coord1D <-round(tabla_p1$coord1D,2)
tabla1$GMP <-round(tabla_p1$GMP,2)
tabla1$CC <-round(tabla_p1$CC,2)
htmlTable(tabla1)%>%
      save_kable(file = "~/Documents/GitHub/EcoPolLast/Results/tablaP1.png")
lista_p1graph<-lista_p1 [,1]
tabla1b<-data.frame(lista_p1graph$Surnames,result_p1$legislators)
tabla1b <-tabla1b[,c(1,6,7,8)]
tabla1b  <-tabla1b[order(tabla1b$coord1D), c(1,2,3,4)]
tabla1b $rank <-rank(tabla1b$coord1D)
tabla1b<-xtable(tabla1b, caption = "Ideological estimation pre-social outburst period")

#grafico

Plot_p1<-ggplot(tabla1b, aes(x = coord1D, y = rank)) +
  geom_point(size =1, aes(color = rank)) +scale_color_gradient(low = "blue", high = "red")+
  guides(color="none")+
  #geom_line(linetype = "dotted" , size = 1,  aes(alpha=0.4))+
  geom_text(label = tabla1b$lista_p1graph.Surnames, nudge_y = 0.3, check_overlap = T) +
  labs(y = "Ranking",
       x = "Coordenates",
       title= "Ideological estimation pre-social outburst period",
       subtitle = "Left-right dimension",
       caption = "Yellow line: median voter")+
  geom_vline(xintercept = 0.0512404703, colour = "orange", linetype = "dashed")+
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())
  

ggplot2::ggsave(Plot_p1, filename = "~/Documents/GitHub/EcoPolLast/Results/Plot_p1.png",dpi = 500, width = 15, height = 30)

########################################2######################################
#Modelo P2
###############################################################################

#Preparo la base para la tabla

lista_p2<-p2[,1:2]


#P1 Modelo nominate

nombres2 <- p2[,2]
p2<- p2[,3:NCOL(p2)]
rc_p2 <- rollcall(p2,             
                  yea=1, # reduce los valores a dos grupos yea/nay
                  nay=0,
                  missing=NA, # todos los otros datos quedan como missing
                  notInLegis=NULL, # vector de ausentes en que seccion
                  legis.names=nombres2,
                  legis.data=NULL,
                  desc="periodo durante estallido")
result_p2 <- wnominate(rc_p2, dims=1, polarity=c(135))
summary(result_p2) # el objeto results contiene la estimacion
plot(result_p2)

#tabla ## Anexo
lista_p2table<-lista_p2 [,2]

tabla_p2 <-data.frame(lista_p2table,result_p2$legislators, lista_p2$Surnames)
tabla_p2 <-tabla_p2[,c(1,6,7,8)]
tabla_p2  <-tabla_p2[order(tabla_p2$coord1D), c(1,2,3,4)]
tabla_p2 $rank <-rank(tabla_p2$coord1D)

tabla2<-xtable(tabla_p2, caption = "Ideological estimation during social outburst period")
tabla2$coord1D <-round(tabla_p2$coord1D,2)
tabla2$GMP <-round(tabla_p2$GMP,2)
tabla2$CC <-round(tabla_p2$CC,2)
htmlTable(tabla2)%>%
  save_kable(file = "~/Documents/GitHub/EcoPolLast/Results/tablaP2.png")
lista_p2graph<-lista_p2 [,1]
tabla2b<-data.frame(lista_p2graph$Surnames,result_p2$legislators)
tabla2b <-tabla2b[,c(1,6,7,8)]
tabla2b  <-tabla2b[order(tabla2b$coord1D), c(1,2,3,4)]
tabla2b $rank <-rank(tabla2b$coord1D)
tabla2b<-xtable(tabla2b, caption = "Ideological estimation during social outburst period")


#grafico

Plot_p2<-ggplot(tabla2b, aes(x = coord1D, y = rank)) +
  geom_point(size =1, aes(color = rank)) +scale_color_gradient(low = "blue", high = "red")+
  guides(color="none")+
  #geom_line(linetype = "dotted" , size = 1,  aes(alpha=0.4))+
  geom_text(label = tabla2b$lista_p2graph.Surnames, nudge_y = 0.3, check_overlap = T) +
  labs(y = "Ranking",
       x = "Coordenates",
       title= "Ideological estimation during social outburst period",
       subtitle = "Left-right dimension",
       caption = "Yellow line: median voter")+
  geom_vline(xintercept = -0.04451257, colour = "orange", linetype = "dashed")+
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())


ggplot2::ggsave(Plot_p2, filename = "~/Documents/GitHub/EcoPolLast/Results/Plot_p2.png",dpi = 500, width = 15, height = 30)


########################################2######################################
#Modelo P3
###############################################################################

#Preparo la base para la tabla

lista_p3<-p3[,1:2]


#P1 Modelo nominate

nombres3 <- p3[,2]
p3<- p3[,3:NCOL(p3)]
rc_p3 <- rollcall(p3,             
                  yea=1, # reduce los valores a dos grupos yea/nay
                  nay=0,
                  missing=NA, # todos los otros datos quedan como missing
                  notInLegis=NULL, # vector de ausentes en que seccion
                  legis.names=nombres3,
                  legis.data=NULL,
                  desc="periodo pandemia")
result_p3 <- wnominate(rc_p3, dims=1, polarity=c(143))
summary(result_p3) # el objeto results contiene la estimacion
plot(result_p3)

#tabla ## Anexo
lista_p3table<-lista_p3 [,2]

tabla_p3 <-data.frame(lista_p3table,result_p3$legislators, lista_p3$Surnames)
tabla_p3 <-tabla_p3[,c(1,6,7,8)]
tabla_p3  <-tabla_p3[order(tabla_p3$coord1D), c(1,2,3,4)]
tabla_p3 $rank <-rank(tabla_p3$coord1D)

tabla3<-xtable(tabla_p3, caption = "Ideological estimation during SARS-CoV-2 pandemic")
tabla3$coord1D <-round(tabla_p3$coord1D,2)
tabla3$GMP <-round(tabla_p3$GMP,2)
tabla3$CC <-round(tabla_p3$CC,2)
htmlTable(tabla3)%>%
  save_kable(file = "~/Documents/GitHub/EcoPolLast/Results/tablaP3.png")
lista_p3graph<-lista_p3 [,1]
tabla3b<-data.frame(lista_p3graph$Surnames,result_p3$legislators)
tabla3b <-tabla3b[,c(1,6,7,8)]
tabla3b  <-tabla3b[order(tabla3b$coord1D), c(1,2,3,4)]
tabla3b $rank <-rank(tabla3b$coord1D)
tabla3b<-xtable(tabla3b, caption = "Ideological estimation during SARS-CoV-2 pandemic")

#grafico

Plot_p3<-ggplot(tabla3b, aes(x = coord1D, y = rank)) +
  geom_point(size =1, aes(color = rank)) +scale_color_gradient(low = "blue", high = "red")+
  guides(color="none")+
  #geom_line(linetype = "dotted" , size = 1,  aes(alpha=0.4))+
  geom_text(label = tabla3b$lista_p3graph.Surnames, nudge_y = 0.3, check_overlap = T) +
  labs(y = "Ranking",
       x = "Coordenates",
       title= "Ideological estimation during SARS-CoV-2 pandemic",
       subtitle = "Left-right dimension",
       caption = "Yellow line: median voter")+
   geom_vline(xintercept = -0.3934787512, colour = "orange", linetype = "dashed")+
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())


ggplot2::ggsave(Plot_p3, filename = "~/Documents/GitHub/EcoPolLast/Results/Plot_p3.png",dpi = 500, width = 15, height = 23)
