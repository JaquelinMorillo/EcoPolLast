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

#########################################
# Carga de datos y Descriptivos
#########################################

BASE_DESCRIPTIVA <- read_excel("BASE_DESCRIPTIVA.xlsx")
p1 <- read_excel("p1b.xlsx")
p2 <- read_excel("p2.xlsx")
p3 <- read_excel("p3.xlsx")

########################################
#Preparo las bases para las tablas
########################################
lista_p1<-p1[,1:2]

########################################
#Modelo nominate
########################################
#p1

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
result_p1 <- wnominate(rc_p1, dims=1, polarity=c(143))
summary(result_p1) # el objeto results contiene la estimacion
plot(result_p1)

#tabla
lista_p1table<-lista_p1 [,2]

tabla_p1 <-data.frame(lista_p1table,result_p1$legislators, lista_p1$apellido)
tabla_p1 <-tabla_p1[,c(1,6,7,8)]
tabla_p1  <-tabla_p1[order(tabla_p1$coord1D), c(1,2,3,4)]
tabla_p1 $rank <-rank(tabla_p1$coord1D)

tabla1<-xtable(tabla_p1, caption = "Tabla de estimación ideológica, periodo pre-estallido")
tabla1$coord1D <-round(tabla_p1$coord1D,2)
tabla1$GMP <-round(tabla_p1$GMP,2)
tabla1$CC <-round(tabla_p1$CC,2)
htmlTable(tabla1)

lista_p1graph<-lista_p1 [,1]
tabla1b<-data.frame(lista_p1graph$apellido,result_p1$legislators)
tabla1b <-tabla1b[,c(1,6,7,8)]
tabla1b  <-tabla1b[order(tabla1b$coord1D), c(1,2,3,4)]
tabla1b $rank <-rank(tabla1b$coord1D)
tabla1b<-xtable(tabla1b, caption = "Tabla de estimación ideológica, periodo pre-estallido")

#grafico

Plot_p1<-ggplot(tabla1b, aes(x = coord1D, y = rank)) +
  geom_point() +
  geom_line()+
  geom_text(label = tabla1b$lista_p1graph.apellido, nudge_y = 0.3, check_overlap = T) +
  labs(y = "Ranking",
       x = "Coordenadas",
       title= "Estimación Ideológica periodo pre-estallido",
       subtitle = "dimensión",
       caption = "Línea roja: Votante mediano")+
  geom_vline(xintercept = -0.0541298054158688, colour = "red", linetype = "dashed")+
  theme(axis.text = element_text(size = 15),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank())

ggplot2::ggsave(Plot_p1, filename = "Plot_p1.png",dpi = 400, width = 15, height = 20)




WEIGHT=(result_senado$weights[2])/(result_senado$weights[1]) # peso relativo dado a la segunda dimension
X1 <- result_senado$legislators$coord1D   # primera dimension
X2 <- (result_senado$legislators$coord2D)*WEIGHT # segunda dimension


# Veamos el grafico ahora paso a paso

proyecto <- 27
voto <- as.integer(rc_senado$votes[,proyecto])
DL1 <- result_senado$rollcalls[proyecto,7]  # spread en dim 1
DL2 <- result_senado$rollcalls[proyecto,8]  # spread en dim 2
ZM1 <- result_senado$rollcalls[proyecto,9]  # punto medio en dim 1
ZM2 <- result_senado$rollcalls[proyecto,10] # punto medio en dim 2
YEA1 <- ZM1-DL1              # diff entre punto medio y desv en dim 1
YEA2W <- (ZM2-DL2)*WEIGHT    # diff entre punto medio y desv en dim 2
NAY1 <- ZM1+DL1              # idem para votos no en dim 1
NAY2W <- (ZM2+DL2)*WEIGHT    # idem para votos no en dim 2
A1 <- NAY1 - YEA1            # distancia yes-no  
A2 <- NAY2W - YEA2W
ALENGTH <- sqrt(A1*A1 + A2*A2) # radio
N1W <- A1/ALENGTH            # distancia (yes-no)/radio
N2W <- A2/ALENGTH
if (N1W < 0){  # nos aseguramos que sean distancias, por lo tanto, signo +
  N1W <- -N1W
  N2W <- -N2W
}
ws <- N1W*ZM1 + N2W*ZM2*WEIGHT  # pondera pesos
xws <- ws*N1W
yws <- ws*N2W


windows()
plot(X1, X2,
     xlab="1ra dimensión (izquierda-derecha)
     N=En contra, S=A favor",
     ylab="2da Dimensión",
     #col=partidos,
     pch=16,
     main="Senado - WNOMINATE - Proyecto de ley")
segments(xws+N2W, yws-N1W, xws-N2W, yws+N1W, col="black", lwd=2)

# Boostrapping en WNOMINATE
library(ellipse)
result_senado_boot <- wnominate(rc_senado, 
                                ubeta=15, # valores por defecto
                                uweights=0.5, # valores por defecto
                                dims=2, 
                                minvotes=10,
                                lop=0.025, 
                                trials=20, # boots
                                polarity=c(1,5), 
                                verbose=FALSE)
std1 <- result_senado_boot$legislators$se1D
std2 <- result_senado_boot$legislators$se2D * WEIGHT
corr12 <- result_senado_boot$legislators$corr.1

windows()
plot(X1, X2,
     xlab="1ra dimensiÃ³n (izquierda-derecha)
     Puntos: Ideal points; Elipses: Margen de error",
     ylab="2da DimensiÃ³n",
     #col=partidos,
     pch=16,
     main="Ejemplo - WNOMINATE
     Intervalos de confianza de puntos ideales - NM",
     type="n")

for (i in 1:nrow(result_senado_boot$legislators)){
  #  if(!is.na(corr12[i]) & (coaliciones[i]=="red")){
  if(!is.na(corr12[i]) ){  
    lines(c(X1[i],X1[i]), c(X2[i]- 1.96*std2[i], X2[i] + 1.96*std2[i]),col="gray")
    lines(c(X1[i] - 1.96*std1[i], X1[i] + 1.96*std1[i]), c(X2[i], X2[i]),col="gray")
    if (abs(corr12[i]) > .30){
      lines(ellipse(x=corr12[i], scale=c(std1[i],std2[i]),centre=c (X1[i] ,X2[i])), col="gray")
    }
  }
}
#points(X1[coaliciones == "red"], X2[coaliciones == "red"], pch=16, col="red", font=2)
segments(xws+N2W, yws-N1W, xws-N2W, yws+N1W, lwd=2, col="black")
polarity <- X1*N1W + X2*N2W - ws