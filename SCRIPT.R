rm(list=ls())
require(here)
aqui <- here()

library(foreign) 
library(gdata)

# librerias parametricas y no parametricas para trabajar con datos en formatos rollcall

library(wnominate) # Poole et al 2011
library(MCMCpack) # Quinn et al 2011
library(pscl)  # Jackman 2012
library(ellipse)

#Cargo base reco
DB <- read_xlsx(paste0(aqui,"/BASE_FINAL_RECO.xlsx"))

#Preparo Base de Datos
nombres <- DB[,2]
DB$Column1 <- NULL
DB <- DB[,3:NCOL(DB)]
rc <- rollcall(DB,             
                  yea=c(1), # reduce los valores a dos grupos yea/nay
                  nay=c(-1),
                  missing=c(0), # todos los otros datos quedan como missing
                  notInLegis=NULL, # vector de ausentes en que seccion
                  legis.names=nombres,
                  legis.data=NULL,
                  desc="Dip 2018-22")
result_dip <- wnominate(rc, dims=2, polarity=c(35,35))
summary(result_dip)
windows()
plot(result_dip)

WEIGHT=(result_dip$weights[2])/(result_dip$weights[1]) # peso relativo dado a la segunda dimension
X1 <- result_dip$legislators$coord1D   # primera dimension
X2 <- (result_dip$legislators$coord2D)*WEIGHT # segunda dimension

proyecto <- 27
voto <- as.integer(rc$votes[,proyecto])
DL1 <- result_dip$rollcalls[proyecto,7]  # spread en dim 1
DL2 <- result_dip$rollcalls[proyecto,8]  # spread en dim 2
ZM1 <- result_dip$rollcalls[proyecto,9]  # punto medio en dim 1
ZM2 <- result_dip$rollcalls[proyecto,10] # punto medio en dim 2
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
     main="Diputados - WNOMINATE - Proyecto de ley")
segments(xws+N2W, yws-N1W, xws-N2W, yws+N1W, col="black", lwd=2)

