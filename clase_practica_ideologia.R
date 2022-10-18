rm(list=ls())
require(here)
aqui <- here()

library(foreign) 
library(gdata)

# librerias parametricas y no parametricas para trabajar con datos
# en formatos rollcall

library(wnominate) # Poole et al 2011
library(MCMCpack) # Quinn et al 2011
library(pscl)  # Jackman 2012
library(anominate) # Lo et al 2013
library(oc) # Poole et al 2012

# aqui trabajaremos con los la libreria wnominate
# (weighted nominal three steps estimation)
# supuestos:
# Recordemos, P(vote que y vote Yea en tema j) = P(Uijy-Uijn>0) 
# lo que es igual a P(eijn - eijy < uijy - uijn)
# Por lo tanto, es la probabilidad acumulada de la diferencia de
# los errores.

# Los datos para estimar w-nominate pueden venir en varios formatos
# aqui vemos el caso de una base de datos en formato .csv
# fuente voteview.com

# esta base contiene votaciones de los representantes de naciones unidas

UN <- read.csv(paste0(aqui,"/un.csv"),header=FALSE,strip.white=TRUE)

# preparamos la base de datos
pais <- UN[,1]
atributos <- matrix(UN[,2],length(UN[,2]),1)
colnames(atributos) <- "party"
UN <- UN[,-c(1,2)] # en la parte de datos solo deben quedar los votos

# para correr wnominate se crea el objeto de clase rollcall
# con la opcion F1 pueden ver el detalle de los elementos del objeto
rc <- rollcall(UN,             
               yea=c(1,2,3), # reduce los valores a dos grupos yea/nay
               nay=c(4,5,6),
               missing=c(7,8,9), # todos los otros datos quedan como missing
               notInLegis=0, # vector de ausentes en que seccion
               legis.names=pais,
               legis.data=atributos,
               desc="UN 31 to 33")

result <- wnominate(rc, dims=2, polarity=c(1,1))
summary(result) # el objeto results contiene la estimacion
# APRE: Aggregated Proportional reduction of error.
# GMP: Geometric mean probability
# miden la mejora en la estimacion al pasar de una a dos dimensiones

names(result) # el objeto result contiene 7 objetos
head(result$legislators)
head(result$rollcalls)
result$dimensions
head(result$eigenvalues)
result$beta
result$weights
result$fits

WEIGHT=(result$weights[2])/(result$weights[1]) # peso relativo dado a la segunda dimension
X1 <- result$legislators$coord1D   # primera dimension
X2 <- (result$legislators$coord2D)*WEIGHT # segunda dimension
windows()
plot(X1,X2,type="n",asp=1,
     xlab="1a dimension",
     ylab="2a dimension",
     xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),font=2,cex=1.2)
mtext("Naciones Unidad: 31 - 33",side=3,line=1.50,cex=1.2,font=2)
points(X1[result$legislators$party == "Other"],X2[result$legislators$party == "Other"],pch=16,col="blue",font=2)
points(X1[result$legislators$party == "WP"],X2[result$legislators$party == "WP"],pch=16,col="red",font=2)

# el paquete trae por defecto la posibilidad de un plot de los resultados
windows()
plot(result)

# tambien se puede separar para efectos de publicacion
plot.coords(result) # separa por grupos
plot.scree(result) # peso relativo de cada eigenvalue
plot.cutlines(result, lines=5) # puntos de corte de cada proyecto
plot.angles(result) # histograma de los puntos de corte por angulos

# entonces ahora hagamos lo mismo con datos de Chile
senado <- read.csv(paste0(aqui,"/votos2014_2016_procesado.csv"),sep=";")
nombres <- senado[,1]
senado <- senado[,2:NCOL(senado)]
rc_senado <- rollcall(senado,             
               yea=c(1), # reduce los valores a dos grupos yea/nay
               nay=c(-1),
               missing=c(0), # todos los otros datos quedan como missing
               notInLegis=NULL, # vector de ausentes en que seccion
               legis.names=nombres,
               legis.data=NULL,
               desc="Senado 2014-15")
result_senado <- wnominate(rc_senado, dims=2, polarity=c(35,35))
summary(result_senado) # el objeto results contiene la estimacion
windows()
plot(result_senado)

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

############################################################
############################################################
############################################################
#
#  Estimacion de ideologia via MCMC
#
############################################################
############################################################
############################################################

# Algunos problemas que tiene NOMINATE y su uso:
# Teoricos: en rigor no se pueden usar todos los roll calls para
#   todos los comportamientos politicos (ej. voto sincero vs estrategico)
# Practicos: sofisticar la estimacion de NOMINATE es computacionalmente
#   limitada porque aumenta exponencialmente el tiempo requerido
# Estadisticos: Como NOMINATE requiere la estimacion simultanea de muchos
#   parametros, la base estadistica del modelo parametrico es debil y
#   no se puede usar directamente la estimacion para computar margenes
#   de error.
#
# NOMINATE es un metodo frecuentista. 
# Es decir, se asume que los datos
# son una muestra de un proceso gobernado por parametros fijos que se
# deben estimar (por ej, via Maximum Likelihood). Para ello se sacan
# muestras de los datos y se busca minimizar los errores. 
# Con esta estimacion se busca hacer estimaciones del tipo:
# "el votante mediano es X y su punto ideal se ubica en -0.1 
#  con un margen de error z"

# Jackman y otros
# sugirieron una aproximacion diferente (similar a los desarrollos
# en psicometria o teoria de respuesta al item, IRT). 
# Aqui, lo conocido y fijo son los datos. Lo variable son los parametros
# Luego se asume una distribucion a priori y se aplica Bayes
# para estimar distribuciones posteriores
# Con esta aproximacion se busca hacer estimaciones del tipo:
# "el legislador X tiene una probabilidad mayor de ser el votante
# mediano que el legislador Y"

# Computacionalmente, agregar mas informacion en modelos bayesianos
# es posible porque el proceso de pasar de a-priori a a-posteriori 
# no cambia. Por el contrario en NOMINATE aumenta consistemente
# el numero de parametros a estimar.

# Ejemplo: Con 120 legisladores y 500 proyectos de ley con 1 dimension 
# el numero de parametros es:
#       120*2    # Estimar parametro a favor y en contra de cada parlamentario
#     + 500(1+1) # para cada PdL el parametro x dimension + su saliencia
#    = 1240 parametros. 
# Agregar una segunda dimension implica 500 parametros nuevos a estimar
# Agregar un parlamentario, 240 nuevos parametros, etc.

# Es decir, el numero de parametros aumenta con el tamano de la muestra
# lo que torna inconsistentes las estimaciones de errores

mcmc_e <- ideal(rc, codes = rc$codes,
                maxiter = 8000, burnin = 1000, thin = 500,
                normalize = T) # ver help para detalles

# comparemos resultados:
plot(-mcmc_e$xbar[,1], result$legislators$coord1D, 
     xlab="Estimacion Bayesiana (1Dim)", ylab="W-NOMINATE (1Dim)")
# notese que multiplique por -1 mcmc_e ¿por que es valido hacerlo?

# Y obtenemos magenes de error 

mcmc.df <- as.data.frame(mcmc_e$x)
hist(mcmc.df[,1])
plot(density(mcmc.df[,1]), xlim=c(-1,1))
lines(density(mcmc.df[,2]), col="red")
lines(density(mcmc.df[,4]), col="green")


############################################################
############################################################
############################################################
#
#  Estimacion de ideologia a partir de encuestas - tscores
#
############################################################
############################################################
############################################################

# Hasta ahora hemos visto estimaciones de ideologia con dos opciones
# pero es posible extraer posiciones ideologicas cuando hay + opciones
# Poole (1998) - ranking 

library(basicspace)

base <- read.csv(paste0(aqui,"/ejemplo_basicspace.csv"))
#base <- read.csv("E:/CURSOS/EconPolFormal/2017/ejemplo_basicspace.csv")
muestra <- sample(row.names(base), 1500)  # necesario x limite de algoritmo
base <- base[row.names(base) %in% muestra,]

# calcula ideologia base gps
tscores <- cbind(as.numeric(base$t_allende),
                 as.numeric(base$t_pinochet),
                 as.numeric(base$t_aylwin),
                 as.numeric(base$t_frei),
                 as.numeric(base$t_lagos),
                 as.numeric(base$t_bachelet1),
                 as.numeric(base$t_pinera),
                 as.numeric(base$t_bachelet2))
tscores <- as.data.frame(tscores)
row.names(tscores) <- seq(1:NROW(tscores))
colnames(tscores) <- c("Allende","Pinochet","Aylwin","Frei",
                        "Lagos","Bachelet1","Pinera","Bachelet2")
tscores <- as.matrix(tscores)

results_bb <- blackbox(tscores, 
                       missing=c(8,9), 
                       verbose=T, 
                       dim=2, 
                       minscale=3)

# podemos ver las estimaciones obtenidas
d1 <- unlist(results_bb$individuals[1])
plot(density(d1, na.rm = T), main="Dist Ideol Encuestados")

# para trabajar con los datos los incorporamos a la base
psi.hat <- -results_bb$individuals[[1]]$c1
psi.hat.df <- as.data.frame(psi.hat)
colnames(psi.hat.df) <- c("ideologia")
base$ideologia <- unlist(psi.hat.df) 

# pero no solo podemos extraer la ideologia de encuestados
# tambien podemos estimar la posicion ideologica de los tscores
results_bb.t <- blackbox_transpose(tscores, dims=1,minscale = 3)
ideologia_tscores <- -results_bb.t$stimuli[[1]][2]

plot(density(d1, na.rm = T), main="Dist Ideol Encuestados")
points(ideologia_tscores[1,1],0,col="red", type='p',pch=19)
points(ideologia_tscores[2,1],0,col="blue", type='p',pch=19)
points(ideologia_tscores[3,1],0,col="red", type='p',pch=19)
points(ideologia_tscores[4,1],0,col="red", type='p',pch=19)
points(ideologia_tscores[5,1],0,col="red", type='p',pch=19)
points(ideologia_tscores[6,1],0,col="red", type='p',pch=19)
points(ideologia_tscores[7,1],0,col="red", type='p',pch=19)
points(ideologia_tscores[8,1],0,col="red", type='p',pch=19)
text(ideologia_tscores[1,1],0.3,row.names(ideologia_tscores)[1],cex = 0.7,srt = 90, col="orange")
text(ideologia_tscores[2,1],0.3,row.names(ideologia_tscores)[2],cex = 0.7,srt = 90, col="blue")
text(ideologia_tscores[3,1],0.3,row.names(ideologia_tscores)[3],cex = 0.7,srt = 90, col="orange")
text(ideologia_tscores[4,1],0.3,row.names(ideologia_tscores)[4],cex = 0.7,srt = 90, col="orange")
text(ideologia_tscores[5,1],0.3,row.names(ideologia_tscores)[5],cex = 0.7,srt = 90, col="orange")
text(ideologia_tscores[6,1],0.3,row.names(ideologia_tscores)[6],cex = 0.7,srt = 90, col="orange")
text(ideologia_tscores[7,1],0.3,row.names(ideologia_tscores)[7],cex = 0.7,srt = 90, col="orange")
text(ideologia_tscores[8,1],0.3,row.names(ideologia_tscores)[8],cex = 0.7,srt = 90, col="orange")

############################################################
############################################################
############################################################
#
#  Estimacion de ideologia dinamica
#
############################################################
############################################################
############################################################

# estimaciones MCMC tienen las ventajas mencionadas, pero
# de todos modos es computacionalmente intensiva (i.e. lento)

require(emIRT)
data <- read.csv(paste0(aqui,"/20170127data_integrado_revisado_solovotosefectivos.csv"))

votes <- data[,2:NCOL(data)]
votes <- as.matrix(votes)
J <- NCOL(votes) # secuencia de los fallos (0,1,2,...)

#################################
# para estimacion dinamica necesitamos cuatro set de datos
# data: votos, 
#       periodo de inicio (starts),
#       periodo de termino (ends),
#       numero de fallo (bill.session)
#       numero de fallos/periodo (asumiremos cada fallo es un periodo)

startlegis <- matrix(0,nrow=NROW(votes), ncol=1)
bill.session <- seq(1:NCOL(votes))
bill.session <- bill.session - 1 
bill.session <- as.matrix(bill.session)

endlegis <- matrix(NCOL(votes)-1,nrow=NROW(votes), ncol=1)

panel.data <- list(rc = votes,
                   startlegis = startlegis,
                   endlegis = endlegis, 
                   bill.session = bill.session,
                   T = NCOL(votes)) # cambiar por s

# starts:
# alpha: parametro de dificultad de orden J*1
# beta: parametro de discriminacion de orden J*1
# x: matriz de N*T

alpha <- matrix(0.1,nrow=NCOL(votes), ncol=1) # 0.1 arbitrario
beta <- matrix(-0.1,nrow=NCOL(votes), ncol=1) # -0.1 arbitrario
#x <- matrix(0,nrow=NROW(votes), ncol=s)
x <- matrix(0,nrow=NROW(votes), ncol=NCOL(votes))
starts.points <- list(alpha = alpha, beta = beta, x = x)

# priors:
# x.mu0: promedio inicial para ideal points de cada legislador de orden N*1
# x.sigma0: varianza inicial para cada estimacion
# beta.mu: promedio inicial para todos los bills, de orden 2*1
# beta.sigma: promedio inicial de varianza para todos los bills, de orden 2*2
# omega2: evolucion de la varianza por legislador

#x.mu0 <- result.w$legislators$coord1D # estimacion wnominate como punto de partida
x.mu0 <- matrix(0,nrow=NROW(votes),ncol=1) # siguiendo ejemplo en paquete emIRT
x.sigma0 <- matrix(1, nrow=NROW(votes),ncol=1) # arbitrariamente prior en 1 siguiendo ejemplo en paquete
beta.mu <- matrix(NA,nrow=2,ncol=1)
beta.mu[1,1] <- 0
beta.mu[2,1] <- 0
beta.sigma <- matrix(NA,nrow=2,ncol=2)
beta.sigma[1,1] <- 1
beta.sigma[2,1] <- 0
beta.sigma[1,2] <- 0
beta.sigma[2,2] <- 1
omega2 <- matrix(0.1,nrow=NROW(votes),1) # 0.1 siguiendo ejemplo en paquete

priors.points <- list(x.mu0 = x.mu0,
                      x.sigma0 = x.sigma0,
                      beta.mu = beta.mu,
                      beta.sigma = beta.sigma, 
                      omega2 = omega2)

base <- list(base.data = panel.data,
             base.cur = starts.points, 
             base.priors = priors.points)

result <- dynIRT(.data = base$base.data,
                 .starts = base$base.cur,
                 .priors = base$base.priors)

ideology <- result$means$x
row.names(ideology) <- data[,1]

# Veamos el resultado
ej1 <- 1
ej2 <- 36
ej3 <- 5
ej4 <- 25
ej5 <- 15

plot(ideology[ej1,],
     ylim=c(-1,18),
     col="blue",
     type="l")
text(100,8,row.names(ideology)[ej1], col="blue", cex = 0.75)
lines(ideology[ej2,],
      col="red")
text(100,7,row.names(ideology)[ej2], col="red", cex = 0.75)
lines(ideology[ej3,],
      col="green")
text(100,6,row.names(ideology)[ej3], col="green", cex = 0.75)
lines(ideology[ej4,],
      col="black")
text(100,5,row.names(ideology)[ej4], col="black", cex = 0.75)
lines(ideology[ej5,],
      col="grey")
text(100,4,row.names(ideology)[ej5], col="grey", cex = 0.75)