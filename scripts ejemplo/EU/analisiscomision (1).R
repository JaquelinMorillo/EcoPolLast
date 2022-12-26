rm(list=ls())
require(here) 
aqui <- here()

library(foreign) # importar datos
library(gdata) # manipulación de datos

# librerias parametricas y no parametricas para trabajar con datos
# en formatos rollcall


library(wnominate) # Poole et al 2011
library(MCMCpack) # Quinn et al 2011 Cadenas de Markov
library(pscl)  # Jackman 2012
library(anominate) # Lo et al 2013 bootstrapping
library(oc) # Poole et al 2012


UN <- read.csv(paste0(aqui,"/votos_comision_Sistemas de Conocimientos.csv"),header=FALSE,strip.white=TRUE)
UN <- UN[-c(1),]

# preparamos la base de datos
nombreperso <- UN[,2] #saca la coluna de los nombres proique el software no la necesita, la dejamos de lado ya que se ocupara despúes
#atributos <- matrix(UN[,2],length(UN[,2]),1)
#colnames(atributos) <- "party"
UN <- UN[,-c(1,2)] # en la parte de datos solo deben quedar los votos
#UN <- UN[-c(1),]

rc <- rollcall(UN,       # este rollcall es la forma en que wnominate necesita que le entreguemos los datos.      
               yea=1, # reduce los valores a dos grupos yea/nay. los yea son los si votó
               nay=0, # los nay son los votó en contra
               missing=NA, # todos los otros datos quedan como missing
               #notInLegis=0, # vector de ausentes en que seccion
               legis.names=nombreperso,
               legis.data=NULL,
               desc="UN 31 to 33")

#polarity pone al paíd en una de las dos dimensiones en un extremo
result <- wnominate(rc, dims=2, polarity=c(3,3))
summary(result) # el objeto results contiene la estimacion
# APRE: Aggregated Proportional reduction of error.
# GMP: Geometric mean probability
# miden la mejora en la estimacion al pasar de una a dos dimensiones

plot(result)

names(result) # el objeto result contiene 7 objetos
head(result$legislators)

result$legislators

tabla <- result$legislators[7:8]
tabla


summary(vector)
vector <- result$legislators[7:8]
write.csv(vector, file = "vector.csv")




head(result$rollcalls)
result$dimensions
head(result$eigenvalues)
result$beta
result$weights
result$fits



