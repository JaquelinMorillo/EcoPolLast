rm(list=ls())
require(here)
aqui <- here()

library(foreign) 
library(gdata)

# librerias parametricas y no parametricas para trabajar con datos en formatos rollcall

library(wnominate) # Poole et al 2011
library(MCMCpack) # Quinn et al 2011
library(pscl)  # Jackman 2012

#Cargo base reco
BASE <- read_xlsx(paste0(aqui,"/BASE_FINAL_RECO.xlsx"))
