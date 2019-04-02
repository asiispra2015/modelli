#1 Aprile 2019
#cross validation del modello
rm(list=objects())
library("CAST")
library("dplyr")
library("lme4")
library("furrr") #purrrin parallelo
source("./fileR/letturaDati.R")

#qui mettiamo tutto quello che serve alla cross validation per il modello specifico:
#ad esempio per lme4, metteremo lmeControl etc etc
source("crossValidation_support.R")

SEME<-1
set.seed(SEME)

#lettura dati osservati, filtriamo i valori di pm10, aggiungiamo coordinate standardizzate e rimoviamo zonal==NA
leggiPM10(nomeFileInput = "pm10_analisi.csv",minimo=10,massimo=100,rm_zonal_na = TRUE)->finale

#lettura del modello di cui vogliamo fare la cross-validation
leggiModello(nomeFileRDS="modello.RDS")->modello

#LOT-CV (leave one time out cross validation)
#indici contiene le righe che definiscono i fold
CreateSpacetimeFolds(x=finale,timevar = "yymmdd",k=10,seed=SEME)->indici



#cross validation: ciclo su "indici" che identificano i fold
purrr::map2(.x=indici$index,.y=indici$indexOut,.f=function(.x,.y){

  #stimo i parametri del modello per il sottoinsieme  identificato da "index"
  finale[.x,]->finaleIndex
  update(modello,data=finaleIndex)->newMod  
  
  #ora predico i valori identificati da indexOut
  finale[.y,]->finaleIndexOut
  predict(newMod,newdata=finaleIndexOut,re.form=NULL,allow.new.levels=TRUE)->predizioni

  finaleIndexOut$pred.mod<-exp(predizioni)
  
  finaleIndexOut
  
})->listaOUT

saveRDS(listaOUT,"risultati_cross_calidation.RDS")

