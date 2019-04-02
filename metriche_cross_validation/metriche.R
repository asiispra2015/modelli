rm(list=objects())
library("CAST")
library("tidyverse")
library("lme4")
library("lattice")
library("latticeExtra")
library("furrr") #purrrin parallelo
source("./fileR/statistiche.R")

readRDS("risultati_cross_calidation.RDS")->risCros
#numero k-fold
length(risCros)->numK

purrr::map(risCros,.f=function(x){

  #dobbiamo usare il log nel caso del modello di Massimo
  correlazione(x$pred.mod,x$pm10,.logx=TRUE,.logy=FALSE,use="everything")->outCor
  rmse(x$pred.mod,x$pm10,.logx=TRUE,.logy=FALSE)->outRmse
  bias(x$pred.mod,x$pm10,.logx=TRUE,.logy=FALSE)->outBias
  r2(.pred=x$pred.mod,.obs=x$pm10,.logpred=TRUE,.logobs=FALSE)->outR2
  
  
  data.frame(corr=outCor,rmse=outRmse,bias=outBias,r2=outR2)
  
  
}) %>% reduce(rbind) %>% as.data.frame->dfRsquared


purrr::imap(risCros,.f=function(.x,.y){
  
  #dobbiamo usare il log nel caso del modello di Massimo
  xyplot(log(pred.mod)~pm10|season,data=.x,panel=function(x,y,...){
    panel.xyplot(x,y,...)
    panel.loess(x,y,col="red")
    panel.abline(a=0,b=1)
  },main=paste0("k-",.y),
  strip = strip.custom(factor.levels=c("Win","Spr","Sum","Aut")),
  aspect = "iso",
  groups = season,
  ylab = "pred.mod",
  xlab="pm10")->grafico
  
  grafico
  
})->listaGrafici


gridExtra::grid.arrange(grobs=listaGrafici[1:4],nrow=2)
gridExtra::grid.arrange(grobs=listaGrafici[5:8],nrow=2)
gridExtra::grid.arrange(grobs=listaGrafici[9:10],nrow=1)



purrr::imap(risCros,.f=function(.x,.y){

  #dobbiamo usare il log nel caso del modello di Massimo
  densityplot(~log(pred.mod)+pm10|season,data=.x,main=paste0("k-",.y),
  strip = strip.custom(factor.levels=c("Win","Spr","Sum","Aut")),
  ref=TRUE,
  aspect="fill",
  ylab = "density pm10",
  xlab="")->grafico
  
  grafico
  
})->listaGrafici

gridExtra::grid.arrange(grobs=listaGrafici[1:4],nrow=2)
gridExtra::grid.arrange(grobs=listaGrafici[5:8],nrow=2)
gridExtra::grid.arrange(grobs=listaGrafici[9:10],nrow=1)
