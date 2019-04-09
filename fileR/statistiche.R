#Statistiche per la validazione del modello
#Include il programma per effettuare la leave one out cross validation (ovvero per creare i k-fold)

creaFolds<-function(k,n,ksize){
  
  if((k*ksize)>n) stop("ksize troppo grande")
  
  righe<-1:n
  #righeOrig non va modificato, mentre righe lo aggiorniamo nei vari cicli
  righeOrig<-righe
  
  listaOut<-vector(mode="list",length=2)
  names(listaOut)<-c("index","indexOut")
  
  purrr::walk(1:k,.f=function(ii){
    
    sample(x=righe,size = ksize,replace = FALSE)->indexOut
    base::setdiff(righe,indexOut)->index
    
    listaOut$indexOut[[ii]]<<-indexOut
    listaOut$index[[ii]]<<-base::setdiff(righeOrig,indexOut)    
    
    
    index->>righe
    
  })
  
  return(listaOut)
  
}#fine creaFolds  

#verifica dei vettori .x e .y passati alle funzioni
verifica<-function(x,y){

  stopifnot((is.numeric(x) && is.numeric(y)))
  stopifnot(length(x)==length(y))

  #allineiamo x e y, in modo di avere gli stessi dati NON NA nei due vettori
  which(is.na(x))->nax
  which(is.na(y))->nay
    
  base::union(nax,nay)->righeNA
  if(length(righeNA)){
    return(list(x=x[-righeNA],y=y[-righeNA]))
  }else{
    return(list(x=x,y=y))    
  }
    

  
}#fine verifica

#correlazione tra .x e .y
#I parametri boolean servono per applicare il logaritmo (ad esempio nel caso del modello di Massimo)
#se le predizioni sono state trasformate in maniera esponenziale
correlazione<-function(.x,.y,.logx=FALSE,.logy=FALSE,...){
  
  verifica(.x,.y)->out
  
  out$x->.x
  out$y->.y  
  rm(out)
  
  stopifnot((is.logical(.logx) && is.logical(.logy)))  
  
  
  if(.logx){.x<-log(.x)}
  if(.logy){.y<-log(.y)}
  
  cor(round(.x,2),round(.y,2),...)->ris

  round(ris,2)
    
}#correlazione



# RMSE --------------------------------------------------------------------

rmse<-function(.x,.y,.logx=FALSE,.logy=FALSE,...){
  
  verifica(.x,.y)->out
  
  out$x->.x
  out$y->.y  
  rm(out)
  
  stopifnot((is.logical(.logx) && is.logical(.logy)))  
  
  if(.logx){.x<-log(.x)}
  if(.logy){.y<-log(.y)}
  
  round(.x,2)->.x
  round(.y,2)->.y
  
  sqrt(mean((.x-.y)^2,na.rm=TRUE))->ris
  
  round(ris,2)
  
}#fine rmse



# Bias --------------------------------------------------------------------

bias<-function(.x,.y,.logx=FALSE,.logy=FALSE,...){
  
  verifica(.x,.y)->out
  
  out$x->.x
  out$y->.y  
  rm(out)
  
  stopifnot((is.logical(.logx) && is.logical(.logy)))  
  
  if(.logx){.x<-log(.x)}
  if(.logy){.y<-log(.y)}
  
  round(.x,2)->.x
  round(.y,2)->.y
  
  mean(.x,na.rm=TRUE)-mean(.y,na.rm=TRUE)->ris
  
  round(ris,2)
  
}#fine bias


# R2 --------------------------------------------------------------------

#.pred: pm10 predetto
#.obs: pm10 osservato

r2<-function(.pred,.obs,.logpred=FALSE,.logobs=FALSE,...){
  
  verifica(x=.pred,y=.obs)->out
  
  out$x->.pred
  out$y->.obs  
  rm(out)
  
  stopifnot((is.logical(.logpred) && is.logical(.logobs)))  
  
  if(.logpred){.pred<-log(.pred)}
  if(.logobs){.obs<-log(.obs)}
  
  round(.pred,2)->.pred
  round(.obs,2)->.obs
  
  sse<-sum((.pred-.obs)^2,na.rm=TRUE)
  sst<-sum((.obs-mean(.obs,na.rm=TRUE))^2,na.rm=TRUE)  
  
  (1-(sse/sst))*100->ris
  
  round(ris,2)
  
}#fine bias
