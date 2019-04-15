#utility varie

# Creazione del calendario giornaliero ------------------------------------

#Input: 
# - annoI e annoF (inizio e fine del calendario in formato yyyy-mm-dd)
# - by: passo temporale del calendario (di default è il giorno)
# - returnDF: se TRUE restituisce un data.frame,altrimenti un vettore di date

#Output:
# -vettore o data.frame (se returnDF=TRUE) con il calendario in base al passo "step"

creaCalendario<-function(annoI,annoF,step="day",returnDF=TRUE,splitDate=FALSE){
  if(missing(annoI) || missing(annoF)) stop("Specificare giorno inizio e fine del calendario: yyyy-mm-dd")

  if(!(is.character(annoI)) || !(is.character(annoF))) stop("annoI o annoF non validi")	

  seq.Date(from=as.Date(annoI),to=as.Date(annoF),by=step)->calendario
  
  if(returnDF){
    if(splitDate){
      return(data.frame(yymmdd=calendario,stringsAsFactors=FALSE) %>% 
               tidyr::separate(col=yymmdd,into=c("yy","mm","dd"),sep="-") %>%
               mutate(mm=stringr::str_pad(mm,pad="0",side="left",width=2),dd=stringr::str_pad(dd,width=2,side="left",pad="0"))) 
    }else{
      return(data.frame(yymmdd=calendario,stringsAsFactors=FALSE))       
    }

  }else{
	return(calendario)	
  }		

}#fine creaCalendario
