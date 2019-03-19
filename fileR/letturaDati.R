library("dplyr")
# Lettura dati del pm10 ---------------------------------------------------

#nomeFileInput: file con i dati di pm10 e i predittori
#num_to_fact: variabili da trasformare in fattoriali (altrimenti NULL)
#minimo e massimo: filtrare i valori di pm10 in base a "minimo" e "massimo". Se NULL nessun filtro viene applicato
#addXcYc: se TRUE aggiunge le variabili "xc.s" e "yc.s" (coordinate delle stazioni standardizzate)

leggiPM10<-function(nomeFileInput,
                    num_to_fact=c("dust","climate_zone","season","cod_reg","yymmdd","zonal"),
                    minimo=NULL,
                    massimo=NULL,
                    addXcYc=FALSE,
                    rm_zonal_na=TRUE,
		                addWday=FALSE){

  if(missing(nomeFileInput) || !is.character(nomeFileInput)) stop("nomeFileInput non valido")
  
  tryCatch({
    readr::read_delim(sprintf("%s",nomeFileInput),delim=";",col_names=TRUE)
  },error=function(e){
    stop(sprintf("File %s non trovato!",nomeFileInput))
  })->dati
  
  if(rm_zonal_na) {dati %>% filter(!is.na(zonal))->dati}
  
  #aggiugno le coordinate standardizzate
  if(addXcYc){
    message("## Aggiungo coordinate standardizzate xc e yc")
    unique(dati$id_centralina)->centraline
    
    dati %>%
      dplyr::select(id_centralina,x,y)->tmp
    #prendiamo un solo dato (coordinata) per centralina
    tmp[!duplicated(tmp$id_centralina),] %>%
      dplyr::mutate(xc.s=scale(x),yc.s=scale(y)) %>%
      dplyr::select(-x,-y)->tmp
    
    left_join(dati,tmp,by=c("id_centralina"="id_centralina"))->dati
    rm(tmp)
  }#fine addXcYc
  
  #aggiungo il mese
  dati$mese<-as.integer(lubridate::month(dati$yymmdd))
  
  if(!is.null(minimo)){
    if(is.numeric(minimo)){
      message(sprintf("## Elimino dati pm10 minori di:",minimo))
      dati %>%
        filter(pm10>=minimo)->dati
    }else{
      message(sprintf("Valore minimo %s non valido e verrà ignorato"))
    }
  }
  
  if(!is.null(massimo)){
    if(is.numeric(massimo)){
      message(sprintf("## Elimino dati pm10 maggiori di:",massimo))
      dati %>%
        filter(pm10<=massimo)->dati
    }else{
      message(sprintf("Valore massimo %s non valido e verrà ignorato"))
    }
  }

  #aggiunge la variabile wday, il giorno della settimana da 1 a 7
  if(addWday){
	if(!any(grepl("^yymmdd$",names(dati)))) stop("var yymmdd non trovata")
	lubridate::wday(dati$yymmdd)->dati$wday
  }#fine su addWday
  

  if(!is.null(num_to_fact)){

	  #Trasformo in factor variabili in pred3  
	  message("## Converto le variabili in fattori")
	    
	  purrr::map_at(dati,.at=num_to_fact,.f=as.factor) %>% 
	    data.frame() %>% as_tibble()->tmp

	  tmp->dati
          rm(tmp)	

  }#fine if su num_to_fact	    
  
  dati

  
}#fine leggiPM10



# Leggi modello -----------------------------------------------------------

leggiModello<-function(nomeFileRDS){
  
  if(missing(nomeFileRDS) || !is.character(nomeFileRDS)) stop("nomeFileRDS non valido")
  tryCatch({
    readRDS(nomeFileRDS)
  },error=function(e){
    stop(sprintf("File modello %s non trovato!",nomeFileRDS))
  })->myMod
  
  myMod
  
}#leggiModello









