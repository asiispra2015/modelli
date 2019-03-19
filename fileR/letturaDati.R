library("dplyr")
# Lettura dati del pm10 ---------------------------------------------------

#nomeFileInput: file con i dati di pm10 e i predittori
#num_to_fact: variabili da trasformare in fattoriali (altrimenti NULL)
#minimo e massimo: filtrare i valori di pm10 in base a "minimo" e "massimo". Se NULL nessun filtro viene applicato
#addXcYc: se TRUE aggiunge le variabili "xc.s" e "yc.s" (coordinate delle stazioni standardizzate)

leggiPM10<-function(nomeFileInput,
                    startDate="2015-01-01",
                    endDate="2015-12-31",
                    minimo=NULL,
                    massimo=NULL,
                    addXcYc=FALSE,
                    addWday=FALSE,
                    addPTP=FALSE,
		                addPPM10=FALSE,
		                num_to_fact=c("dust","climate_zone","season","cod_reg","yymmdd","zonal"),
		                rm_zonal_na=TRUE){

  print(sprintf("Lettura dati periodo %s - %s",startDate,endDate))
  
  #creazione calendario
  seq.Date(from=as.Date(startDate),to=as.Date(endDate),by="day",format="%Y-%m-%d")->calendario
  length(calendario)->numeroGiorni
  
  data.frame(yymmdd=as.character(calendario),stringsAsFactors = FALSE)->dfCal
  rm(calendario)
  
  if(missing(nomeFileInput) || !is.character(nomeFileInput)) stop("nomeFileInput non valido")
  
  tryCatch({
    readr::read_delim(sprintf("%s",nomeFileInput),delim=";",col_names=TRUE)
  },error=function(e){
    stop(sprintf("File %s non trovato!",nomeFileInput))
  })->dati
  
  dati %>%
    filter(yymmdd>=startDate & yymmdd<=endDate)->dati
  
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
  
  #aggiunge la variabile wday, il giorno della settimana da 1 a 7
  if(addWday){
	if(!any(grepl("^yymmdd$",names(dati)))) stop("var yymmdd non trovata")
	lubridate::wday(dati$yymmdd)->dati$wday
  }#fine su addWday
  

  #add previous PM10
  if(addPPM10 || addPTP){
    
    unique(dati$id_centralina)->centraline
    stopifnot(length(centraline)>0)
    
    
    furrr::future_map(centraline,.f=function(cc){
      
      dati %>%
        filter(id_centralina==cc) %>%
        mutate(yymmdd=as.character(yymmdd))->subDati
      
      left_join(dfCal,subDati,by=c("yymmdd"="yymmdd"))->jsubDati
      rm(subDati)
     
      if((nrow(jsubDati)!=numeroGiorni)) stop("addPPM10 funziona con serie giornaliere complete")
      
      #add previous pm10
      if(addPPM10) {jsubDati$ppm10<-c(NA,jsubDati$pm10[1: (numeroGiorni-1) ])}
      
      #add previous total precipitation
      if(addPTP) {
        jsubDati$ptp.s<-c(NA,jsubDati$tp.s[1:(numeroGiorni-1) ])
        jsubDati$ptp<-c(NA,jsubDati$tp[1:(numeroGiorni-1)])
      }      
      
      jsubDati %>%
        filter(!is.na(pm10))
      
    }) %>% reduce(bind_rows) %>% as.data.frame(.)->dati
    
    
  }#fine addPM10/addPTP
  
  
  if(!is.null(num_to_fact)){
    
    #Trasformo in factor variabili in pred3  
    message("## Converto le variabili in fattori")
    
    purrr::map_at(dati,.at=num_to_fact,.f=as.factor) %>% 
      data.frame() %>% as_tibble()->tmp
    
    tmp->dati
    rm(tmp)	
    
  }#fine if su num_to_fact	
  
  
  #il filtro sul minimo e massimo deve essere fatto alla fine, dopo aver calcolato le variabili "previous"
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









