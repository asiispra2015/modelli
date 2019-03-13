#Restituisce la formula del modello:
# - logPm10: usa il log del pm10 come variabile risposta
# - variabili centrate: restituisce la formual con le variabili centrate
# - uv10: se FALSE, restituisco wdir e wspeed, altrimenti restituisco u10 e v10
# - xcyc: se TRUE restituisco xc.s e yc.s (coordinate centrate e standardizzate)
# - logpbl: se TRUE restituisco il logaritmo del pbl

formulaModello<-function(logPM10=TRUE,logAOD=FALSE,variabiliCentrate=TRUE,uv10=FALSE,xcyc=FALSE,logpbl=FALSE){
  
  if(!variabiliCentrate){
    
    print("Modello con variabili -->NON<-- centrate")
    
    #formula fixed senza climate_zone e cod_reg, variabili non centrate
    pm10.formula.fixed <- formula(pm10 ~ aod550+season
                                    # meteo
                                  +t2m+sp+tp+u10+v10
                                  # other spatio-temporal
                                  +pbl00+pbl12+ndvi+dust
                                  # spatial 1: population, elevation, isa
                                  +p_istat+q_dem+i_surface
                                  # spatial 2: point emissions and areal emissio
                                  +d_impianti+co_punt+nh3_diff+pm10_diff
                                  # spatial 4: land coverage vars.
                                  +cl_hidv+cl_lwdv+cl_arbl+cl_crop+cl_pstr+cl_agri+cl_dcds+cl_evgr+cl_shrb
                                  # spatial 5: streets vars.
                                  +d_a1+d_a2+av_buf_a1+av_buf_a23+av_buf_oth
                                  # spatial 5: other proximity variables
                                  +d_costa+d_aero)
    
  }else{
    
    print("Modello con variabili centrate")
    
    #formula fixed senza climate_zone e cod_reg, variabili centrate
    pm10.formula.fixed <- formula(pm10 ~ aod550+season
                                  +t2m.s+sp.s+tp.s+u10.s+v10.s
                                  # other spatio-temporal
                                  +pbl00.inv.s+pbl12.inv.s+ndvi.s+dust
                                  # spatial 1: population, elevation, isa
                                  +p_istat.s+q_dem.s+i_surface.s
                                  # spatial 2: point emissions and areal emissions
                                  +d_impianti.inv.s+co_punt.s+nh3_diff.s+pm10_diff.s
                                  # spatial 4: land coverage vars.
                                  +cl_hidv.s+cl_lwdv.s+cl_arbl.s+cl_crop.s+cl_pstr.s+cl_agri.s+cl_dcds.s+cl_evgr.s+cl_shrb.s
                                  # spatial 5: streets vars.
                                  +d_a1.inv.s+d_a2.inv.s+av_buf_a1.s+av_buf_a23.s+av_buf_oth.s
                                  # spatial 5: other proximity variables
                                  +d_costa.inv.s+d_aero.inv.s)
    
  }
  
  #Elimino p_istat.s 
  update(pm10.formula.fixed,.~.-p_istat.s)->pm10.formula.fixed
  
  #stessa formula con il log del pm10
  if(logPM10){update(pm10.formula.fixed,log(pm10)~.)->pm10.formula.fixed}
  
  #log aod o aod?
  if(logAOD){update(pm10.formula.fixed,.~.-aod550+log.aod550)->pm10.formula.fixed}
  
  #stessa formula con il log del pm10 e wdir e wspeed invece di u10 e v10
  if(!uv10) {update(pm10.formula.fixed,.~.-u10.s-v10.s+wdir.s+wspeed.s)->pm10.formula.fixed}
  
  #il log del pbl00 e del pbl12
  if(logpbl) {update(pm10.formula.fixed,.~.-pbl12.inv.s+log.pbl12.inv.s-pbl00.inv.s+log.pbl00.inv.s)->pm10.formula.fixed}
  
  #le coordinate standardizzate
  if(xcyc) {update(pm10.formula.fixed,.~.+xc.s+yc.s)->pm10.formula.fixed}
  
  pm10.formula.fixed

}#fine funzione formulaModello
