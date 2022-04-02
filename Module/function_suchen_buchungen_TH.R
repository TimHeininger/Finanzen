label_suche_TH<-function(label,exakt_enthaelt = TRUE){
  
  if(exakt_enthaelt==TRUE){
    buchungen %>% 
      filter(str_detect(Was,label)) ->
      ergebnis
  }else{
    buchungen %>% 
      filter(Was==label) ->
      ergebnis
  }
  
  ergebnis %>% 
    arrange(desc(Datum))  ->
    ergebnis
  
  #ergebnis<-as.data.frame(ergebnis)
  return(ergebnis)
}














