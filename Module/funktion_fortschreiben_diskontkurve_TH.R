


diskont_fortschreibung_TH<-function(diskontkurve_input,liste_stichtage,laenge_zinsen){
  
  # Inputs ####
  
  #' liste_stichtage: Liste mit Stichtagen auf die Zinskurve fortgeschrieben werden soll
  #' diskontkurve_input: erste Spalte Nummerierung der diskontfaktoren, zweite Spalte Stichtag, dritte Spalte diskontfaktoren (diskontiert auf stichtag)
  #' laenge_zinsen: länge der Zinskurve: Annahme, alle Zinskurven haben die gleiche Länge
  
  # diskontkurve_input<-diskontierung_aufbereitet
  # liste_stichtage<-.liste_stichtage
  # laenge_zinsen<-.laenge_zinsen
  
  diskontkurve_input %>% 
    setNames(c("Monat","anfall_dat_von","diskontfaktor")) ->
    diskontkurve_input
  
  
  #' .min_Pdat<-min(liste_stichtage)
  #' 
  #' 
  #' diskontkurve_input %>% 
  #'   filter(anfall_dat_von<=.min_Pdat) %>% # Waehle Zinskurve, die am naehesten am kleinsten Pdat liegt
  #'   filter(anfall_dat_von==max(anfall_dat_von, na.rm = TRUE)) -> 
  #'   #filter((Monat %% 12)==0)-> #' Wähle nur diskontfaktoren aus, die im Dezember liegen--> Zinskurve von Carsten ist auf Jahresbasis, deshalb muss hier nicht auf die Dezember-Monate gefiltert werden
  #'   diskontkurve_input
  #' 
  #' min(diskontkurve_input$anfall_dat_von) %>%
  #'   matrix(ncol=1,nrow=1) %>%
  #'   data.frame %>%
  #'   setNames("min_aj") %>% 
  #'   mutate(min_aj = as.Date(min_aj, origin="1970-01-01")) %>% 
  #'   cbind("jahr" = 0) ->
  #'   .min_aj
  #' 
  #' #' Ausfawhl der stichtage
  #' 
  #' 0:(interval(.min_Pdat,max(liste_stichtage))%/%years(1)+0) %>% 
  #'   matrix(nrow = length(.), ncol = 1) %>% 
  #'   tibble %>% 
  #'   setNames("jahr") %>% 
  #'   mutate(min_aj = rollbackward(.min_Pdat, roll_to_first = TRUE)+years(jahr-1),
  #'          min_aj = rollforward(min_aj),
  #'          jahr =  jahr+1) %>% 
  #'   rbind(.min_aj, .) %>%   
  #'   tibble %>% 
  #'   mutate(anfall_dat_von = case_when(min_aj<=.min_aj[1,1] ~ .min_aj[1,1],
  #'                                     TRUE ~ min_aj)) %>% 
  #'   select(min_aj,anfall_dat_von) %>% 
  #'   mutate(anz_zinsen = laenge_zinsen) %>% 
  #'   uncount(anz_zinsen, .id = "Monat") %>% 
  #'   left_join(diskontkurve_input) %>% # Zinsen hinzufügen
  #'   select(min_aj,Monat,diskontfaktor) %>% 
  #'   rename(anfall_dat_von = min_aj) %>% 
  #'   arrange(Monat,anfall_dat_von) %>% 
  #'   fill(diskontfaktor) %>%
  #'   arrange(anfall_dat_von,Monat) %>% 
  #'   mutate(anzahl_stichtag = 1) %>% 
  #'   uncount(anzahl_stichtag) %>% 
  #'   cbind("stichtag" = rep(min(liste_stichtage), times = nrow(.)/1)) %>% 
  #'   mutate(lag_monate = (12-month(anfall_dat_von)+(year(stichtag)-year(anfall_dat_von)-1)*12+month(stichtag)),
  #'          lag_monate_2 = interval(anfall_dat_von,stichtag)%/%years(1)) %>%  # erste Fortschreibung einführen
  #'   mutate(lag_monate_2 = as.numeric(lag_monate_2),
  #'          Monat_2 = Monat-lag_monate_2) %>% 
  #'   filter(Monat_2>=0) -> #%>% filter(anfall_dat_von==ymd("2019-01-31")) ->
  #'   diskont_lag
  
  diskontkurve_input %>% 
    select(anfall_dat_von) %>% 
    distinct %>% 
    unlist %>% 
    unname ->
    temp_vektor
  
c(temp_vektor,liste_stichtage) %>% unique  %>% as.Date(origin="1970-01-01") -> temp_vektor # sicherstellen dass alle stichtage vorhanden sind
  
  diskontkurve_input %>% 
    mutate(stichtag = length(temp_vektor)) %>% 
    uncount(stichtag, .id = "nr") %>% 
    cbind("stichtag" = rep(temp_vektor, times = nrow(.)/length(temp_vektor))) %>% 
    mutate(lag_monate_2 = interval(anfall_dat_von,stichtag)%/%months(1),
           Monat_2 = Monat - lag_monate_2) -> diskont_lag
    
  
  #' Nenner ermitteln fuer erste Fortschreibung
  
  diskont_lag %>%
    filter(Monat_2==0) %>%
    select(anfall_dat_von,diskontfaktor,stichtag) %>%
    distinct %>%
    rename(nenner = diskontfaktor) %>%
    left_join(diskont_lag, .) %>% 
    filter(Monat_2>0) %>% 
    mutate(diskontfaktor_lock = diskontfaktor/nenner) %>% 
    mutate(diskontfaktor_lock = coalesce(diskontfaktor_lock,diskontfaktor),
           Monat = Monat-lag_monate_2) -> 
    diskont_lag
  
  
  #' Fortschreibung weitere Stichtage
  
  # diskont_lag %>%
  #   mutate(anz_stichtage = (length(liste_stichtage))) %>%
  #   uncount(anz_stichtage, .id = "nr_stich") %>%
  #   mutate(stichtag_fortschreibung = rollbackward(stichtag, roll_to_first = TRUE)+years(nr_stich-1),
  #          stichtag_fortschreibung = rollforward(stichtag_fortschreibung)) %>%
  #   select(-c(lag_monate,lag_monate_2,Monat_2,nenner,nr_stich,diskontfaktor)) %>%
  #   rename(diskontfaktor = diskontfaktor_lock) %>%
  #   mutate(lag_monate_2 = interval(stichtag,stichtag_fortschreibung)%/%years(1)) %>%
  #   mutate(lag_monate_2 = as.numeric(lag_monate_2),
  #          Monat_2 = Monat-lag_monate_2)  %>%
  #   filter(Monat_2>=0) ->
  #   diskont_lag_fortschreibung
  # 
  # 
  # diskont_lag_fortschreibung %>%
  #   filter(Monat_2==0) %>%
  #   select(anfall_dat_von,diskontfaktor,stichtag, stichtag_fortschreibung) %>%
  #   distinct %>%
  #   rename(nenner = diskontfaktor) %>%
  #   left_join(diskont_lag_fortschreibung, .) %>%
  #   filter(Monat_2>0) %>%
  #   mutate(diskontfaktor_lock = diskontfaktor/nenner) %>%
  #   mutate(diskontfaktor_lock = coalesce(diskontfaktor_lock,0),
  #          Monat = Monat-lag_monate_2) ->
  #   diskont_lag_fortschreibung
  # 
  # 
  # diskont_lag_fortschreibung %>% 
  #   # filter(anfall_dat_von==ymd("2020-12-31")) %>% 
  #   select(Monat,stichtag_fortschreibung,anfall_dat_von,diskontfaktor_lock) %>% 
  #   rename(diskontfaktor = diskontfaktor_lock) %>% 
  #   distinct ->
  #   diskontkurve
  # 
  # diskontkurve_input %>% 
  #   mutate(stichtag_fortschreibung = anfall_dat_von) %>% 
  #   select(names(diskontkurve)) %>% 
  #   rbind(diskontkurve) -> 
  #   diskontkurve
  
  
  diskont_lag %>% 
    select(Monat,anfall_dat_von,stichtag,diskontfaktor_lock) %>% 
    rename(stichtag = anfall_dat_von,
           stichtag_fortschreibung = stichtag,
           diskontfaktor = diskontfaktor_lock) ->
    diskontkurve
  
  
  return(diskontkurve)
  
}




















