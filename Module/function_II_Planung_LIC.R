II_Planung_LIC<-function(filename,
                        stichtag_daten,
                        stichtag_planung,
                        schema,
                        name_cockpit_planung,
                        pfad_R_lib,
                        Pversion,
                        anz_planjahre,
                        pfad_R_code,
                        con,
                        con_TEST){
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Beschreibung ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  .libPaths(pfad_R_lib)
  
  #Benötigte Libraries laden
  # suppressPackageStartupMessages(library(ChainLadder)) # Behandlung von Dreiecken
  library(openxlsx) # Excel
  library(readxl) # Excel
  # library(MASS) #Verteilungen
  # library(dplyr) # pipe operator (vorsicht bei gleichzeitiger Verwendung von dplyr und data.table)
  library(stringr)
  # library(abind)
  # library(parallel) # Paralellisierung auf mehrere Kerne
  # library(foreach)  # Paralellisierung auf mehrere Kerne
  # library(doParallel)  # Paralellisierung auf mehrere Kerne
  # library(corrplot)   # Erstellen von Graphiken
  # library(graphics) # Erstellen von Graphiken
  # suppressPackageStartupMessages(library(ggplot2))  # Erstellen von Graphiken
  library(sqldf)   # SQL-Code auf Dataframes, dataframes müssen globale Variablen sein (also ohne vorrangestellen .)
  library(lubridate) # Behandlung von Datum, Uhrzeit etc. (funktionen ymd, dmy, ...)
  # library(data.table) # behandlung von Tabellen (vorsicht bei gleichzeitiger Verwendung von dplyr und data.table)
  # library(tidyr) # Manipulation von daten
  library(magrittr) # pipe operators
  # library(wrapr) # pipe operators
  # library(rqdatatable) # Code als String speichern und ausführen
  # library(testthat) # Testpackage
  library(tidyverse)
  
  library(DBI)
  library(dbplyr)
  library(rstudioapi)
  library(odbc)
  
  fwrite<-data.table::fwrite
  
  
  # filename <- dirname(current_path )

  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Manuelle Eingaben ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  .name_Validierung_Parameter<-"Validierung_Parameter.xlsx"
  .sheet_Validierung_CF<-"Validierung_CF"
  .sheet_Validierung_RA<-"Validierung_RA"
  .sheet_Validierung_RA_vorjahre<-"Validierung_RA_vorjahre"
  
  
  source(paste(pfad_R_code,"funktion_verbindung_DSS_TH.R", sep = "/"), encoding = 'UTF-8', echo=FALSE)
  source(paste(pfad_R_code,"funktion_fortschreiben_diskontkurve_TH.R", sep = "/"), encoding = 'UTF-8', echo=FALSE)
  
  .GJ<-year(ymd(stichtag_planung))
  
  stichtag_planung<-ymd(stichtag_planung)
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Parameter aus Exceldatei laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Parameter aus Exceldatei laden")
  
  .input_parameter <- read_excel(paste(filename,name_cockpit_planung, sep = "/"), sheet = "Parameter_R_Code")
  .input_parameter <- .input_parameter[which(.input_parameter$Berechnungsschritt == "LIC"),]
  
  .variables <- unique(.input_parameter$Variablen_name)
  
  for (.i in 1:length(.variables)) {
    
    assign(.variables[.i], as.vector(.input_parameter$Wert[.input_parameter$Variablen_name == .variables[.i]], mode = unique(.input_parameter$Type[.input_parameter$Variablen_name == .variables[.i]])))
    
  }
  
  
  rm(.input_parameter)
  
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Einlesen Inputdaten ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Einlesen Inputdaten")
  
  #' Einlesen der Validierten Daten
  
  read.xlsx(paste(filename,"Output",.name_Validierung_Parameter, sep = "/"), sheet = .sheet_Validierung_CF) ->
    Validierung
  
  read.xlsx(paste(filename,"Output",.name_Validierung_Parameter, sep = "/"), sheet = .sheet_Validierung_RA) %>%
    select(-c(faktor_ra,faktor_ra_standard)) %>%
    rename(faktor_ra = faktor_ra_verwendet) ->
    faktor_ra
  
  
  read.xlsx(paste(filename,"Output",.name_Validierung_Parameter, sep = "/"), sheet = .sheet_Validierung_RA_vorjahre) ->
    faktor_ra_vorjahre
  
  read.xlsx(paste(filename,"Inputs",.name_kennzeichen_datei, sep = "/"), sheet = .sheet_kennzeichen_datei) %>% 
    rename(basis_pf = rc_pf) ->
    input_C_Kennzeichen
  
  
  # read.xlsx(paste(filename,"Inputs",.name_zinsen, sep = "/"), sheet = .sheet_zinsen) %>% 
  #   mutate(Monat = row_number()) %>% 
  #   pivot_longer(cols = c(1:(ncol(.)-1)),names_to = "anfall_dat_von",values_to = "diskontfaktor") %>% 
  #   mutate(anfall_dat_von = as.numeric(anfall_dat_von),
  #          anfall_dat_von = as.Date(anfall_dat_von, origin="1899-12-30")) -> 
  #   diskontierung_aufbereitet
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # ALL: DSS Daten laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("ALL: DSS Daten laden")
  
  #' Mapping fuer interne Gesellschaften
  
  ("select *
  
  from daisy.r_rg_stada_firma_ges_int_ext 
  
  ") %>% 
    dbGetQuery(con, . ) %>% 
    group_by(across(names(.)[which(names(.)!="upd_dat")])) %>% 
    filter(upd_dat == max(upd_dat, na.rm = TRUE)) %>% 
    ungroup %>% 
    select(gesellschaft,ifrs17_int_ext) %>% 
    distinct %>%
    rename(gsch_par = gesellschaft,
           int_ext_korr = ifrs17_int_ext) ->
    mapping_int_ext
  
  #' Ziaportfoid
  
  ("select
  basis_pf,
  prop_nonprop,
  fak_obl,
  ziaportfoid
  
  from schema_abfrage.s_rg_seg_ifrs_mega_mapping
  
  where  stichtag = CAST('stichtag_abfrage' as date) and basis_pf <> 'NICHT_RELEVANT'
  
  group by 1,2,3,4") %>% 
    str_replace_all("stichtag_abfrage", as.character(stichtag_daten)) %>%
    str_replace_all("schema_abfrage",schema) %>% 
    dbGetQuery(con, . ) -> 
    ziaportfoid
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # ALL: Zinsen laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  ("SELECT *
    FROM     ft_ifrs_ood_ph.fh_dskt_kurve_his
    WHERE    guelt_stat = 'L'
    AND      whg_id = 'EUR'
    order by sti_dat
    ;") %>%
    dbGetQuery(con_TEST, . ) ->
    Zinsen
  
  
  
  Zinsen %>%
    filter(sti_dat==ymd(paste(.GJ-1,"12","31",sep = "-"))) %>% 
    select(sti_dat,starts_with("jhr")) %>%
    pivot_longer(names_to = "jhr", values_to = "zsk", cols = starts_with("jhr"), names_prefix = "jhr") ->
    temp_zinsen
  
  temp_zinsen %>%
    mutate(zsk = zsk/100,
           jhr = as.numeric(jhr),
           df_j = 1/(1+zsk)^jhr) %>%
    cbind("monat" = 12) %>%
    uncount(monat, .id = "monat") %>%
    mutate(tau = jhr-1+monat*30/360,
           mon_lauf = (jhr-1)*12+monat,
           zinsen1 = zsk) ->
    temp_zinsen
  
  temp_zinsen %>%
    select(sti_dat,jhr,monat,zinsen1) %>%
    rename(zinsen2 = zinsen1) %>%
    mutate(jhr = jhr -1) %>%
    left_join(temp_zinsen, . ) %>%
    mutate(df1 = df_j) ->
    temp_zinsen
  
  
  temp_zinsen %>%
    select(sti_dat,jhr,monat,df1) %>%
    rename(df2 = df1) %>%
    mutate(jhr = jhr +1) %>%
    left_join(temp_zinsen, . ) ->
    temp_zinsen
  
  temp_zinsen %>%
    mutate(zsk_m = (monat-1)/12*zinsen2+(12-monat+1)/12*zinsen1,
           df_m = case_when(jhr==1 ~ df1^tau,
                            TRUE ~ df2^(tau*(jhr-tau)/(jhr-1))*df1^(tau*(tau-jhr+1)/jhr))) %>% #-> temp_table
    select(mon_lauf,sti_dat,df_m) %>%
    distinct %>%
    setNames(c("Monat","anfall_dat_von","diskontfaktor")) %>%
    arrange(anfall_dat_von,Monat)->
    diskontierung_aufbereitet
  
  
  
  rm(Zinsen, temp_zinsen)
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Zins fortschreiben ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Zins fortschreiben")
  
  Validierung %>%
    select(plan_jahr) %>%
    distinct %>%
    mutate(plan_jahr = ymd(paste(plan_jahr,"-12-31", sep = ""))) %>%
    unlist %>%
    unname %>%
    as.Date( . , origin="1970-01-01") ->
    .liste_stichtage
  
  .laenge_zinsen<-nrow(diskontierung_aufbereitet)
  
  diskont_fortschreibung_TH(diskontierung_aufbereitet,.liste_stichtage,.laenge_zinsen) ->
    diskontkurve
  
  diskontkurve %>% select(-stichtag) -> diskontkurve
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Cashflows erzeugen ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Cashflows erzeugen")
  
  Validierung %>% 
    mutate(BE = Ultimate-wert,
           across(starts_with("CF_"), ~ .x*BE)) %>%
    select(-Ultimate) %>% 
    rename(anfall_jahr = plan_jahr,
           CF_0 = wert)-> # Damit wert mit gespiegelt wird
    plandaten_cf
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # LIC: Spiegelung & Risikoanpassung Schadenzahlung GJ ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("LIC: Spiegelung & Risikoanpassung Schadenzahlung GJ")
  
  #' Risikoanpassung hinzufuegen
  
  plandaten_cf %>%  # filter(basis_pf=="NatCat" & gsch_bil=="001" & gsch_par=="040") %>% 
    select(-BE) %>% 
    pivot_longer(cols = starts_with("CF_"), names_to = "CF_NR", values_to = "BE") %>% 
    # filter(BE!=0) %>% 
    left_join(faktor_ra) %>% 
    rename(RA = faktor_ra) %>% 
    mutate(RA = RA*BE) %>% 
    pivot_longer(cols = c((ncol(.)-1),ncol(.)), names_to = "BE_RA", values_to = "wrt") ->
    temp_cf
  
  
  temp_cf %>% # filteren auf Daten, die nicht gespiegelt werden muessen
    filter(!(gesch_art!=30 & int_ext_korr!="ext")) ->
    temp_cf_1
  
  
  #' Spiegelung
  
  temp_cf %>% 
    filter(gesch_art!=30 & int_ext_korr!="ext") %>% 
    mutate(gesch_art = 30,
           ag_ueg = "ueg",
           wrt = wrt) %>% 
    rename(gsch_par = gsch_bil, # Kennzeichen tauschen
           gsch_bil = gsch_par) %>%
    select(names(temp_cf_1)) %>% 
    group_by(across(!matches("wrt"))) %>% # zusammenfassen, was zusammenfaellt
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    select(names(temp_cf)) %>% 
    rbind(temp_cf) %>% 
    select(names(temp_cf_1)) %>% 
    rbind(temp_cf_1) ->
    temp_cf
  
  
  #' RA-Faktoren erzeugen
  
  temp_cf %>% 
    select(-CF_NR) %>% 
    group_by(gsch_bil,gsch_par,gesch_art,basis_pf,prop_nonprop, fak_obl,longtail_shorttail,anfall_jahr,BE_RA) %>% # Ebene der Lieferung
    summarise(across(matches("wrt"), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup %>% 
    pivot_wider(names_from = BE_RA,values_from = wrt) %>% 
    mutate(faktor_ra = RA/BE) %>% 
    select(c(names(faktor_ra),"longtail_shorttail","anfall_jahr")) ->
    faktor_ra
  
  
  faktor_ra %>% 
    select(names(faktor_ra_vorjahre)) %>% 
    rbind(faktor_ra_vorjahre) -> 
    faktor_ra
  
  
  temp_cf %>% 
    filter(BE_RA=="BE") %>% 
    group_by(across(!matches("wrt"))) %>% 
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    select(-BE_RA) ->
    temp_cf
  
  temp_cf %>% 
    select(-CF_NR) %>% 
    group_by(across(!matches("wrt"))) %>% 
    summarise(BE = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    left_join(temp_cf) %>% 
    mutate(CF_NR = str_remove(CF_NR,"CF_"),
           CF_NR = as.numeric(CF_NR)) %>% 
    pivot_wider(names_from = CF_NR, values_from = wrt, names_prefix = "CF_", names_sort = TRUE) -> 
    plandaten_cf
  
  # plandaten_cf%>% filter(gsch_bil=="001" & basis_pf=="NatCat" & gsch_par=="040")
  
  rm(temp_cf,temp_cf_1)
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # LIC: Upload Daten schadenzahlung GJ ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("LIC: Upload Daten schadenzahlung GJ")
  
  #' C_Kennzeichen aggregieren auf Output Niveau
  
  input_C_Kennzeichen %>% 
    filter(if_all(names(.)[.spalte_kennzeichen_FuV], ~ . != "FuV")) %>% # Schliesse fuer Schluesselung Forderungen und Verbindlichkeiten aus, da sie in den HGB Planzahlen nicht vorhanden sind
    filter(if_all(names(.)[.spalte_kennzeichen_kost_erloesart], ~ . != "RfB")) %>% # schliesse RfB aus, behalte Renten und Schadenzahlungen
    left_join(mapping_int_ext) %>% 
    mutate(int_ext_korr = coalesce(int_ext_korr,"ext")) %>% 
    #' Korrektur der Kennzeichen auf Planungskennzeichen
    mutate(ag_ueg = case_when(gesch_art == 30 ~ "ueg",
                              TRUE ~ "ag"),
           gsch_par= case_when(int_ext_korr=="ext" ~ "ext",
                               is.na(int_ext_korr) ~ "ext", # externes Geschaeft wird nicht zugeordnet
                               TRUE ~ gsch_par),
           gesch_art = as.numeric(gesch_art),
           across(where(is.numeric) & starts_with(c("BE","CF_")), ~ case_when(gesch_art == 30 ~ -.x, TRUE ~ .x))) %>% # Uebernahme wieder Vorzeichen drehen, da es weiter unten erneut gedreht wird
    group_by(across(names(.)[c(.spalte_kennzeichen_gesch_art,
                               .spalte_kennzeichen_prop_nonprop,
                               .spalte_kennzeichen_fak_obl,
                               .spalte_kennzeichen_gsch_bil,
                               .spalte_kennzeichen_basis_pf,
                               .spalte_kennzeichen_anfall_jahr,
                               .spalte_kennzeichen_gsch_par,
                               which(names(.) %in% c("int_ext_korr","ag_ueg","longtail_shorttail","kges")))])) %>% 
    summarise(across(where(is.numeric) & starts_with(c("BE","CF_")), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup %>% 
    filter(anfall_jahr<.GJ) %>% 
    filter(BE!=0) %>% 
    filter(if_any(where(is.numeric) & starts_with(c("BE","CF_")), ~ .x!=0)) ->
    plandaten_cf_vorjahre
  
  #' Spaltennamen angleichen
  .spalten_namen<-names(plandaten_cf_vorjahre)[which(startsWith(names(plandaten_cf_vorjahre),"CF_"))]
  .spalten_namen<-str_remove(.spalten_namen,"CF_")  
  .spalten_namen<-as.numeric(.spalten_namen)-1
  .spalten_namen<-c(.spalten_namen,max(.spalten_namen)+1)  
  .spalten_namen<-paste("CF_",.spalten_namen,sep = "")
  .spalten_namen<-c(names(plandaten_cf_vorjahre)[which(!startsWith(names(plandaten_cf_vorjahre),"CF_"))],.spalten_namen)
  
  
  plandaten_cf_vorjahre %>% 
    cbind("CF_neu"=0) %>% 
    set_names(.spalten_namen) ->
    plandaten_cf_vorjahre
  
  rm(.spalten_namen)
  
  plandaten_cf %>% 
    group_by(across(names(plandaten_cf)[!(startsWith(names(plandaten_cf),"CF") | startsWith(names(plandaten_cf),"BE"))])) %>% 
    summarise(across(where(is.numeric) & starts_with(c("BE","CF_")), ~ sum(.x, na.rm = TRUE))) %>%
    ungroup %>% 
    rename(wert = CF_0) ->
    plandaten_cf
  
  
  
  plandaten_cf_vorjahre %>% 
    rename(wert = CF_0) %>% 
    cbind("datei_name"= "C_Kennzeichen","sheet_name" = "Sheet 1") %>% 
    rbind(plandaten_cf) ->
    plandaten_cf_schadenzahlungen_gj
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # LIC: Datenbankformat erzeugen ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("LIC: Datenbankformat erzeugen")
  
  min_aj<-min(plandaten_cf_schadenzahlungen_gj$anfall_jahr)
  max_aj<-max(plandaten_cf_schadenzahlungen_gj$anfall_jahr)
  
  #' fae_dats erzeugen
  
  c(min_aj:max_aj) %>% 
    matrix(nrow = length(.), ncol = 1) %>% 
    tibble %>% 
    set_names("anfall_jahr") %>% 
    cbind("anz_cf_jahr" = .anz_cf_jahre*12) %>% 
    mutate(start_fae_dat = case_when(anfall_jahr<.GJ ~ as.numeric(.GJ), # Damit fae_dat im GJ anfangen, diese Daten enthalten bereits Informationen ueber GJ
                                     TRUE ~ as.numeric(anfall_jahr)+1)) %>% # Sicherstellen, dass fae_dat in der Zukunft liegen
    uncount(anz_cf_jahr, .id = "monat") %>% 
    mutate(monat = as.numeric(monat),
           fae_dat = ymd(paste(start_fae_dat,"01-01", sep = "-"))+months(monat-1),
           fae_dat = ymd(paste(year(fae_dat),"07-01", sep = "-")),
           fae_dat = rollforward(fae_dat),
           monat = case_when(anfall_jahr<.GJ ~ (monat -1), # VJ haben alles an Reserve entahlten, da Stand 31.12.letztes_jahr
                             TRUE~ monat), # plandaten
           monat = paste("CF_",monat, sep = "")) %>% 
    select(-start_fae_dat)->
    mapping_fae_dat
  
  
  
  #' fae_dat fuer zukuenftige Planjahre und GJ hinzufuegen
  mapping_fae_dat %>% 
    filter(anfall_jahr>=.GJ) %>% 
    select(anfall_jahr) %>% 
    distinct %>% 
    mutate(monat = "CF_0",
           fae_dat = ymd(paste(anfall_jahr,"-07-31"))) %>% 
    rbind(mapping_fae_dat) %>% 
    arrange_all -> 
    mapping_fae_dat
  
  
  #' Schadenzahlungen GJ
  
  plandaten_cf_schadenzahlungen_gj %>% # filter(gsch_bil=="040" & basis_pf=="NatCat" & gsch_par =="ext" & anfall_jahr>=.GJ) %>% 
    select(-BE) %>% 
    pivot_longer(cols = starts_with("CF_"), names_to = "monat", values_to = "betrag") %>% 
    filter(betrag != 0) %>% 
    left_join(mapping_fae_dat) %>% 
    select(-monat) %>%
    group_by(across(!matches("betrag"))) %>% 
    summarise(across(matches("betrag"), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup %>%  #-> temp_table
    #' Shift in Plandaten durchfruehren --> Prognose der Abbrechnung ODER der Bilanzhalen... ohne Shift: Bilanzzahlen
    #' --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    mutate(fae_dat = year(fae_dat),
           fae_dat = paste("CF_",fae_dat,sep="")) %>%
    rename(CF_nr = fae_dat,
           CF_orginal = betrag) ->
    temp_table
  
  temp_table %>%
    mutate(CF_nr=str_remove(CF_nr,"CF_"),
           CF_nr=as.numeric(CF_nr)+1,
           CF_nr = paste("CF_", CF_nr, sep = "")) %>%
    rename(CF_verschoben = CF_orginal) %>%
    full_join(temp_table, .) %>%
    mutate(across(where(is.numeric) & starts_with("CF_"), ~ coalesce(.x,0))) %>%
    mutate(betrag = case_when(anfall_jahr>=.GJ & prop_nonprop=="prop" ~ 0.75*CF_orginal+0.25*CF_verschoben,# alle prop Vertraege (obl und alles fak) --> Annahme quartaerlich abgerechnet, also einen Monat spaeter ausgezahlt --> 0.25 rutscht ins naechste Jahr
                              anfall_jahr>=.GJ & prop_nonprop=="nprop" ~ 0*CF_orginal+1*CF_verschoben, # alle nprop Vertraege werden im Dezember abgerechnet --> Januar des naechsten Jahres fließt Betrag
                              TRUE ~ CF_orginal),
           CF_nr = str_remove(CF_nr,"CF_"),
           CF_nr = as.numeric(CF_nr),
           CF_nr = ymd(paste(CF_nr,"-07-31", sep = ""))) %>%
    rename(fae_dat = CF_nr) %>%
    select(-c(CF_orginal,CF_verschoben)) ->
    temp_table
  
  
  #' --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #' Shift durchgefuehrt
  
  
  temp_table %>% 
    mutate(anz_planjahre = as.numeric(anz_planjahre))  %>% 
    uncount(anz_planjahre, .id = "Pdat") %>%  # mutate(Pdat_2 = (Pdat+.GJ-1))-> d
    mutate(Pdat = (Pdat+.GJ),
           Pdat = ymd(paste(Pdat,"12-31", sep = "-")),
           anfall_dat = dmy(paste("01.01.",anfall_jahr, sep = "")),
           GuV_pos = case_when(year(fae_dat)<year(Pdat) ~ "Release of Incurred Claims", # Auszahlungen der Vorjahre
                               year(fae_dat)==year(Pdat) ~ "Zahlungen fuer Versicherungsfaelle",#"Versicherungsserviceaufwand", # Zahlungen des Geschaeftsjahres
                               TRUE ~ "Liability for Incurred Claims"),
           GuV_pos_nr = case_when(year(fae_dat)<year(Pdat) ~ "Payments_Past", # Auszahlungen der Vorjahre
                                  year(fae_dat)==year(Pdat) ~ "Payments_GJ",#"Versicherungsserviceaufwand", # Zahlungen des Geschaeftsjahres
                                  TRUE ~ "Payments_Future")) %>% # LIC Reserve
    filter(year(anfall_dat)<=year(Pdat)) -> # Anfalljahr kann nicht nach dem Plandatum (Stichtag der Bilanz) liegen fuer LIC
    DSS_schadenzahlungen_gj
  
  
  #' Ziaportfoid
  
  DSS_schadenzahlungen_gj %>% 
    left_join(ziaportfoid) %>% 
    mutate(fg_typ = "LIC",
           bereich = "KH",
           sti_dat = stichtag_planung,
           kohorte = anfall_jahr,
           gruppe = 3,
           GJ = year(ymd(stichtag_planung)),
           Pversion = Pversion,
           kost_erloesart = case_when(gesch_art == 30 ~"Z20000",
                                      TRUE ~ "Z21000"),
           bewertungsansatz = "PAA",
           kz_erwart = "EW") %>% 
    rename(wrt = betrag) %>% 
    filter(wrt!=0) %>% 
    mutate(across(ends_with("dat"), ~ case_when(nchar(month(.x))==1 & nchar(day(.x))==1 ~ paste(paste("0",day(.x), sep=""),paste("0",month(.x), sep = ""),year(.x), sep = "."),
                                                nchar(month(.x))==1 & nchar(day(.x))==2 ~ paste(day(.x),paste("0",month(.x), sep = ""),year(.x), sep = "."),
                                                nchar(month(.x))==2 & nchar(day(.x))==1 ~ paste(paste("0",day(.x), sep=""),month(.x),year(.x), sep = "."),
                                                TRUE  ~ paste(day(.x),month(.x), year(.x), sep = ".")))) %>% 
    select(-longtail_shorttail) ->
    DSS_schadenzahlungen_gj
  
  
  faktor_ra %>% 
    left_join(ziaportfoid) %>% 
    mutate(fg_typ = "LIC")->
    faktor_ra
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # ALL: Zinsen/Output Generieren ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("ALL: Zinsen/Output Generieren")
  
  #' Output Validierung erzeugen
  
  DSS_schadenzahlungen_gj %>% 
    # rbind(DSS_LRC) %>% 
    group_by(across(!matches("wrt"))) %>% 
    summarise(across(matches("wrt"), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup %>% 
    left_join(faktor_ra) %>% 
    mutate(faktor_ra = coalesce(faktor_ra,0),
           abs_ra = faktor_ra*wrt) ->
    temp_output_validierung
  
  #' Zinsen auswaehlen und aufbereiten
  
  temp_output_validierung %>% 
    select(Pdat) %>% 
    distinct %>% 
    mutate(Pdat = dmy(Pdat)) %>% 
    unlist %>% 
    unname %>% 
    as.Date( . , origin="1970-01-01") %>% 
    min -> 
    .min_Pdat
  
  #' Die Diskontierungsfaktoren werden nur aus Dezember verwendet, da die Zahlen ein ganzes Jahre repraesentieren
  # write.xlsx(diskontkurve,paste(filename,"diskontkurve.xlsx", sep = "/"))
  
  temp_output_validierung %>% #filter(gsch_bil=="040" & basis_pf=="NatCat" & gsch_par=="ext" & fg_typ=="LIC") %>%  # filter(gsch_bil=="001" & basis_pf=="Kraftfahrt_Haftpflicht" & anfall_jahr>=.GJ & fg_typ=="LIC") %>%  
    mutate(across(ends_with("dat"), ~ dmy(.x)),
           Monat = (year(fae_dat)-year(Pdat)),
           Pdat_vorher = year(Pdat)-1, # fuer Zinsaenderung Plandatum des vorhjahres hinzufuegen
           Pdat_vorher = ymd(paste(Pdat_vorher,"-12-31", sep = ""))) %>% 
    left_join(diskontkurve, by = c("Monat"="Monat","Pdat_vorher" = "stichtag_fortschreibung")) %>% 
    rename(diskontfaktor_vorher = diskontfaktor) %>% # umbenennen der Plandaten damit zwei Zinskurven in die Tabelle eingefuegt werden koennen
    left_join(diskontkurve, by = c("Monat"="Monat","Pdat" = "stichtag_fortschreibung")) %>% 
    mutate(diskontfaktor = case_when(Monat<=0 ~ 1, 
                                     TRUE ~ diskontfaktor),
           diskontfaktor_vorher = case_when((year(fae_dat)-year(Pdat_vorher))*12<=0 ~ 1, # wenn hier Gleichheit erlaubt, dann wird aktuelles Jahr nicht diskontiert
                                            year(Pdat) == anfall_jahr ~ diskontfaktor, # Stellt sicher, dass im GJ keine Zinsaenderung gerechnet wird
                                            TRUE ~ diskontfaktor_vorher)) %>% 
    #select(-Pdat_vorher) %>% 
    pivot_longer(cols = which(names(.) %in% c("wrt","abs_ra")), names_to = "BE_RA", values_to = "nominal") %>% 
    select(-c(Monat,faktor_ra)) %>% 
    mutate(BE_RA = recode(BE_RA,
                          "wrt" = "BE",
                          .default = "RA"),
           barwert = nominal*diskontfaktor,
           zinsen = nominal-barwert,
           # int_ext = int_ext_korr,
           zinsaenderung = case_when(fae_dat>=Pdat ~ nominal*(diskontfaktor-diskontfaktor_vorher),
                                     TRUE ~ 0)) %>% 
    # zinsaenderung = nominal*(diskontfaktor-diskontfaktor_vorher)) %>% 
    pivot_longer(cols = which(names(.) %in% c("nominal","barwert","zinsen","zinsaenderung")), names_to = "Kategorie",values_to = "wrt" ) %>% 
    mutate(GuV_pos = case_when(str_detect(GuV_pos,"Beitraege") ~ "Versicherungsumsatz", # LRC Thema
                               TRUE ~ GuV_pos)) %>% 
    group_by(across(!matches(c("wrt","fae_dat","diskontfaktor","diskontfaktor_vorher")))) %>% # fae_dat wegaggregieren
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(Kategorie = case_when(Kategorie=="zinsaenderung" & wrt < 0 & gesch_art!=30 ~ "zinsen_ertrag",
                                 Kategorie=="zinsaenderung" & wrt >= 0 & gesch_art!=30~ "zinsen_aufwand",
                                 Kategorie=="zinsaenderung" & wrt < 0 & gesch_art==30 ~ "zinsen_aufwand",
                                 Kategorie=="zinsaenderung" & wrt >= 0 & gesch_art==30 ~ "zinsen_ertrag",
                                 TRUE ~ Kategorie),
           GuV_pos = case_when(Kategorie == "zinsen_aufwand" ~ "Zinsaufwand (Zinsaenderung und Locked-in Zins)",
                               Kategorie == "zinsen_ertrag" ~ "Zinsertrag (Zinsaenderung und Locked-in Zins)",
                               TRUE ~ GuV_pos),
           wrt = case_when(gesch_art==30 ~ -wrt,
                           TRUE ~ wrt),
           daten_quelle = case_when(kohorte<.GJ ~ "LIC (IST-Werte)",
                                    kohorte==.GJ ~ "Hochrechnung",
                                    kohorte>.GJ ~ "Planung")) %>% 
    select(names(.)[c(1:(ncol(.)-2),ncol(.),(ncol(.)-1))]) -> # setze wrt-Spalte an die letzte Stelle
    datenbank
  
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # ALL: Notwendige Daten herauslesen und aufbereiten ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("ALL: Notwendige Daten herauslesen und aufbereiten")
  
  
  
  #' LRC, LIC, Zinsaenderung auswaehlen
  datenbank %>% 
    filter((fg_typ=="LRC" & Kategorie=="nominal" & BE_RA=="BE") | # LRC ist undiskontiert und ohne Risikoanpassung (PAA)
             (fg_typ=="LIC" & Kategorie=="barwert") | # LIC alles diskontiert, Release wird nicht mit ausgegeben
             (fg_typ=="LIC" & grepl("~*zinsen_",Kategorie))) ->
    # left_join(Sachkonten) -> 
    Reserven_Zinsen
  
  
  #' Veraenderung LIC/LRC hinzufuegen
  
  #' Aufloesung: Daten nur aus Vorjahren
  datenbank %>% 
    filter(Kategorie=="nominal" & wrt!=0) %>% # Datenauswahl fuer Planung und Hochrechnung
    filter(GuV_pos_nr=="Payments_GJ" & (year(Pdat)-1)>=anfall_jahr & Pdat>.min_Pdat) %>% # Datenauswahl fuer Veraenderung LIC/LRC --> kann nur fg_typ = LIC enthalten
    mutate(wrt = -wrt, # Gegenposition herstellen
           GuV_pos = case_when(fg_typ=="LIC" ~ "Veraenderung LIC",
                               fg_typ=="LRC" ~ "Veraenderung LRC"),
           GuV_pos_nr= "Veraenderung Aufloesung VJ") %>% 
    group_by(across(names(.)[-ncol(.)])) %>% 
    summarise(across(names(.)[ncol(.)], ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup ->
    Aufloesung
  
  #' (year(Pdat)-1)>=anfall_jahr : Grund dass es keine Aufloesung in LRC PAA gibt
  
  #' Neue LIC
  
  datenbank %>% 
    filter(Kategorie=="barwert" & wrt !=0) %>% # Datenauswahl fuer Planung und Hochrechnung
    filter(year(Pdat)==anfall_jahr & Pdat>.min_Pdat & GuV_pos_nr!="Payments_GJ") %>% # Entferne "& GuV_pos_nr!="Payments_GJ" --> Zahlung GJ Neugeschaeft wird beruecksichtigt
    mutate(wrt = case_when(GuV_pos_nr=="Payments_GJ" ~ -wrt, # GJ Zahlung als Zahlung oder LIC
                           TRUE ~ wrt),
           GuV_pos_nr =case_when(GuV_pos_nr=="Payments_GJ" ~ "Veraenderung Aufloesung GJ",
                                 TRUE ~ "Veraenderung Neugeschaeft"),
           GuV_pos = case_when(fg_typ=="LIC" ~ "Veraenderung LIC",
                               fg_typ=="LRC" ~ "Veraenderung LRC")) %>% 
    group_by(across(names(.)[-ncol(.)])) %>% 
    summarise(across(names(.)[ncol(.)], ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup ->
    Neugeschaeft
  
  
  Neugeschaeft %>% 
    select(names(Aufloesung)) %>% 
    rbind(Aufloesung) %>% 
    select(names(Reserven_Zinsen)) %>% 
    rbind(Reserven_Zinsen) ->
    output_validierung
  
  
  output_validierung %>% 
    filter(BE_RA=="RA" & int_ext_korr=="int" & gesch_art!=30) -> # filtere auf Daten die gespiegelt werden muessen
    temp_ra_spiegeln
  
  output_validierung %>% 
    filter(!(BE_RA=="RA" & int_ext_korr=="int" & gesch_art==30)) -> # filtere auf Daten die fertig sind
    output_validierung
  
  
  #' Spiegelung der Risikoanpassung
  
  temp_ra_spiegeln %>% 
    rename(gsch_bil = gsch_par,
           gsch_par = gsch_bil) %>% 
    mutate(gesch_art = 30,
           kost_erloesart = case_when(gesch_art == 30 ~"Z20000",
                                      TRUE ~ "Z21000"),
           wrt = -wrt) %>% 
    group_by(across(!matches("wrt"))) %>% 
    summarise(across(matches("wrt"), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup %>% 
    select(names(output_validierung)) %>% 
    rbind(output_validierung) %>% 
    mutate(ag_ueg = case_when(gesch_art==30 ~ "ueg",
                              TRUE ~ "ag"),
           wrt= case_when(str_detect(GuV_pos,"Zins") & Pdat==.min_Pdat ~ 0,
                          TRUE ~ wrt)) %>% 
    filter(!(fg_typ=="LRC" & GuV_pos=="Veraenderung LRC")) %>% 
    mutate(across(ends_with("dat"), ~ case_when(nchar(month(.x))==1 & nchar(day(.x))==1 ~ paste(paste("0",day(.x), sep=""),paste("0",month(.x), sep = ""),year(.x), sep = "."),
                                                nchar(month(.x))==1 & nchar(day(.x))==2 ~ paste(day(.x),paste("0",month(.x), sep = ""),year(.x), sep = "."),
                                                nchar(month(.x))==2 & nchar(day(.x))==1 ~ paste(paste("0",day(.x), sep=""),month(.x),year(.x), sep = "."),
                                                TRUE  ~ paste(day(.x),month(.x), year(.x), sep = ".")))) %>% 
    arrange_all ->
    Output_LIC
  
  
  
  rm(temp_output_validierung,temp_ra_spiegeln,output_validierung)
  
  wb<-createWorkbook()
  
  addWorksheet(wb,"LIC", zoom = 80)
  
  writeData(wb,"LIC",Output_LIC)
  
  saveWorkbook(wb,paste(filename,"Output","LIC.xlsx", sep = "/"), overwrite = TRUE)
  
  
  
  
  
  
  
  
  
}