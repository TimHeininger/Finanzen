I_Planung_LIC<-function(filename,
                        stichtag_daten,
                        stichtag_planung,
                        schema,
                        name_cockpit_planung,
                        pfad_R_lib,
                        con){
  
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
  pfad_input_daten<-filename
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Parameter aus Exceldatei laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  .input_parameter <- read_excel(paste(filename,name_cockpit_planung, sep = "/"), sheet = "Parameter_R_Code")
  .input_parameter <- .input_parameter[which(.input_parameter$Berechnungsschritt == "LIC"),]

  .variables <- unique(.input_parameter$Variablen_name)

  for (.i in 1:length(.variables)) {

    assign(.variables[.i], as.vector(.input_parameter$Wert[.input_parameter$Variablen_name == .variables[.i]], mode = unique(.input_parameter$Type[.input_parameter$Variablen_name == .variables[.i]])))

  }


  rm(.input_parameter)
  
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Input laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  read_excel(paste(pfad_input_daten,name_cockpit_planung, sep = "/"), sheet = .sheet_plandaten) %>% 
    filter(if_all(names(.)[ncol(.)], ~ .x!=0)) %>% 
    mutate(kges = recode(kges, ext = as.character(NA))) ->
    input_plandaten
  
  
  read_excel(paste(pfad_input_daten,name_cockpit_planung, sep = "/"), sheet = .sheet_bafin_sparten) %>% 
    # rename(gsch_bil = `bil. Gesellschaft`,
    #        gsch_par = Gegenpartei) %>% 
    filter(IFRS_17!="NICHT_RELEVANT" & !is.na(IFRS_17)) %>% 
    select(basis_pf,bav_zweig_nr,IFRS_17,gsch_bil,gsch_par,ag_ueg) %>% 
    distinct ->
    input_bafinsparten
  
  
  read.xlsx(paste(pfad_input_daten,"Inputs",.name_kennzeichen_datei, sep = "/"), sheet = .sheet_kennzeichen_datei) %>% 
    rename(basis_pf = rc_pf) ->
    input_C_Kennzeichen
  
  
  # read.xlsx(paste(pfad_input_daten,"Inputs",.name_zinsen, sep = "/"), sheet = .sheet_zinsen) %>% 
  #   mutate(Monat = row_number()) %>% 
  #   pivot_longer(cols = c(1:(ncol(.)-1)),names_to = "anfall_dat_von",values_to = "diskontfaktor") %>% 
  #   mutate(anfall_dat_von = as.numeric(anfall_dat_von),
  #          anfall_dat_von = as.Date(anfall_dat_von, origin="1899-12-30")) -> 
  #   diskontierung_aufbereitet
  # 
  # 
  # .laenge_zinsen<-nrow(diskontierung_aufbereitet)
  
  
  input_plandaten %>% 
    select(plan_jahr) %>% 
    distinct %>% 
    nrow -> 
    .anz_planjahre
  
    .GJ<-year(ymd(stichtag_planung))-1
  
    stichtag_planung<-ymd(stichtag_planung)

  
  
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # ALL: DSS Daten laden ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
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
    
    
    
    c(names(input_C_Kennzeichen),names(mapping_int_ext),"ag_ueg") %>% 
      matrix(nrow = length(.), ncol = 1) %>% 
      tibble %>% 
      set_names("names") %>% 
      distinct %>% 
      nrow ->
      .spalte_kennzeichen_ag_ueg
    
    
    input_C_Kennzeichen %>% 
      filter(lic_methode!="Spiegelung") %>% 
      filter(if_all(names(.)[.spalte_kennzeichen_FuV], ~ . != "FuV")) %>% # Schliesse fuer Schluesselung Forderungen und Verbindlichkeiten aus, da sie in den HGB Planzahlen nicht vorhanden sind
      filter(if_all(names(.)[.spalte_kennzeichen_kost_erloesart], ~ . != "RfB")) %>% # schliesse RfB aus, behalte Renten und Schadenzahlungen
      left_join(mapping_int_ext) %>% 
      #' Korrektur der Kennzeichen auf Planungskennzeichen
      mutate(int_ext_korr = coalesce(int_ext_korr,"ext"),
             ag_ueg = case_when(gesch_art == 30 ~ "ueg",
                                TRUE ~ "ag"),
             gsch_par= case_when(int_ext_korr=="ext" ~ "ext",
                                 TRUE ~ gsch_par),
             gesch_art = as.numeric(gesch_art),
             prop_nonprop = case_when(fak_obl=="fak" ~ "prop",
                                      TRUE ~ prop_nonprop),
             kges = case_when(nchar(kges)==1 ~ paste("00",kges, sep = ""),
                              nchar(kges)==2 ~ paste("0",kges,sep = ""),
                              TRUE ~ as.character(kges))) ->
      C_Kennzeichen
    
    names(C_Kennzeichen)[c(.spalte_kennzeichen_BE,.spalte_kennzeichen_HGB)] <- c("BE","HGB")
    
    
    
    #' Daten für Segmentierung
    
    ("select
gesch_art,
CASE WHEN fak_obl='fak' THEN 'prop' ELSE prop_nonprop END as prop_nonprop,
fak_obl,
gsch_bil,
basis_pf,
kohorte as anfall_jahr,
CASE WHEN int_ext = 'int' THEN gsch_par ELSE 'ext' END as gsch_par,
CASE WHEN gesch_art = 30 THEN 'ueg' ELSE 'ag' END as ag_ueg

from schema_abfrage.s_rg_seg_ifrs_mega_mapping 

where stichtag = CAST('stichtag_abfrage' as Date) and kohorte<=GJ_abfrage


group by 1,2,3,4,5,6,7,8

") %>% 
      str_replace_all("stichtag_abfrage", as.character(stichtag_daten)) %>%
      str_replace_all("schema_abfrage",schema) %>% 
      str_replace_all("GJ_abfrage",as.character(.GJ)) %>% 
      dbGetQuery(con, . ) %>% 
      cbind("HGB"= 0, "BE"=0) ->
      mapping_segmentierung
    
    
    data.table::fread(paste(filename,"Inputs","r_rg_faktor_ra_lic.csv", sep = "\\"), dec = ",", sep = ";") %>% 
      mutate(Kennzeichen = case_when(endsWith(kost_erloesart,"7") ~ "Rente",
                                     endsWith(kost_erloesart,"11") ~ "RfB",
                                     TRUE ~ "Cashflow"),
             kges = as.character(kges),
             across(names(.)[c(1,2,11)], ~ case_when(nchar(.x)==1 ~ paste("00",.x, sep = ""),
                                                     nchar(.x)==2 ~ paste("0",.x,sep = ""),
                                                     TRUE ~ as.character(.x))),
             fg_typ = "LIC")  %>% 
      # select(-kost_erloesart) %>% 
      filter(Kennzeichen!="RfB") %>% 
      select(gsch_bil,gsch_par,gesch_art,anfalljahr,kges,basis_pf,prop_nonprop,fak_obl,fg_typ,faktor_ra,Kennzeichen) %>% 
      rename(anfall_jahr = anfalljahr) ->
      s_ifrs_rg_faktor_ra_sn
    
    
    
    #' Ra Faktoren abziehen
    
    # ("select 
    #  gsch_bil,
    #  gsch_par,
    #  gesch_art,
    #  anfalljahr as anfall_jahr,
    #  kost_erloesart,
    #  kges,
    #  basis_pf,
    #  prop_nonprop,
    #  fak_obl,
    #  fg_typ,
    #  faktor_ra
    # 
    #   
    #   from schema_abfrage.s_ifrs_rg_faktor_ra_sn
    #   
    #   where stichtag = CAST('stichtag_abfrage' as date) and guelt_stat = 'L' and fg_typ = 'LIC'
    #   
    #   group by 1,2,3,4,5,6,7,8,9,10,11
    #   ") %>% 
    #   str_replace_all("stichtag_abfrage", as.character(stichtag_daten)) %>%
    #   str_replace_all("schema_abfrage",schema) %>% 
    #   dbGetQuery(con, . ) %>% 
    #   mutate(Kennzeichen = case_when(endsWith(kost_erloesart,"7") ~ "Rente",
    #                                  endsWith(kost_erloesart,"11") ~ "RfB",
    #                                  TRUE ~ "Cashflow")) %>% 
    #   select(-kost_erloesart) %>% 
    #   filter(Kennzeichen!="RfB") ->
    #   s_ifrs_rg_faktor_ra_sn
    
    
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
    # Zins fortschreiben ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    # input_plandaten %>%
    #   select(plan_jahr) %>%
    #   distinct %>%
    #   mutate(plan_jahr = ymd(paste(plan_jahr,"-12-31", sep = ""))) %>%
    #   unlist %>%
    #   unname %>%
    #   as.Date( . , origin="1970-01-01") ->
    #   .liste_stichtage
    # 
    # 
    # diskont_fortschreibung_TH(diskontierung_aufbereitet,.liste_stichtage,.laenge_zinsen) ->
    #   diskontkurve
    
    
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # LIC: RA Faktor Planung ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    print("LIC: RA Faktor Planung")
    
    s_ifrs_rg_faktor_ra_sn %>% 
      filter(fg_typ=="LIC") -> 
      temp_ra_lic
    
    #' Faktor Plandaten berechnen
    
    input_C_Kennzeichen %>%
      mutate(gesch_art = as.numeric(gesch_art),
             kges = case_when(nchar(kges)==1 ~ paste("00",kges, sep = ""),
                              nchar(kges)==2 ~ paste("0",kges,sep = ""),
                              TRUE ~ as.character(kges))) %>% 
      filter(lic_methode!="Spiegelung" & BE!=0 & FuV!="FuV" & Kennzeichen!="RfB" & Kennzeichen!="Rente") %>% 
      group_by(across(names(.)[c(.spalte_kennzeichen_gsch_bil,
                                 .spalte_kennzeichen_gsch_par,
                                 .spalte_kennzeichen_gesch_art,
                                 .spalte_kennzeichen_anfall_jahr,
                                 .spalte_kennzeichen_kost_erloesart,
                                 .spalte_kennzeichen_kges,
                                 .spalte_kennzeichen_basis_pf,
                                 .spalte_kennzeichen_prop_nonprop,
                                 .spalte_kennzeichen_fak_obl,
                                 .spalte_kennzeichen_long_short)])) %>% 
      summarise(across(names(.)[.spalte_kennzeichen_BE], ~ sum(.x, na.rm = TRUE))) %>% 
      ungroup %>% 
      # rename(basis_pf = rc_pf) %>% 
      left_join(temp_ra_lic) %>% 
      left_join(mapping_int_ext) %>%
      mutate(faktor_ra = faktor_ra*BE,
             int_ext_korr = coalesce(int_ext_korr,"ext"),
             gsch_par= case_when(int_ext_korr=="ext" ~ "ext",
                                 TRUE ~ gsch_par),
             prop_nonprop= case_when(fak_obl=="fak" ~ "prop",
                                     TRUE ~ prop_nonprop)) %>% 
      group_by(gsch_bil,gsch_par,gesch_art,anfall_jahr,basis_pf,prop_nonprop,fak_obl) %>% 
      summarise(BE = sum(BE, na.rm = TRUE),
                faktor_ra = sum(faktor_ra, na.rm = TRUE)) %>% 
      ungroup ->
      temp_ra
    
    
    temp_ra %>% 
      group_by(gsch_bil,gsch_par,gesch_art,basis_pf,prop_nonprop,fak_obl) %>%
      filter(anfall_jahr == max(anfall_jahr)) %>% 
      ungroup %>% 
      mutate(faktor_ra = faktor_ra/BE) %>% 
      select(-c(BE,anfall_jahr)) ->
      faktor_ra
    
    #' RA Faktor Standard berechne
    
    
    input_C_Kennzeichen %>%
      mutate(gesch_art = as.numeric(gesch_art),
             kges = case_when(nchar(kges)==1 ~ paste("00",kges, sep = ""),
                              nchar(kges)==2 ~ paste("0",kges,sep = ""),
                              TRUE ~ as.character(kges))) %>% 
      filter(lic_methode!="Spiegelung" & BE!=0 & FuV!="FuV" & Kennzeichen!="RfB" & Kennzeichen!="Rente") %>% 
      group_by(across(names(.)[c(.spalte_kennzeichen_gsch_bil,
                                 .spalte_kennzeichen_gsch_par,
                                 .spalte_kennzeichen_gesch_art,
                                 .spalte_kennzeichen_anfall_jahr,
                                 .spalte_kennzeichen_kost_erloesart,
                                 .spalte_kennzeichen_kges,
                                 .spalte_kennzeichen_basis_pf,
                                 .spalte_kennzeichen_prop_nonprop,
                                 .spalte_kennzeichen_fak_obl,
                                 .spalte_kennzeichen_long_short)])) %>% 
      summarise(across(names(.)[.spalte_kennzeichen_BE], ~ sum(.x, na.rm = TRUE))) %>% 
      ungroup %>%
      left_join(temp_ra_lic) %>% 
      left_join(mapping_int_ext) %>%
      mutate(faktor_ra = faktor_ra*BE,
             int_ext_korr = coalesce(int_ext_korr,"ext"),
             gsch_par= case_when(int_ext_korr=="ext" ~ "ext",
                                 TRUE ~ gsch_par),
             prop_nonprop= case_when(fak_obl=="fak" ~ "prop",
                                     TRUE ~ prop_nonprop)) %>% 
      group_by(longtail_shorttail) %>% 
      summarise(BE = sum(BE, na.rm = TRUE),
                faktor_ra = sum(faktor_ra, na.rm = TRUE)) %>% 
      ungroup %>% 
      mutate(faktor_ra_standard = faktor_ra/BE) %>% 
      select(-c(BE,faktor_ra)) ->
      faktor_ra_standard
    
    #' Faktor RA fuer die Vorjahre berechnen
    
    input_C_Kennzeichen %>%
      mutate(gesch_art = as.numeric(gesch_art),
             kges = case_when(nchar(kges)==1 ~ paste("00",kges, sep = ""),
                              nchar(kges)==2 ~ paste("0",kges,sep = ""),
                              TRUE ~ as.character(kges))) %>% 
      filter(BE!=0 & FuV!="FuV" & Kennzeichen!="RfB" & anfall_jahr<.GJ) %>% 
      group_by(across(names(.)[c(.spalte_kennzeichen_gsch_bil,
                                 .spalte_kennzeichen_gsch_par,
                                 .spalte_kennzeichen_gesch_art,
                                 .spalte_kennzeichen_anfall_jahr,
                                 .spalte_kennzeichen_kost_erloesart,
                                 .spalte_kennzeichen_kges,
                                 .spalte_kennzeichen_basis_pf,
                                 .spalte_kennzeichen_prop_nonprop,
                                 .spalte_kennzeichen_fak_obl,
                                 .spalte_kennzeichen_long_short)])) %>% 
      summarise(across(names(.)[.spalte_kennzeichen_BE], ~ sum(.x, na.rm = TRUE))) %>% 
      ungroup %>%
      left_join(temp_ra_lic) %>% 
      left_join(mapping_int_ext) %>%
      mutate(faktor_ra = faktor_ra*BE,
             gsch_par= case_when(int_ext_korr=="ext" ~ "ext",
                                 TRUE ~ gsch_par) ) %>% 
      group_by(gsch_bil,gsch_par,gesch_art,anfall_jahr,basis_pf,prop_nonprop,fak_obl) %>% 
      summarise(BE = sum(BE, na.rm = TRUE),
                faktor_ra = sum(faktor_ra, na.rm = TRUE)) %>% 
      ungroup %>% 
      group_by(gsch_bil,gsch_par,gesch_art,basis_pf,prop_nonprop,fak_obl,anfall_jahr) %>%
      ungroup %>% 
      mutate(faktor_ra = faktor_ra/BE) %>% 
      select(-BE) ->
      faktor_ra_vorjahre
    
    
    #' Begrenze RA-Faktoren
    
    faktor_ra %<>% 
      mutate(faktor_ra = case_when(faktor_ra>0.20 ~ 0.05,
                                   faktor_ra<0.001 ~ 0.02,
                                   TRUE ~ faktor_ra))
    
    
    faktor_ra_vorjahre %<>% 
      mutate(faktor_ra = case_when(faktor_ra>0.20 ~ 0.05,
                                   faktor_ra<0.001 ~ 0.02,
                                   TRUE ~ faktor_ra))
    
    rm(temp_ra,temp_ra_lic)
    
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # ALL: Skalierungsfaktor Schadendaten  ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    print("ALL: Skalierungsfaktor Schadendaten")
    
    #' In der Prognose der Ist Cashflows wird versucht auf Grund der Abrechnung die eigentliche Zahlung zu Prognostizieren --> Shift um 1 Monat der Cashflows
    #' Planzahlen entsprechen aber dem ersten vollständigen Jahr --> Fur Berechnung des BE_planung muss der Shift rueckgaengig gemacht werden
    
    C_Kennzeichen %>% 
      group_by(across(names(.)[c(.spalte_kennzeichen_gesch_art,
                                 .spalte_kennzeichen_prop_nonprop,
                                 .spalte_kennzeichen_fak_obl,
                                 .spalte_kennzeichen_gsch_bil,
                                 .spalte_kennzeichen_basis_pf,
                                 .spalte_kennzeichen_anfall_jahr,
                                 .spalte_kennzeichen_gsch_par,
                                 .spalte_kennzeichen_ag_ueg,
                                 .spalte_kennzeichen_long_short)])) %>% 
      summarise(across(c(BE,CF_2,CF_3,CF_4,CF_5,CF_6,CF_7,CF_8,CF_9,CF_10,CF_11,CF_12,CF_13), ~ sum(.x, na.rm = TRUE))) %>% # 
      ungroup %>% 
      filter(BE!=0) %>% 
      pivot_longer(cols = starts_with("CF_"), names_to = "CF_nr",values_to = "wrt" ) %>%  # Berechne Auszahlungen im ersten Jahr
      select(-CF_nr) %>% 
      group_by(across(!matches("wrt"))) %>% 
      summarise(auszahlung_erstes_jahr = sum(wrt, na.rm = TRUE)) %>% 
      ungroup %>% 
      mutate(skalierung = auszahlung_erstes_jahr/BE) %>% 
      select(-c(BE,auszahlung_erstes_jahr)) %>% 
      group_by(across(!matches(c("skalierung","anfall_jahr")))) %>% 
      filter(anfall_jahr==max(anfall_jahr,na.rm = TRUE)) %>% # nur maximales Anfalljahr behalten
      ungroup %>% 
      select(-anfall_jahr)->
      skalierung
    
    
    #' Skalierung mit standardfaktor
    
    
    
    C_Kennzeichen %>% 
      group_by(across(names(.)[c(.spalte_kennzeichen_anfall_jahr,
                                 .spalte_kennzeichen_long_short)])) %>% 
      summarise(across(c(BE,CF_2,CF_3,CF_4,CF_5,CF_6,CF_7,CF_8,CF_9,CF_10,CF_11,CF_12,CF_13), ~ sum(.x, na.rm = TRUE))) %>% # 
      ungroup %>% 
      filter(BE!=0) %>% 
      pivot_longer(cols = starts_with("CF_"), names_to = "CF_nr",values_to = "wrt" ) %>%  # Berechne Auszahlungen im ersten Jahr
      select(-CF_nr) %>% 
      group_by(across(!matches("wrt"))) %>% 
      summarise(auszahlung_erstes_jahr = sum(wrt, na.rm = TRUE)) %>% 
      ungroup %>% 
      mutate(skalierung = auszahlung_erstes_jahr/BE) %>% 
      select(-c(BE,auszahlung_erstes_jahr)) %>% 
      group_by(across(!matches(c("skalierung","anfall_jahr")))) %>% 
      filter(anfall_jahr==max(anfall_jahr,na.rm = TRUE)) %>% # nur maximales Anfalljahr behalten
      ungroup %>% 
      select(-anfall_jahr) ->
      skalierung_standard
    
    
    
    
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # LIC: Mapping fuer Schluesselung erstellen fuer Schadenzahlung GJ ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    print("LIC: Mapping fuer Schluesselung erstellen fuer Schadenzahlung GJ")
    
    #' Schluessel zur Aufteilung auf basis_pf berechnen
    
    #' Spalte ermitteln in die ag_ueg geschrieben wird (nicht trivial, da sich namen ueberschneiden)
    
    C_Kennzeichen %>% 
      group_by(across(names(.)[c(.spalte_kennzeichen_gesch_art,
                                 .spalte_kennzeichen_prop_nonprop,
                                 .spalte_kennzeichen_fak_obl,
                                 .spalte_kennzeichen_gsch_bil,
                                 .spalte_kennzeichen_basis_pf,
                                 .spalte_kennzeichen_anfall_jahr,
                                 .spalte_kennzeichen_gsch_par,
                                 #.spalte_kennzeichen_bav_zweig_nr,
                                 #.spalte_kennzeichen_FuV,
                                 #.spalte_kennzeichen_kost_erloesart,
                                 .spalte_kennzeichen_ag_ueg,
                                 .spalte_kennzeichen_long_short)])) %>% 
      summarise(across(names(.)[c(.spalte_kennzeichen_HGB,.spalte_kennzeichen_BE)], ~ sum(.x, na.rm = TRUE))) %>%
      ungroup ->
      temp_mapping_aufteilung_lic 
    
    #' Sicherstellen, dass alle GVVs existieren (insebesondere GVV die nur im Mega Mapping exsistieren, aber keine IST-Reserve haben, fehlen in der C_Kennzeichen Datei)
    
    temp_mapping_aufteilung_lic %>% 
      select(basis_pf,longtail_shorttail) %>% 
      distinct ->
      .mapping_long_short
    
    
    .mapping_long_short %>% 
      left_join(mapping_segmentierung, . ) %>% 
      mutate(longtail_shorttail = coalesce(longtail_shorttail,"Shorttail")) %>% # Alle nicht zugeordneten basis_pf werden auf Shorttail gesetzt
      select(names(temp_mapping_aufteilung_lic)) %>% # sicherstellen dass rbind Befehl funktioniert
      rbind(temp_mapping_aufteilung_lic) %>% 
      group_by(across(names(.)[1:(ncol(.)-2)])) %>% 
      summarise(across(names(.)[(ncol(.)-1):ncol(.)], ~ sum(.x, na.rm = TRUE))) %>% 
      ungroup -> 
      temp_mapping_aufteilung_lic
    
    
    
    
    # C_Kennzeichen %>% filter(gesch_art==20 & prop_nonprop=="nprop" & fak_obl=="obl" & gsch_bil=="023" & gsch_par =="ext")->d_2
    
    #' spalten temp_mapping auslesen
    .spalte_mapping_aufteilung_lic_gesch_art<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_gesch_art])
    .spalte_mapping_aufteilung_lic_long_short<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_long_short])
    .spalte_mapping_aufteilung_lic_prop_nonprop<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_prop_nonprop])
    .spalte_mapping_aufteilung_lic_fak_obl<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_fak_obl])
    .spalte_mapping_aufteilung_lic_gsch_bil<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_gsch_bil])
    .spalte_mapping_aufteilung_lic_basis_pf<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_basis_pf])
    .spalte_mapping_aufteilung_lic_anfall_jahr<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_anfall_jahr])
    .spalte_mapping_aufteilung_lic_gsch_par<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_gsch_par])
    .spalte_mapping_aufteilung_lic_bav_zweig_nr<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_bav_zweig_nr])
    #.spalte_mapping_aufteilung_lic_FuV<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_FuV])
    #.spalte_mapping_aufteilung_lic_kost_erloesart<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_kost_erloesart])
    .spalte_mapping_aufteilung_lic_ag_ueg<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_ag_ueg])
    .spalte_mapping_aufteilung_lic_HGB<-ncol(temp_mapping_aufteilung_lic)-1
    .spalte_mapping_aufteilung_lic_BE<-ncol(temp_mapping_aufteilung_lic)
    
    
    temp_mapping_aufteilung_lic %>% 
      group_by(across(names(.)[-c(.spalte_mapping_aufteilung_lic_anfall_jahr,
                                  .spalte_mapping_aufteilung_lic_HGB,
                                  .spalte_mapping_aufteilung_lic_BE)])) %>% 
      #.spalte_mapping_aufteilung_lic_FuV,
      #.spalte_mapping_aufteilung_lic_kost_erloesart)])) %>% 
      summarise(across(names(.)[.spalte_mapping_aufteilung_lic_anfall_jahr], ~ max(.x, na.rm = TRUE), .names = "max_aj")) %>% 
      ungroup %>% 
      left_join(temp_mapping_aufteilung_lic, .) %>% 
      filter(if_all(names(.)[.spalte_mapping_aufteilung_lic_anfall_jahr], ~ . == max_aj)) %>% # behalte nur das maximale anfalljahr
      select(-max_aj) %>% 
      full_join(input_bafinsparten) %>%
      group_by(across(names(.)[c(.spalte_mapping_aufteilung_lic_gesch_art,
                                 .spalte_mapping_aufteilung_lic_prop_nonprop,
                                 .spalte_mapping_aufteilung_lic_fak_obl,
                                 .spalte_mapping_aufteilung_lic_gsch_bil,
                                 .spalte_mapping_aufteilung_lic_basis_pf,
                                 #.spalte_mapping_aufteilung_lic_anfall_jahr,
                                 .spalte_mapping_aufteilung_lic_gsch_par,
                                 .spalte_mapping_aufteilung_lic_long_short,
                                 ncol(.),
                                 #.spalte_mapping_aufteilung_lic_FuV,
                                 #.spalte_mapping_aufteilung_lic_kost_erloesart,
                                 .spalte_mapping_aufteilung_lic_ag_ueg)])) %>% 
      summarise(across(names(.)[c(.spalte_mapping_aufteilung_lic_HGB,.spalte_mapping_aufteilung_lic_BE)], ~ sum(.x, na.rm = TRUE))) %>% # pro Gruppe maximalen AJ nur behalten
      ungroup ->
      mapping_aufteilung_lic
    
    mapping_aufteilung_lic %>% 
      filter(is.na(gesch_art)) %>% 
      select(gsch_bil,basis_pf,gsch_par,IFRS_17,ag_ueg,HGB,BE) ->
      Fehler_Segmentierung
    
    input_plandaten %>% 
      inner_join(Fehler_Segmentierung, by = c("gsch_bil"="gsch_bil","gsch_par"="gsch_par","bafin_sparte"="IFRS_17")) %>% 
      left_join(.mapping_long_short) %>% 
      rename(IFRS_17=bafin_sparte) %>% 
      select(names(mapping_aufteilung_lic)) %>% 
      distinct %>% 
      rbind(mapping_aufteilung_lic) ->
      mapping_aufteilung_lic
    
    
    
    #' spalten mapping auslesen
    .spalte_mapping_aufteilung_lic_gesch_art<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_gesch_art])
    .spalte_mapping_aufteilung_lic_long_short<-which(names(temp_mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_long_short])
    .spalte_mapping_aufteilung_lic_prop_nonprop<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_prop_nonprop])
    .spalte_mapping_aufteilung_lic_fak_obl<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_fak_obl])
    .spalte_mapping_aufteilung_lic_gsch_bil<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_gsch_bil])
    .spalte_mapping_aufteilung_lic_basis_pf<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_basis_pf])
    .spalte_mapping_aufteilung_lic_gsch_par<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_gsch_par])
    .spalte_mapping_aufteilung_lic_bav_zweig_nr<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_bav_zweig_nr])
    # .spalte_mapping_aufteilung_lic_FuV<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_FuV])
    # .spalte_mapping_aufteilung_lic_kost_erloesart<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_kost_erloesart])
    .spalte_mapping_aufteilung_lic_ag_ueg<-which(names(mapping_aufteilung_lic)==names(C_Kennzeichen)[.spalte_kennzeichen_ag_ueg])
    .spalte_mapping_aufteilung_lic_HGB<-ncol(mapping_aufteilung_lic)-1
    .spalte_mapping_aufteilung_lic_BE<-ncol(mapping_aufteilung_lic)
    
    
    mapping_aufteilung_lic %<>%
      rename(bafin_sparte = IFRS_17) %>% 
      mutate(across(names(.)[.spalte_mapping_aufteilung_lic_gesch_art], ~ as.numeric(.x)))
    
    names(mapping_aufteilung_lic)[c(.spalte_mapping_aufteilung_lic_HGB,.spalte_mapping_aufteilung_lic_BE)]<-c("HGB","BE") # Jetzt kann mit Namen der Spalte gearbeitet werden
    
    rm(temp_mapping_aufteilung_lic)
    
    
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # LIC: Kennzeichen Plandaten fuer Schadenzahlungen GJ ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    print("LIC: Kennzeichen Plandaten fuer Schadenzahlungen GJ")
    
    #'  Kennzeichen anfuegen
    
    input_plandaten %>% 
      filter(if_all(names(.)[.spalte_plandatei_betragsart], ~ . =="Schadenzahlung GJ")) %>% #nrow
      filter(!(gesch_art==30 & gsch_par!="ext")) %>%
      left_join(mapping_aufteilung_lic) ->
      temp_plandaten
    
    #' Anteil berechnen
    
    temp_plandaten %>% 
      group_by(across(names(input_plandaten))) %>% 
      summarise(across(names(.)[c(ncol(.)-1)], ~ sum(.x, na.rm = TRUE), .names = "nenner_HGB")) %>% 
      ungroup %>% 
      left_join(temp_plandaten, . ) ->
      plandaten
    
    temp_plandaten %>% 
      group_by(across(names(input_plandaten))) %>% 
      summarise(across(names(.)[c(ncol(.))], ~ sum(.x, na.rm = TRUE), .names = "nenner_BE"),
                anzahl = n()) %>% 
      ungroup %>% 
      left_join(plandaten, . ) ->
      plandaten
    
    
    plandaten %>% 
      mutate(anteil = case_when(nenner_BE!=0 ~ BE/nenner_BE, # Wenn IFRS Resere existiert hiermit aufteilen
                                nenner_BE==0 & nenner_HGB!=0 ~ HGB/nenner_HGB, # Wenn keine IFRS Reserve aber HGB existiert, dann damit
                                TRUE ~ 1/anzahl), # sonst gleichmaessig aufteilen
             quelle_anteil = case_when(nenner_BE!=0 ~ "IFRS", 
                                       nenner_BE==0 & nenner_HGB!=0 ~ "HGB", 
                                       TRUE ~ "gleichmaessig")) %>% 
      select(-c(HGB,BE,nenner_HGB,nenner_BE,anzahl)) ->
      plandaten
    
    
    #' Dopplungen betrachten
    
    plandaten %>% 
      select(names(input_plandaten)) %>% 
      group_by_all %>% 
      summarise(anzahl = n()) %>% 
      ungroup %>% 
      left_join(plandaten) %>% 
      filter(anzahl > 1) ->
      duplikate_plandaten
    
    
    plandaten %>% 
      filter(is.na(basis_pf)) -> 
      Fehler_Zuweisung_LIC
    
    
    
    rm(temp_plandaten)
    
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # LIC: Berechnung der Pattern fuer Schadenzahlungen GJ ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    print("LIC: Berechnung der Pattern fuer Schadenzahlungen GJ")
    
    C_Kennzeichen %>% 
      # select(-CF_1) %>% # Shift entfernen
      filter(lic_methode!="Spiegelung" & !(Kennzeichen %in% c("Rente","RfB"))) %>% 
      mutate(across(where(is.numeric) & starts_with(c("BE","CF_")), ~ case_when(gesch_art == 30 ~ -.x,
                                                                                TRUE ~ .x))) %>% 
      group_by(across(names(.)[c(.spalte_kennzeichen_gesch_art,
                                 .spalte_kennzeichen_prop_nonprop,
                                 .spalte_kennzeichen_fak_obl,
                                 .spalte_kennzeichen_gsch_bil,
                                 .spalte_kennzeichen_basis_pf,
                                 .spalte_kennzeichen_anfall_jahr,
                                 .spalte_kennzeichen_gsch_par,
                                 (.spalte_kennzeichen_ag_ueg-1),
                                 .spalte_kennzeichen_long_short)])) %>% 
      summarise(across(names(.)[c(.spalte_kennzeichen_BE)] | starts_with("CF_"), ~ sum(.x, na.rm = TRUE))) %>% 
      ungroup %>% 
      filter(if_any(starts_with("BE") | starts_with("CF_"), ~ . != 0)) %>%    # nur behalten, falls Pattern auch berechnet werden kann
      mutate(across(starts_with("CF_"), ~ .x/BE)) %>%
      select(-BE) ->
      temp_pattern
    
    
    # Shift aus Inputdaten entfernen
    temp_names_pattern<-names(temp_pattern)
    
    temp_pattern %>% 
      select(-CF_1) %>% 
      cbind("CF_neu" = 0) %>% 
      setNames(temp_names_pattern) ->
      temp_pattern
    
    temp_pattern %>% 
      select(!starts_with("CF_")) %>% 
      group_by(across(names(.)[which(names(.)!=names(C_Kennzeichen)[.spalte_kennzeichen_anfall_jahr])])) %>% 
      summarise(across(names(.)[which(names(.)==names(C_Kennzeichen)[.spalte_kennzeichen_anfall_jahr])], ~ max(.x, na.rm = TRUE), .names = "max_aj")) %>% 
      ungroup %>% 
      left_join(temp_pattern) %>% 
      filter(if_all(names(.)[which(names(.)==names(C_Kennzeichen)[.spalte_kennzeichen_anfall_jahr])], ~ . == max_aj)) %>% 
      select(-c(max_aj,anfall_jahr)) ->
      pattern
    
    #' Standardpattern berechnen
    
    #' Wir werden fak und obl, sowie prop und nprop Vertraege zusammen geworfen, dies ist kein Problem, da in der Planung nur eine Jahressicht exsititert und keine Sicht pro Monat 
    #' --> nur relevant wie viel im ersten Jahr ausgezahlt wird und wie viel nicht und nur relevant für Diskontierung
    
    C_Kennzeichen %>% 
      filter(lic_methode!="Spiegelung"& !(Kennzeichen %in% c("Rente","RfB"))) %>%
      mutate(across(where(is.numeric) & starts_with(c("BE","CF_")), ~ case_when(gesch_art == 30 ~ -.x,
                                                                                TRUE ~ .x))) %>% 
      group_by(across(names(.)[c(.spalte_kennzeichen_anfall_jahr,
                                 .spalte_kennzeichen_long_short)])) %>% 
      summarise(across(names(.)[c(.spalte_kennzeichen_BE)] | starts_with("CF_"), ~ sum(.x, na.rm = TRUE))) %>% 
      ungroup %>% 
      filter(if_any(starts_with("BE") | starts_with("CF_"), ~ . != 0)) %>%    # nur behalten, falls Pattern auch berechnet werden kann
      mutate(across(starts_with("CF_"), ~ .x/BE)) %>%  # Keine Spiegelung der Pattern notwendig
      select(-BE) ->
      temp_pattern
    
    # Shift aus Inputdaten entfernen
    temp_names_pattern<-names(temp_pattern)
    
    temp_pattern %>% 
      select(-CF_1) %>% 
      cbind("CF_neu" = 0) %>% 
      setNames(temp_names_pattern) ->
      temp_pattern
    
    temp_pattern %>% 
      select(!starts_with("CF_")) %>% 
      group_by(across(names(.)[which(names(.)!=names(C_Kennzeichen)[.spalte_kennzeichen_anfall_jahr])])) %>% 
      summarise(across(names(.)[which(names(.)==names(C_Kennzeichen)[.spalte_kennzeichen_anfall_jahr])], ~ max(.x, na.rm = TRUE), .names = "max_aj")) %>% 
      ungroup %>% 
      left_join(temp_pattern) %>% 
      filter(if_all(names(.)[which(names(.)==names(C_Kennzeichen)[.spalte_kennzeichen_anfall_jahr])], ~ . == max_aj)) %>% 
      select(-c(max_aj,anfall_jahr)) ->
      pattern_standard
    
    rm(temp_pattern, temp_names_pattern)
    
    
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # LIC: Cashflow Schadenzahlung GJ erzeugen ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    print("LIC: Cashflow Schadenzahlung GJ erzeugen")
    
    plandaten %>% 
      left_join(mapping_int_ext) %>% 
      mutate(int_ext_korr = coalesce(int_ext_korr,"ext")) %>% 
      filter(!(gesch_art==30 & int_ext_korr!="ext")) %>% # verarbeite keine internen Uebnerhamen, diese werden gespiegelt
      mutate(longtail_shorttail= coalesce(longtail_shorttail, "Shorttail")) %>% # Falls keine Segmentierung zugewiesen werden konnte verwendet Shorttail-Standardpattern
      left_join(skalierung) -> 
      temp_plandaten_cf
    
    #' Standard pattern zuweisen
    
    temp_plandaten_cf  %>% #filter(gsch_bil=="001" & basis_pf=="NatCat" & gsch_par=="040") %>%  
      filter(is.na(skalierung) | skalierung==0) %>% 
      select(names(plandaten)) %>% 
      left_join(skalierung_standard) %>% 
      left_join(mapping_int_ext) %>% 
      mutate(int_ext_korr = coalesce(int_ext_korr,"ext")) %>%
      select(names(temp_plandaten_cf)) %>% 
      rbind(filter(temp_plandaten_cf,!(is.na(skalierung) | skalierung==0)))  ->
      temp_plandaten_cf
    
    temp_plandaten_cf %>% 
      mutate(Ultimate = wert/skalierung*anteil) %>% 
      group_by(datei_name,sheet_name,gesch_art,gsch_bil,prop_nonprop,fak_obl,basis_pf,gsch_par,kges,longtail_shorttail,int_ext_korr,plan_jahr) %>% 
      summarise(wert = sum(wert,na.rm = TRUE),
                Ultimate = sum(Ultimate, na.rm = TRUE)) %>% 
      ungroup ->
      Validierung
    
    
    Validierung %>% 
      left_join(faktor_ra) %>% #filter(gsch_bil=="001" & basis_pf=="NatCat" & gsch_par=="040") %>% 
      left_join(faktor_ra_standard) %>% 
      mutate(faktor_ra_verwendet = coalesce(faktor_ra,faktor_ra_standard)) -> 
      Validierung
    
    
    Validierung %>% 
      select(gesch_art,gsch_bil, prop_nonprop,fak_obl,basis_pf,gsch_par,faktor_ra,faktor_ra_standard,faktor_ra_verwendet) %>% 
      distinct ->
      Validierung_ra
    
    #' Pattern hinzufuegen
    
    Validierung %>% 
      select(-starts_with("faktor_ra")) %>% 
      #select(-longtail_shorttail) %>% 
      left_join(pattern) -> 
      Validierung
    
    #' #' Standardpattern hinzufuegen
    
    
    Validierung %>% 
      filter(is.na(CF_1)) %>% 
      mutate(longtail_shorttail= coalesce(longtail_shorttail,"Shorttail")) %>% 
      select(!starts_with("CF_")) %>% 
      left_join(pattern_standard) %>%
      select(names(Validierung)) ->
      temp_Validierung
    
    Validierung %>% 
      filter(!is.na(CF_1)) %>% 
      rbind(temp_Validierung) %>% 
      mutate(ag_ueg = case_when(gesch_art==30 ~"ueg",
                                TRUE ~"ag")) ->
      Validierung
    
    rm(temp_Validierung)
    
    
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Validierung LIC Daten ####
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    print("Validierung LIC Daten")
    
    wb<-createWorkbook()
    
    addWorksheet(wb,"Validierung_CF",zoom = 80)
    addWorksheet(wb,"Validierung_RA",zoom = 80)
    addWorksheet(wb,"Validierung_RA_vorjahre", zoom = 80)
    
    writeData(wb,"Validierung_CF",Validierung)
    writeData(wb,"Validierung_RA",Validierung_ra)
    writeData(wb,"Validierung_RA_vorjahre",faktor_ra_vorjahre)
    
    
    NumStyle <- createStyle(halign = "right", numFmt = "#,##0")
    ProzStyle<- createStyle(halign = "right", numFmt = "0.00%")
    
    .spalten_numerisch<-which(names(Validierung) %in% c("wert","Ultimate"))
    .spalten_prozent<- which(startsWith(names(Validierung),"CF_"))
    
    for (.index_numerisch in .spalten_numerisch) {
      addStyle(wb,sheet = "Validierung_CF",style = NumStyle , rows = 2:(nrow(Validierung)+1),cols = .index_numerisch ) 
    }
    
    for (.index_prozent in .spalten_prozent) {
      addStyle(wb,sheet = "Validierung_CF",style = ProzStyle , rows = 2:(nrow(Validierung)+1),cols = .index_prozent ) 
    }
    
    
    .spalten_prozent<- which(startsWith(names(Validierung_ra),"faktor_ra"))
    
    
    for (.index_prozent in .spalten_prozent) {
      addStyle(wb,sheet = "Validierung_RA",style = ProzStyle , rows = 2:(nrow(Validierung_ra)+1),cols = .index_prozent ) 
    }
    
    .spalten_prozent<- which(startsWith(names(faktor_ra_vorjahre),"faktor_ra"))
    
    
    for (.index_prozent in .spalten_prozent) {
      addStyle(wb,sheet = "Validierung_RA_vorjahre",style = ProzStyle , rows = 2:(nrow(faktor_ra_vorjahre)+1),cols = .index_prozent ) 
    }
    
    
    saveWorkbook(wb,paste(filename,"Output","Validierung_Parameter.xlsx", sep = "/"), overwrite = TRUE)
    
    
    
    
    
}
    
    
    
    
    
    