III_Planung_LRC<-function(filename,
                         stichtag_daten,
                         stichtag_planung,
                         schema,
                         name_cockpit_planung,
                         pfad_R_lib,
                         Pversion,
                         faktor_LRC,
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
  

  
  .GJ<-year(stichtag_planung)
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Parameter aus Exceldatei laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Parameter aus Exceldatei laden")
  
  .input_parameter <- read_excel(paste(filename,name_cockpit_planung, sep = "/"), sheet = "Parameter_R_Code")
  .input_parameter <- .input_parameter[which(.input_parameter$Berechnungsschritt == "LRC"),]
  
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
  
  print("Input laden")
  
  read_excel(paste(filename,name_cockpit_planung, sep = "/"), sheet = .sheet_plandaten) %>% 
    filter(if_all(names(.)[ncol(.)], ~ .x!=0)) %>% 
    mutate(kges = recode(kges, ext = as.character(NA))) ->
    input_plandaten
  
  
  read_excel(paste(filename,name_cockpit_planung, sep = "/"), sheet = .sheet_bafin_sparten) %>% 
    # rename(gsch_bil = `bil. Gesellschaft`,
    #        gsch_par = Gegenpartei) %>% 
    filter(IFRS_17!="NICHT_RELEVANT" & !is.na(IFRS_17)) %>% 
    select(basis_pf,bav_zweig_nr,IFRS_17,gsch_bil,gsch_par,ag_ueg) %>% 
    distinct ->
    input_bafinsparten
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # ALL: DSS Daten laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("ALL: DSS Daten laden")
  
  #' mapping int_ext
  
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
  
  
  #' ziaportfoid
  
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
  
  #' Premiums berechnen 
  
  ("select 
lic.gesch_art,
lic.gsch_bil,
CASE WHEN lic.fak_obl='fak' THEN 'prop' ELSE lic.prop_nonprop END as prop_nonprop,
lic.fak_obl,
CASE WHEN int_ext.ifrs17_int_ext = 'int' THEN lic.gsch_par ELSE 'ext' END as gsch_par,
CASE WHEN lic.gesch_art = 30 THEN 'ueg' ELSE 'ag' END as ag_ueg,
--lic.kges,
--rv_zweig.bav_zweig_nr,
lic.basis_pf,
lic.anfall_jahr,
-sum(lic.betrag) as geb_beitrag

from schema_abfrage.s_rg_input_lic as lic
left Join daisy.s_rg_d010_rv_zweig_rais as rv_zweig on(rv_zweig.rv_zweig_rais = lic.rv_zweig_rais and rv_zweig.guelt_ab <= CAST('stichtag_abfrage' as date) and rv_zweig.guelt_bis > CAST('stichtag_abfrage' as date))
left join daisy.r_rg_stada_firma_ges_int_ext as int_ext on (int_ext.gesellschaft = lic.gsch_par)

where lic.stichtag = CAST('stichtag_abfrage' as date) and resq_total = 'Total Premiums' and lic.anfall_jahr = lic.abr_jahr and (gesch_art<>30 or (gesch_art=30 and int_ext='ext'))

Group by 1,2,3,4,5,6,7,8
order by 1,2,3,4,5,6,8,9,7") %>% 
    str_replace_all("stichtag_abfrage", as.character(stichtag_daten)) %>%
    str_replace_all("schema_abfrage",schema) %>% 
    dbGetQuery(con, . ) %>% 
    left_join(input_bafinsparten) %>% 
    select(-bav_zweig_nr) %>% 
    rename(bafin_sparte = IFRS_17) %>% 
    group_by(across(!matches("geb_beitrag"))) %>% 
    summarise(across(matches("geb_beitrag"), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup %>% 
    filter(bafin_sparte!="NICHT_RELEVANT") -> 
    geb_beitrag
  
  
  
  #' Beitragsuebertraege 
  
  
  ("Select 
CASE 				
WHEN vertrag.vtg_uebern_art_mt ~'bernahmen' THEN 30 				
WHEN SUBSTR(buchung.land_geo_di,7,1) = 'D' THEN 20 				
ELSE 40 END AS gesch_art,
--vertrag.rv_vtg_nr,
vertrag.fak_obl_kt,
vertrag.prop_nonprop_kt,
a.gesellschaft as gsch_bil,
CASE WHEN b.ifrs17_int_ext='int' THEN buchung.gesellschaft ELSE 'ext' END as gsch_par,
--buchung.gesellschaft as gsch_par,
'Liability for Incurred Claims' as GuV_pos,
--buchung.rv_zweig_rais,
mega.basis_pf,
sum(buchung.btg) as wrt_bue

 from daisy.s_rg_d011_rv_vertrag as vertrag
left join daisy.s_rg_d009_abgabe_btg as buchung on (buchung.rv_vtg_nr = vertrag.rv_vtg_nr and buchung.firma = vertrag.firma and buchung.guelt_ab<=CAST('stichtag_abfrage' as date) 
and buchung.guelt_bis > CAST('stichtag_abfrage' as date))
left join daisy.r_rg_stada_firma_ges_int_ext as a on (a.firma = vertrag.firma)
left join daisy.r_rg_stada_betragsarten as betragsarten on (betragsarten.bta = buchung.bta)
left join daisy.r_rg_stada_firma_ges_int_ext as b on (buchung.gesellschaft = b.gesellschaft) -- Unterscheidung int_ext
left join schema_abfrage.s_rg_seg_ifrs_mega_mapping as mega on (mega.rv_vtg_nr = vertrag.rv_vtg_nr 
                                                             and a.gesellschaft = mega.gsch_bil 
                                                             and buchung.gesellschaft = mega.gsch_par 
                                                             and mega.rv_zweig_rais = buchung.rv_zweig_rais 
                                                             and CAST(mega.kohorte as text) = buchung.anfall_jahr 
                                                             and mega.stichtag = CAST('stichtag_abfrage' as date))

where vertrag.guelt_ab <= CAST('stichtag_abfrage' as date) and vertrag.guelt_bis > CAST('stichtag_abfrage' as date) and buchung.bta in('0200','0205','0210','0215','0250','0255','0270') and buchung.bilanz_jahr = 'GJ_abfrage'  


group by 1,2,3,4,5,6,7

having sum(buchung.btg)<>0

UNION

Select 
CASE 				
WHEN vertrag.vtg_uebern_art_mt ~'bernahmen' THEN 30 				
WHEN SUBSTR(buchung.land_geo_di,7,1) = 'D' THEN 20 				
ELSE 40 END AS gesch_art,
--vertrag.rv_vtg_nr,
vertrag.fak_obl_kt,
vertrag.prop_nonprop_kt,
a.gesellschaft as gsch_bil,
CASE WHEN b.ifrs17_int_ext='int' THEN buchung.gesellschaft ELSE 'ext' END as gsch_par,
--buchung.gesellschaft as gsch_par,
'Liability for Incurred Claims' as GuV_pos,
--buchung.rv_zweig_rais,
mega.basis_pf,
-sum(buchung.btg) as wrt_bue

 from daisy.s_rg_d011_rv_vertrag as vertrag
left join daisy.s_rg_d015_uebernahme_btg as buchung on (buchung.rv_vtg_nr = vertrag.rv_vtg_nr and buchung.firma = vertrag.firma and buchung.guelt_ab<=CAST('stichtag_abfrage' as date) 
and buchung.guelt_bis > CAST('stichtag_abfrage' as date))
left join daisy.r_rg_stada_firma_ges_int_ext as a on (a.firma = vertrag.firma) -- Nummer 3 stellig machen
left join daisy.r_rg_stada_betragsarten as betragsarten on (betragsarten.bta = buchung.bta)
left join daisy.r_rg_stada_firma_ges_int_ext as b on (buchung.gesellschaft = b.gesellschaft) -- Unterscheidung int_ext
left join schema_abfrage.s_rg_seg_ifrs_mega_mapping as mega on (mega.rv_vtg_nr = vertrag.rv_vtg_nr 
                                                             and a.gesellschaft = mega.gsch_bil 
                                                             and buchung.gesellschaft = mega.gsch_par 
                                                             and mega.rv_zweig_rais = buchung.rv_zweig_rais 
                                                             and CAST(mega.kohorte as text) = buchung.anfall_jahr 
                                                             and mega.stichtag = CAST('stichtag_abfrage' as date))

where vertrag.guelt_ab <= CAST('stichtag_abfrage' as date) and vertrag.guelt_bis > CAST('stichtag_abfrage' as date) and buchung.bta in('0200','0205','0210','0215','0250','0255','0270') and buchung.bilanz_jahr = 'GJ_abfrage'  


group by 1,2,3,4,5,6,7

having sum(buchung.btg)<>0

") %>% 
    str_replace_all("stichtag_abfrage", as.character(stichtag_daten)) %>%
    str_replace_all("schema_abfrage",schema) %>% 
    str_replace_all("GJ_abfrage", as.character((.GJ-1))) %>% 
    dbGetQuery(con, . ) %>%
    mutate(prop_nonprop_kt = case_when(prop_nonprop_kt=="Prop." ~ "prop",
                                       prop_nonprop_kt=="Non-Prop" ~ "nprop",
                                       TRUE ~ "NICHT_GEFUNDEN"),
           fak_obl_kt = case_when(fak_obl_kt=="Fak." ~ "fak",
                                  fak_obl_kt=="Obl."~ "obl",
                                  TRUE ~ "NICHT_GEFUNDEN"),
           ag_ueg = case_when(gesch_art == 30 ~"ueg",
                              TRUE ~ "ag")) %>% 
    rename(prop_nonprop = prop_nonprop_kt,
           fak_obl = fak_obl_kt,
           GuV_pos = guv_pos) ->
    beitragsuebertraege
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # LRC: Mapping fuer Schluesselung erstellen fuer Beitragsuebertraege (PAA) ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("LRC: Mapping fuer Schluesselung erstellen fuer Beitragsuebertraege (PAA)")
  
  #' Sicherstellen, dass alle GVVs existieren (insebesondere GVV die nur im Mega Mapping exsistieren, aber keine IST-Reserve haben, fehlen in der C_Kennzeichen Datei)
  
  input_plandaten %>% 
    select(-kges) %>% 
    anti_join(geb_beitrag) %>% 
    select(gesch_art,gsch_bil,gsch_par,bafin_sparte,fak_obl,prop_nonprop,plan_jahr) %>%
    rename(anfall_jahr = plan_jahr) %>% 
    mutate(ag_ueg = case_when(gesch_art==30 ~ "ueg",
                              TRUE ~ "ag")) %>% 
    distinct %>% 
    rename(IFRS_17 = bafin_sparte) %>% 
    left_join(input_bafinsparten)  %>% 
    rename(bafin_sparte = IFRS_17) %>% 
    mutate(geb_beitrag=0) %>% 
    select(names(geb_beitrag)) %>% 
    rbind(geb_beitrag) %>% 
    group_by(across(names(.)[1:(ncol(.)-1)])) %>%
    summarise(across(names(.)[ncol(.)], ~ sum(.x, na.rm = TRUE))) %>%
    ungroup  ->
    temp_table
  
  temp_table %>% 
    select(-basis_pf) %>% 
    group_by(across(!matches("geb_beitrag"))) %>% 
    summarise(across(matches("geb_beitrag"), ~ sum(.x, na.rm = TRUE), .names = "Nenner"),
              across(matches("geb_beitrag"), ~ n(), .names = "anzahl")) %>% 
    ungroup  %>%
    left_join(temp_table) %>% 
    mutate(anteil = case_when(Nenner!= 0 ~ geb_beitrag/Nenner,
                              TRUE ~ 1/anzahl),
           quelle_anteil = case_when(Nenner!= 0 ~ "beitrag",
                                     TRUE ~ "gleichmaessig")) %>% 
    select(-c(Nenner,geb_beitrag,anzahl)) %>% 
    group_by(across(!matches(c("anteil","anfall_jahr","basis_pf")))) %>% 
    filter(anfall_jahr==max(anfall_jahr, na.rm = TRUE)) %>% 
    ungroup %>% 
    select(-anfall_jahr) -> 
    mapping_aufteilung_lrc
  
  rm(temp_table)
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # LRC: Betragsuebertraege (PAA) ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("LRC: Betragsuebertraege (PAA)")
  
  input_plandaten %>% 
    filter(gesch_art %in% c(20,40) | (gesch_art == 30 & gsch_par=="ext") & wert!=0)%>% # Filtere auf Abgabe und ext. Uebernahme
    mutate(betragsart = str_replace_all(betragsart," ","_")) %>% 
    filter(grepl("~*Beitrag",betragsart) | grepl("~*Provision",betragsart) ) %>% # Ziehe relevante Daten ab
    pivot_wider(names_from = betragsart, values_from = wert, values_fill = 0) %>% #write.xlsx(paste(filename,"Output","test.xlsx", sep = "/"), overwrite = TRUE)
    mutate(unearned_premiums = gebuchter_Beitrag -  verdienter_Beitrag,# beitragsuebertraege --> kummuliert entspricht dies der LRC
           earned_premiums = (gebuchter_Beitrag - Provision)*as.numeric(faktor_LRC), # Versicherungsumsatz/Verdienter Beitrag
           earned_premiums_2 = (gebuchter_Beitrag - Provision), # Erhaltene Beitrag
           earned_premiums_3 = -(gebuchter_Beitrag - Provision)) %>%  # Erwartete Beitraege
    filter(if_any(ends_with("premiums"), ~ .x!=0)) %>% 
    left_join(mapping_aufteilung_lrc)  %>% 
    rename(anfall_jahr = plan_jahr) %>% 
    pivot_longer(names_to = "keza_earned", values_to = "wrt", cols = contains("premiums")) %>% 
    left_join(ziaportfoid) %>% 
    # left_join(mapping_long_short) %>% 
    mutate(across(matches(c("gebuchter_Beitrag","verdienter_Beitrag","wrt")), ~ anteil*.x),
           fae_dat = ymd(paste(anfall_jahr+1,"-01-01", sep = "")),
           fg_typ = "LRC",
           bereich = "KH",
           sti_dat = stichtag_planung,
           kohorte = anfall_jahr,
           gruppe = 3,
           GJ = year(ymd(stichtag_planung)),
           Pversion = Pversion,
           bewertungsansatz = "PAA",
           kz_erwart = "EW")  %>% # pivot_wider(names_from = keza_earned, values_from = wrt) -> d_2
    mutate(anz_planjahre = 1) %>% # anz_planjahre 
    uncount(anz_planjahre, .id = "Pdat") %>% 
    mutate(Pdat = anfall_jahr,#(Pdat+.GJ-1),
           Pdat = ymd(paste(Pdat,"12-31", sep = "-")),
           anfall_dat = dmy(paste("01.01.",anfall_jahr, sep = "")),
           GuV_pos= case_when(keza_earned == "earned_premiums" ~ "Verdiente Beitraege",
                              keza_earned == "earned_premiums_2" ~ "Erhaltene Beitraege",
                              keza_earned == "earned_premiums_3" ~ "Erwartete Beitraege",
                              TRUE ~ "Liability for Remaining Coverage"),
           GuV_pos_nr= "Payments_GJ") ->
    plandaten_lrc
  
  
  
  plandaten_lrc %>%
    filter(is.na(basis_pf)) %>% 
    mutate(across(matches(c("verdienter_Beitrag","gebuchter_Beitrag","Provision")), ~ coalesce(.x,0))) %>% 
    select(datei_name,sheet_name,gesch_art,gsch_bil,prop_nonprop,fak_obl,gsch_par,kges,bafin_sparte,anfall_jahr,basis_pf, verdienter_Beitrag, gebuchter_Beitrag, Provision) %>% 
    distinct %>% 
    filter(!if_all(matches(c("verdienter_Beitrag","gebuchter_Beitrag","Provision")), ~ .x==0)) ->
    Fehler_Zuweisung_LRC
  
  
  #' Risikoanpassung hinzufuegen
  plandaten_lrc %>% 
    # filter(gesch_art==20 & prop_nonprop=="nprop" & fak_obl=="obl"& gsch_bil=="023" & basis_pf=="Feuer_FeuerSach") %>% 
    mutate(Kennzeichen ="Cashflow") -> 
    temp_cf
  
  
  #' Kummulierten Werte berechnen
  temp_cf %>%
    left_join(mapping_int_ext) %>% 
    mutate(int_ext_korr = coalesce(int_ext_korr,"ext"),
           kost_erloesart = case_when(gesch_art == 30 ~"Z20000",
                                      TRUE ~ "Z21000"),
           ag_ueg = case_when(gesch_art==30 ~ "ueg",
                              TRUE ~ "ag"),
           wert = wrt) %>% 
    #filter(gsch_bil=="001" & int_ext_korr=="ext" & sheet_name=="AH_04200"& grepl("~*Liabilit",GuV_pos)) %>% 
    # select(c(names(DSS_schadenzahlungen_gj),"ag_ueg")) %>% 
    group_by(across(!matches("wrt"))) %>% 
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    # select(c(names(DSS_schadenzahlungen_gj),"ag_ueg")) %>% 
    arrange_all ->
    temp_table
  
  
  temp_table %>%  
    cbind("test_spalte" = 1) %>% 
    group_by(across(!matches(c("wrt","anfall_jahr","anfall_dat","fae_dat","kohorte","Pdat","wert")))) %>% 
    summarise(wrt_kum = cumsum(wrt),
              anfall_jahr = cumsum(test_spalte)+.GJ-1) %>% 
    ungroup %>% 
    select(-test_spalte) %>% 
    mutate(Pdat = ymd(paste(anfall_jahr,"-12-31", sep = ""))) %>% 
    left_join(temp_table, . )  %>% #filter(gesch_art==20 & prop_nonprop=="nprop" & fak_obl=="obl"& gsch_bil=="023" & basis_pf=="Feuer_FeuerSach" & GuV_pos=="Liability for Incurred Claims") %>% 
    left_join(beitragsuebertraege) %>% 
    mutate(across(contains("wrt"), ~ coalesce(.x,0)),
           wrt_kum = wrt_kum+wrt_bue,
           across(ends_with("dat"), ~ case_when(nchar(month(.x))==1 & nchar(day(.x))==1 ~ paste(paste("0",day(.x), sep=""),paste("0",month(.x), sep = ""),year(.x), sep = "."),
                                                nchar(month(.x))==1 & nchar(day(.x))==2 ~ paste(day(.x),paste("0",month(.x), sep = ""),year(.x), sep = "."),
                                                nchar(month(.x))==2 & nchar(day(.x))==1 ~ paste(paste("0",day(.x), sep=""),month(.x),year(.x), sep = "."),
                                                TRUE  ~ paste(day(.x),month(.x), year(.x), sep = ".")))) %>% 
    rename(wrt_ink = wrt) %>% 
    select(-wrt_bue) %>% 
    pivot_longer(names_to = "ink_kum",values_to = "wrt", names_prefix = "wrt_", cols = starts_with("wrt_")) ->
    DSS_LRC
  
  
  
  rm(temp_table, temp_cf)
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # LRC: Spiegelung (PAA) ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  DSS_LRC %>% 
    filter(gesch_art %in% c(20,40) & int_ext_korr !="ext") %>% 
    rename(gsch_bil = gsch_par,
           gsch_par = gsch_bil) %>% 
    mutate(gesch_art = 30,
           kost_erloesart = case_when(gesch_art == 30 ~"Z20000",
                                      TRUE ~ "Z21000"),
           wrt = wrt) %>% 
    group_by(across(!matches("wrt"))) %>% 
    summarise(across(matches("wrt"), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup %>% 
    select(names(DSS_LRC)) %>% 
    rbind(filter(DSS_LRC,DSS_LRC$gesch_art!=30 | (DSS_LRC$gesch_art==30 & DSS_LRC$gsch_par=="ext"))) %>% 
    mutate(ag_ueg = case_when(gesch_art==30 ~ "ueg",
                              TRUE ~ "ag")) %>% 
    arrange_all %>% 
    left_join(mapping_int_ext) %>% 
    mutate(int_ext_korr = coalesce(int_ext_korr,"ext")) ->
    DSS_LRC
  
  
  
  #' filtere auf notwendige Daten fuer Output
  DSS_LRC %>% 
    filter((ink_kum=="ink" & str_detect(GuV_pos,"Beitraege")) | (ink_kum=="kum" & GuV_pos=="Liability for Remaining Coverage")) ->
    DSS_LRC
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # ALL: Zinsen/Output Generieren ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("ALL: Zinsen/Output Generieren")
  
  DSS_LRC %>% 
    group_by(across(!matches("wrt"))) %>% 
    summarise(across(matches("wrt"), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup ->
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
    #left_join(diskontkurve, by = c("Monat"="Monat","Pdat_vorher" = "stichtag_fortschreibung")) %>% 
    #rename(diskontfaktor_vorher = diskontfaktor) %>% # umbenennen der Plandaten damit zwei Zinskurven in die Tabelle eingefuegt werden koennen
    #left_join(diskontkurve, by = c("Monat"="Monat","Pdat" = "stichtag_fortschreibung")) %>% 
    # mutate(diskontfaktor = case_when(Monat<=0 ~ 1, 
    #                                  TRUE ~ diskontfaktor),
    #        diskontfaktor_vorher = case_when((year(fae_dat)-year(Pdat_vorher))*12<=0 ~ 1, # wenn hier Gleichheit erlaubt, dann wird aktuelles Jahr nicht diskontiert
    #                                         year(Pdat) == anfall_jahr ~ diskontfaktor, # Stellt sicher, dass im GJ keine Zinsaenderung gerechnet wird
    #                                         TRUE ~ diskontfaktor_vorher)) %>% 
    #select(-Pdat_vorher) %>%
    # pivot_longer(cols = which(names(.) %in% c("wrt","abs_ra")), names_to = "BE_RA", values_to = "nominal") %>% 
    # select(-c(Monat,faktor_ra)) %>% 
  mutate(BE_RA="BE",
         Kategorie = "nominal") %>% 
    # BE_RA = recode(BE_RA,
    #                "wrt" = "BE",
    #                .default = "RA"),
    # barwert = nominal*diskontfaktor,
    # zinsen = nominal-barwert,
    # int_ext = int_ext_korr,
    # zinsaenderung = case_when(fae_dat>=Pdat ~ nominal*(diskontfaktor-diskontfaktor_vorher),
    #                           TRUE ~ 0)) %>% 
    # zinsaenderung = nominal*(diskontfaktor-diskontfaktor_vorher)) %>% 
    # pivot_longer(cols = which(names(.) %in% c("nominal","barwert","zinsen","zinsaenderung")), names_to = "Kategorie",values_to = "wrt" ) %>% 
    mutate(GuV_pos = case_when(str_detect(GuV_pos,"Beitraege") ~ "Versicherungsumsatz", # LRC Thema
                               TRUE ~ GuV_pos)) %>% 
    # group_by(across(!matches(c("wrt","fae_dat")))) %>% # fae_dat wegaggregieren
    # summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    # ungroup %>% 
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
    group_by(gesch_art,prop_nonprop,fak_obl,gsch_bil,basis_pf,anfall_jahr,gsch_par,kges,int_ext_korr,ag_ueg,wert,datei_name,sheet_name,Pdat,anfall_dat,GuV_pos,GuV_pos_nr,ziaportfoid,fg_typ,bereich,sti_dat,kohorte,
             gruppe, GJ,Pversion,kost_erloesart,bewertungsansatz,kz_erwart,Pdat_vorher,BE_RA,Kategorie,daten_quelle) %>% 
    summarise(wrt = sum(wrt,na.rm = TRUE)) %>% 
    ungroup  -> # setze wrt-Spalte an die letzte Stelle
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
           GuV_pos_nr= "Veraenderung Auflösung VJ") %>% 
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
           GuV_pos_nr =case_when(GuV_pos_nr=="Payments_GJ" ~ "Veraenderung Auflösung GJ",
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
    Output_LRC
  
  rm(temp_output_validierung,temp_ra_spiegeln,output_validierung)
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # ALL: Output erstellen ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("ALL: Output erstellen")
  
  wb<-createWorkbook()
  
  addWorksheet(wb,"LRC", zoom = 80)
  
  writeData(wb,"LRC",Output_LRC)
  
  saveWorkbook(wb,paste(filename,"Output","LRC.xlsx", sep = "/"), overwrite = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}