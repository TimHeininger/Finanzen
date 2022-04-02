IV_Bilanz<-function(filename,
                    name_cockpit_planung,
                    pfad_R_lib,
                    stichtag_planung,
                    Pversion){
  
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
  .input_parameter <- .input_parameter[which(.input_parameter$Berechnungsschritt == "Bilanz"),]
  
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
  
  
  read.xlsx(paste(filename,"Output",.name_LIC, sep = "/"), sheet = .sheet_LIC) ->
    LIC
  
  
  read.xlsx(paste(filename,"Output",.name_LRC, sep = "/"), sheet = .sheet_LRC) ->
    LRC
  
  # OPL<-read.xlsx(paste(filename,"OPL21","Lieferdatei","OPL_3.xlsx", sep = "/"), sheet = "cf")
  
  Bilanz_GuV_Vorlage<-read.xlsx(paste(filename,"Inputs","Bilanz_GuV_Vorlage.xlsx", sep = "/"), sheet = "Bilanz_GuV_Vorlage")
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # GuV und Bilanz erstellen ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  print("GuV und Bilanz erstellen")
  
  
  
  # OPL %>% 
  #   mutate(GuV_pos = str_replace_all(GuV_pos,"Veränderung","Veraenderung"),
  #          GuV_pos = str_replace_all(GuV_pos,"Liabilities","Liability")) %>%
  LIC %>%
    rbind(LRC) %>%
    mutate(Unterkategorie = case_when(grepl("~*Veraenderung",GuV_pos) & BE_RA=="BE" & fg_typ=="LIC" ~ "VA - Veraenderung LIC - Schaeden/Leistungen/Kosten",
                                      grepl("~*Veraenderung",GuV_pos) & BE_RA=="RA" & fg_typ=="LIC" ~ "VA - Veraenderung LIC - Risikoanpassung",
                                      grepl("~*Zahlungen",GuV_pos) & fg_typ =="LIC" ~ "VA - Zahlungen",
                                      grepl("~*Zinsertrag",GuV_pos) & fg_typ =="LIC" ~ "Zinsertrag (Zinsaenderung und Locked-in Zins)",
                                      grepl("~*Zinsaufwand",GuV_pos) & fg_typ =="LIC" ~ "Zinsaufwand (Zinsaenderung und Locked-in Zins)",
                                      GuV_pos=="Liability for Incurred Claims" & BE_RA=="BE" ~ "Barwert der Schaeden/Leistungen und Kosten",
                                      GuV_pos=="Liability for Incurred Claims" & BE_RA=="RA" ~ "Risikoanpassung",
                                      GuV_pos=="Liability for Remaining Coverage" ~ "Liability for Remaining Coverage",
                                      TRUE ~ GuV_pos)) %>% 
    mutate(Hauptkategorie = case_when(grepl("~*VA - Veraenderung LIC",Unterkategorie) ~ "Versicherungsserviceaufwand",
                                      grepl("~*VA - Zahlungen",Unterkategorie) & fg_typ == "LIC" ~ "Versicherungsserviceaufwand",
                                      grepl("~*Zins",Unterkategorie) & fg_typ=="LIC" ~ "Finanzergebnis",
                                      # GuV_pos=="Liability for Remaining Coverage" ~ "Liability for Remaining Coverage",
                                      GuV_pos=="Liability for Incurred Claims" ~ "Liability for Incurred Claims",
                                      TRUE ~ as.character(NA)),
           neues_altes_aj = case_when(anfall_jahr<.GJ ~"altes_AJ",
                                      TRUE ~ "neues_AJ")) %>% 
    select(-c(Kategorie,Pdat_vorher)) %>% 
    pivot_longer(names_to = "Spalte_test",values_to = "Kategorie", cols = ends_with("kategorie")) %>% 
    filter(!is.na(Kategorie)) %>%
    mutate(neues_altes_aj = case_when(Kategorie %in% c("Finanzergebnis","Versicherungsserviceaufwand","Versicherungsumsatz","Liability for Incurred Claims","Liability for Remaining Coverage") ~ "alles",
                                      TRUE ~ neues_altes_aj)) %>% 
    select(-Spalte_test) %>% 
    filter(wrt!=0) ->
    Datenbank_Planung
  
  #' verischerungstechnische Ergebnis
  
  Datenbank_Planung %>% 
    filter(Kategorie %in% c("Versicherungsumsatz","Versicherungsserviceaufwand")) %>% # eigentlich noch sonst. vt. Aufwand (Verwaltungskosten, Feuerschutzsteuer) --> aktuell = 0
    mutate(Kategorie="Versicherungstechnisches Ergebnis") %>% 
    group_by(across(names(.)[which(names(.)!="wrt")])) %>% 
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    select(names(Datenbank_Planung)) %>% 
    rbind(Datenbank_Planung) ->
    Datenbank_Planung
  
  
  #' Netto Finanzergebnis
  
  Datenbank_Planung %>% 
    filter(Kategorie %in% c("Finanzergebnis")) %>% # eigentlich noch Kapitalergebnis --> aktuell = 0
    mutate(Kategorie="Netto-Finanzergebnis (inkl. KA)") %>% 
    group_by(across(names(.)[which(names(.)!="wrt")])) %>% 
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    select(names(Datenbank_Planung)) %>% 
    rbind(Datenbank_Planung) ->
    Datenbank_Planung
  
  
  #' Ergebnis v. Steuern
  
  Datenbank_Planung %>% 
    filter(Kategorie %in% c("Netto-Finanzergebnis (inkl. KA)","Versicherungstechnisches Ergebnis")) %>% # eigentlich noch Kapitalergebnis --> aktuell = 0
    mutate(Kategorie="Ergebnis v. Steuern") %>% 
    group_by(across(names(.)[which(names(.)!="wrt")])) %>% 
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    select(names(Datenbank_Planung)) %>% 
    rbind(Datenbank_Planung) ->
    Datenbank_Planung
  
  #' versicherungstechnische Rueckstellungen
  
  Datenbank_Planung %>% 
    filter(Kategorie %in% c("Liability for Incurred Claims","Liability for Remaining Coverage")) %>% # eigentlich noch Kapitalergebnis --> aktuell = 0
    mutate(Kategorie="Versicherungstechnische Rueckstellungen") %>% 
    group_by(across(names(.)[which(names(.)!="wrt")])) %>% 
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    select(names(Datenbank_Planung)) %>% 
    rbind(Datenbank_Planung) ->
    Datenbank_Planung
  
  
  Datenbank_Planung %>% 
    left_join(Bilanz_GuV_Vorlage) %>% 
    mutate(Variable = case_when(GuV_pos == "Release of Incurred Claims" ~ "IFRS17_EBT", 
                                TRUE ~ Variable)) ->
    Datenbank_Planung
  
  
  Datenbank_Planung %>% 
    group_by(gesch_art,gsch_bil,gsch_par,Kategorie,Pdat,Nummer,GuV_Bilanz) %>% 
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup %>% 
    pivot_wider(names_from = Pdat, values_from = wrt, names_prefix = "Pdat_", names_sort = TRUE, values_fill = 0) %>% 
    group_by(gesch_art,gsch_bil,gsch_par,Kategorie,GuV_Bilanz,Nummer) %>%   
    summarise(across(starts_with("Pdat"), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup %>% 
    arrange(gesch_art,gsch_bil,gsch_par,Nummer) %>% 
    select(-Nummer)->
    Output_Planung
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Steuerung/Eingriff/Validierung ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # csv Dateien erstellen fuer PlaTo Tool ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Output Tabelle erstellen")
  
  
  
  Datenbank_Planung %>% 
    mutate(anfall_jahr = as.numeric(anfall_jahr),
           Buchhaltungsszenario = case_when(anfall_jahr<=.GJ-1 ~ "VJ",
                                            anfall_jahr==.GJ ~ "IST",
                                            anfall_jahr==.GJ+1 ~ "HR",
                                            anfall_jahr>.GJ ~ paste("PLAN",anfall_jahr-.GJ-1, sep = ""),
                                            TRUE ~ "NICHT_RELEVANT"),
           Planungsszenario = "Basis",
           Datenstand = .GJ,
           Geschaeftsart = case_when(gesch_art==20 ~ "aG",
                                     gesch_art==30 ~ "ueG",
                                     gesch_art==40 ~ "Retro"),
           ID_Spalte = paste(Buchhaltungsszenario,bewertungsansatz,sep = "_"),
           Jahr = case_when(Buchhaltungsszenario!="NICHT_RELEVANT" & Buchhaltungsszenario=="VJ" ~ .GJ-2,
                            Buchhaltungsszenario!="NICHT_RELEVANT" & Buchhaltungsszenario=="IST" ~ .GJ-1,
                            Buchhaltungsszenario!="NICHT_RELEVANT" & Buchhaltungsszenario=="HR" ~ as.numeric(.GJ),
                            Buchhaltungsszenario!="NICHT_RELEVANT" & startsWith(Buchhaltungsszenario,"PLAN") ~ anfall_jahr,
                            TRUE ~ as.numeric(NA)),
           across(matches(c("gsch_bil","gsch_par","kges"), ~ case_when(nchar(.x)==1 ~ paste("00",.x, sep = ""),
                                                                       nchar(.x)==2 ~ paste("0",.x, sep = ""),
                                                                       TRUE ~ as.character(.x))))) %>% 
    filter(!is.na(Jahr)) %>% 
    cbind("Details zu Bestandteilen" = as.character(NA),
          "Annahme(n) / Kommentierung" = as.character(NA)) ->
    Datenbank_Planung
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Output erstellen ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Output erstellen")
  
  wb<-createWorkbook()

  addWorksheet(wb,"Planung_Hochrechnung",zoom = 80)
  addWorksheet(wb,"Datenbank_Planung",zoom = 80)

  writeData(wb,"Planung_Hochrechnung",Output_Planung)
  writeData(wb,"Datenbank_Planung",Datenbank_Planung)


  NumStyle <- createStyle(halign = "right", numFmt = "#,##0")
  # DateStyle <- createStyle(halign = "right", numFmt = "m/d/yyyy")

  .spalten_numerisch<-which(startsWith(names(Output_Planung),"Pdat"))




  for (.index_numerisch in .spalten_numerisch) {
    addStyle(wb,sheet = "Planung_Hochrechnung",style = NumStyle , rows = 2:(nrow(Output_Planung)+1),cols = .index_numerisch )
  }


  .spalten_numerisch<-which(startsWith(names(Datenbank_Planung),"wrt"))


  for (.index_numerisch in .spalten_numerisch) {
    addStyle(wb,sheet = "Datenbank_Planung",style = NumStyle , rows = 2:(nrow(Datenbank_Planung)+1),cols = .index_numerisch )
  }


  saveWorkbook(wb,  paste(filename,"Output",paste("Planung_Hochrechnung_",Pversion,".xlsx", sep = ""), sep = "/"),overwrite = TRUE)
  
  
  
  fwrite(Datenbank_Planung, paste(filename,"Output",paste("Planung_Hochrechnung_",Pversion,".csv", sep = ""), sep = "/"), dec = ",", sep = ";")
  
  
  return(Datenbank_Planung)
  
  
}
  
  













