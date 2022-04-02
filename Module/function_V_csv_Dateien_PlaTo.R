V_Bilanz<-function(filename,
                         name_cockpit_planung,
                         pfad_R_lib,
                         name_vorlage,
                         sheet_vorlage){
  
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
  
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Parameter aus Exceldatei laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Parameter aus Exceldatei laden")
  
  .input_parameter <- read_excel(paste(filename,name_cockpit_planung, sep = "/"), sheet = "Parameter_R_Code")
  .input_parameter <- .input_parameter[which(.input_parameter$Berechnungsschritt == "csv_Dateien"),]
  
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
  
  # read_excel(paste(filename,name_cockpit_planung, sep = "/"), sheet = .sheet_output_planung) %>% 
  #   rename(Bewertungsmodell = bewertungsansatz,
  #          Gesellschaft = gsch_bil) ->
  #   input_plandaten
  
  data.table::fread(paste(filename,"Output","Planung_Hochrechnung_OPL.csv", sep = "/"), sep = ";", dec = ",") %>% 
    mutate(across(matches(c("gsch_bil","gsch_par")), ~ case_when(nchar(.x) == 1 ~ paste("00",.x, sep = ""),
                                                                 nchar(.x) == 2 ~ paste("0",.x, sep = ""),
                                                                 TRUE ~ as.character(.x)) )) %>% 
    rename(Bewertungsmodell = bewertungsansatz,
           Gesellschaft = gsch_bil) ->
    input_plandaten
  
  
  read_excel(paste(filename,"Inputs",name_vorlage, sep = "/"), sheet = sheet_vorlage) ->
    Vorlage_csv
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Output erstellen ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  print("Output erstellen")
  
  input_plandaten %>% 
    # filter(Geschaeftsart=="aG" & Gesellschaft=="001") %>% 
    mutate(PlaTo_datei = case_when(Geschaeftsart=="aG" ~ "aG_Datei",
                                   Geschaeftsart=="ueG" ~ "ueG_Datei",
                                   Geschaeftsart=="Retro" ~ "Retro_Datei")) %>% 
    group_by(Variable,Jahr,Buchhaltungsszenario,Planungsszenario,Bewertungsmodell,Gesellschaft,Datenstand,ID_Spalte,Geschaeftsart,`Details zu Bestandteilen`,`Annahme(n) / Kommentierung`) %>% 
    summarise(Wert = sum(wrt, na.rm = TRUE)) %>% 
    ungroup ->
    csv_daten
  
  
  #' Geschart 'Gesamt' hinzufuegen
  
  csv_daten %>% 
    mutate(Bewertungsmodell= as.character(NA),
           ID_Spalte = as.character(NA),
           Geschaeftsart = "Gesamt") %>% 
    group_by(across(names(.)[-ncol(.)])) %>% 
    summarise(Wert = sum(Wert, na.rm = TRUE)) %>% 
    ungroup %>% 
    rbind(csv_daten) ->
    csv_daten
  
  
  
  input_plandaten %>% 
    select(Geschaeftsart,Gesellschaft) %>% 
    distinct ->
    dateien
  
  
  #' Vorlage um Spalte Jahr erweitern
  
  
  csv_daten %>% 
    select(Jahr, Buchhaltungsszenario,Datenstand,Planungsszenario) %>% 
    distinct %>% 
    left_join(Vorlage_csv, . ) ->
    Vorlage_csv
  
  
  
  #' Vorlage erweitern um Gesellschaft
  
  csv_daten %>% 
    select(Gesellschaft) %>% 
    distinct %>% 
    unlist %>% 
    unname ->
    temp_gsch_bil
  
  Vorlage_csv %>% 
    mutate(anzahl_gsch_bil = length(temp_gsch_bil)) %>% 
    uncount(anzahl_gsch_bil) %>% 
    cbind("Gesellschaft" = rep(temp_gsch_bil, times = nrow(.)/length(temp_gsch_bil))) -> 
    Vorlage_csv
  
  rm(temp_gsch_bil)
  
  
  #' Erzeuge alle Variablen fuer PlaTo - Tool
  csv_daten %>% 
    left_join(Vorlage_csv, . ) ->
    csv_daten
  
  
  wb<-createWorkbook()
  
  dateien %>% 
    mutate(Worksheet_name = paste(Geschaeftsart,Gesellschaft, sep = "_")) %>% 
    select(Worksheet_name) %>% 
    distinct %>% 
    unlist %>% 
    unname ->
    .liste_worksheets
  
  
  csv_daten %>% 
    mutate(Wert = coalesce(Wert,0)) ->
    csv_daten
  
  
  sapply(c(1:nrow(dateien)), 
         function(i){
           
           name_output<-paste(paste(unname(unlist(dateien[i,1])),unname(unlist(dateien[i,2])), sep = "_"),".csv", sep = "")
           
           csv_daten %>% 
             filter(Geschaeftsart==unname(unlist(dateien[i,1])) & Gesellschaft==unname(unlist(dateien[i,2]))) %>% 
             select(Variable,Wert,Jahr,Buchhaltungsszenario,Planungsszenario,Bewertungsmodell,Gesellschaft,Datenstand,ID_Spalte,Geschäftsart,`Details zu Bestandteilen`,`Annahme(n) / Kommentierung`) ->
             # select(-c(PlaTo_datei,Geschaeftsart)) ->
             temp_data
           
           temp_data %>% 
             fwrite(paste(.filename,"Output","csv_Dateien",name_output, sep = "/"), sep = ",", dec = ".")
           
           addWorksheet(wb,.liste_worksheets[i], zoom = 80)
           writeData(wb,.liste_worksheets[i],temp_data)
           
           
           
           
         })
  
  
  saveWorkbook(wb,paste(.filename,"Output","csv_daten.xlsx", sep = "/"), overwrite = TRUE)




}





























