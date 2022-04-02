VI_Steuerung<-function(liste_gesch_art,
                       liste_prop_nonprop,
                       liste_fak_obl,
                       liste_gsch_bil,
                       liste_gsch_par,
                       liste_BE_RA,
                       pfad_R_lib,
                       filename){
  
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
  # Input laden ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  data.table::fread(paste(filename,"Output","Planung_Hochrechnung_OPL.csv", sep = "/"), sep = ";", dec = ",") %>% 
    mutate(across(matches(c("gsch_bil","gsch_par")), ~ case_when(nchar(.x) == 1 ~ paste("00",.x, sep = ""),
                                                                 nchar(.x) == 2 ~ paste("0",.x, sep = ""),
                                                                 TRUE ~ as.character(.x)) )) %>% 
    rename(Bewertungsmodell = bewertungsansatz,
           Gesellschaft = gsch_bil) ->
    input_plandaten
  
  
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Filter auf relevante Daten ####
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
  input_plandaten %>% 
    filter(  gesch_art %in% liste_gesch_art &
               prop_nonprop %in% liste_prop_nonprop &
               fak_obl %in% liste_fak_obl &
               Gesellschaft %in% liste_gsch_bil &
               gsch_par %in% liste_gsch_par &
               BE_RA %in% liste_BE_RA &
               !is.na(GuV_Bilanz) & GuV_Bilanz!="") %>% 
    group_by(GuV_Bilanz,Kategorie,Buchhaltungsszenario) %>% 
    summarise(wrt = sum(wrt, na.rm = TRUE)) %>% 
    ungroup ->
    temp_table
  
 temp_table %>% 
    pivot_wider(names_from = Buchhaltungsszenario, values_from = wrt, values_fill = 0, values_fn = ~round(.x, digits = 2)) %>% 
    select(GuV_Bilanz, Kategorie,VJ,HR,IST,starts_with("PLAN")) -> 
    Bilanz
  
  
  rm(temp_table)
  
  return(Bilanz)
  
  
  
  
}























