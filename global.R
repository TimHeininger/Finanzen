
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************
# Globale Definitionen
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************


#****************************************************************************************************************************************************
# Pakete laden ----
#****************************************************************************************************************************************************
library(renv)  # Projekt-Umgebungen
library(shiny)
library(shinydashboard)
library(shinyalert) # Pop-Up-Nachrichten
library(shinyjs) # JavaScript Funktionen werden nutzbar gemacht
library(shinyFeedback) # Rückmeldungen bei Eingabefeldern
# library(data.table) # für fwrite und fread (Ergänzung zu Data-Frames)
library(rstudioapi)
library(magrittr) # Pipe-Operator
library(tidyr) # Pipe-Operator
library(dplyr) # Pipe-Operator
library(DT)
library(stringr) # String-Operationen
library(odbc) # für Kommunikation mit DSS
library(openxlsx) # für Excel-Export
library(shinyFiles)
library(DBI)
library(dbplyr)
library(readr) 
library(tidyverse)
library(lubridate)
library(kableExtra)
library(data.table)


#****************************************************************************************************************************************************
# Workdirectory setzen ----
#****************************************************************************************************************************************************

# .current_path <- rstudioapi::getSourceEditorContext()$path
# try(setwd(dirname(.current_path)))

.filename<<-"~/Documents/Finanzen/Abrechnungssystem"
.pfad_r_code<<-"~/Documents/GitHub/Finanzen"
.filename_daten<<- paste(.filename,"/Daten",sep = "")
.filename_module<<-paste(.pfad_r_code,"Module", sep = "/")

# buchungen<<-read.xlsx(paste(.filename_daten,"Buchungen.xlsx", sep = "/"), sheet = "Daten")
# fwrite(buchungen,file = paste(.filename_daten,"Buchungen.csv", sep = "/"), dec = ",", sep = ";")

fread(paste(.filename_daten,"Buchungen.csv", sep = "/"), dec = ",", sep = ";") %>% 
  mutate(Datum = as.Date(Datum, origin = "1899-12-30")) ->>
  buchungen


 
#****************************************************************************************************************************************************


#****************************************************************************************************************************************************
# Funktionen 
#****************************************************************************************************************************************************

source(paste(.filename_module,'function_suchen_buchungen_TH.R', sep = "/"), encoding = "utf8")

#****************************************************************************************************************************************************



#****************************************************************************************************************************************************
# globale Variablen ----



#****************************************************************************************************************************************************
# Einstellungen ändern ----
#****************************************************************************************************************************************************

# Wissenschaftliche Zahlendarstellung 1e+07 etc. deaktivieren
options(scipen = 999)

#****************************************************************************************************************************************************
