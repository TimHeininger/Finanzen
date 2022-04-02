# Alle Inputs sollen eingelesen werden 

inputs_einlesen <- function(auswahl){


# Einlesen der Excel-Datei C_Kennzeichen.xlsx  ##################
if("C_Kennzeichen" %in% auswahl){
    # m54_input_srs_modell_output ----
    m54_input_srs_modell_output <<- read.xlsx(.pfad_c_kennzeichen,startRow = 1)
}
  
  
# Einlesen der Excel-Datei Input_allgemeine_Daten.xlsx #######################

if("Input_allgemeine_Daten" %in% auswahl){
    # m03_input_sparte_auf_s2segment ----
    m03_input_sparte_auf_s2segment <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M03 Mapping PRS", startRow = 2, cols = c(1:5))
    
    # m03_input_schluessel_zuordnung ----
    m03_input_schluessel_zuordnung <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M03 Mapping PRS", startRow = 2, cols = c(7:9))
    
    # Laden der Tabelle "m03_input_sparte_auf_s2segment" aus dem Tabellenblatt "M03 Mapping PRS" 
    
    .mapping_prs <- read.xlsx(.pfad_input_allgemeine_daten, sheet="M03 Mapping PRS",startRow=2, cols = c(1:5))   %>% as.data.frame()
    .mtab_prop_segment_nr_name <<- distinct(.mapping_prs[,2:3])
    
    
    # m07_input_akt_saufw ----
    m07_input_akt_saufw <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M07 Input erwartete Zession", startRow = 2, cols = c(1,2))
    
    # m11_input_ausfall_wkeit ----
    m11_input_ausfall_wkeit <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M11 Input Ausfallw-keit", startRow = 2, cols = c(1:2))
    
    # m11_input_ausfallrate ----
    m11_input_ausfallrate <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M11 Input Ausfallw-keit", startRow = 2, cols = c(4))
    
    # m16_input_filter_retro_schluessel ----
    m16_input_filter_retro_schluessel <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M16 Input Retro Schluessel", startRow = 2)
    
    # m33_input_ausschluss_uebernahme ----
    m33_input_ausschluss_uebernahme <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M33 Input Ausschluss Uebernahme", startRow = 2)
    
    # m35_input_datenbasis_prs ----
    m35_input_datenbasis_prs <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M35 Input Filter Tabellen", startRow = 2, cols = c(1:3))
    
    # m35_input_datenbasis_ps ----
    m35_input_datenbasis_ps <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M35 Input Filter Tabellen", startRow = 2, cols = c(5:8))
    
    # m35_input_datenbasis_fp_future ----
    m35_input_datenbasis_fp_future <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M35 Input Filter Tabellen", startRow = 2, cols = c(10:13))
    
    # m58_input_bta_zuordnung ----
    m58_input_bta_zuordnung <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M58 Input bta Zuordnung", startRow = 2)
    
    # m63_input_inflationsrate ----
    m63_input_inflationsrate <<- read.xlsx(.pfad_input_allgemeine_daten, sheet = "M63 Input Inflationsrate", startRow = 2)
    
}

# Einlesen der Excel-Datei Input_Controlling #######################

if("Input_Controlling" %in% auswahl){
    # m05_input_piv ----
    if(substring(stichtag_aktuell,4,5)!="12")
      m05_input_piv <<- read.xlsx(.pfad_input_controlling, sheet = "M05 Input PIV", startRow = 2)
    
    if(substring(stichtag_aktuell,4,5)=="12")
      m05_input_piv <<- matrix(nrow=0,ncol=1) %>% as.data.frame()
      
    # m08_input_piv_vq ----
    if(substring(stichtag_aktuell,4,5)!="12")
      m08_input_piv_vq <<- read.xlsx(.pfad_input_controlling, sheet = "M08 Input PIV VQ", startRow = 2)
    
    if(substring(stichtag_aktuell,4,5)=="12")
      m08_input_piv_vq <<- matrix(nrow=0,ncol=1) %>% as.data.frame()
    
 
    # m36_input_planung ----
    m36_input_planung <<- read.xlsx(.pfad_input_controlling, sheet = "M36 Input Planung", startRow = 2)
}

# Einlesen der Excel-Datei Input_DSS ###############################

if("Input_DSS" %in% auswahl){
    # m04_input_gesellschaft_nr ----
    m04_input_gesellschaft_nr <<- read.xlsx(.pfad_input_dss, sheet = "M04 Input Gesellschaftsnummer", startRow = 2)
    
    # m12_input_abg_istbuchungen ----
    if(substring(stichtag_aktuell,4,5)!="12")
    m12_input_abg_istbuchungen <<- read.xlsx(.pfad_input_dss, sheet = "M12 Input IST Buchungen", startRow = 2, cols = c(1:15))
    
    if(substring(stichtag_aktuell,4,5)=="12")
      m12_input_abg_istbuchungen <<- matrix(nrow=0,ncol=1) %>% as.data.frame()
    
    # m12_input_ueb_istbuchungen ----
    if(substring(stichtag_aktuell,4,5)!="12")
     m12_input_ueb_istbuchungen <<- read.xlsx(.pfad_input_dss, sheet = "M12 Input IST Buchungen", startRow = 2, cols = c(18:33))
    
    if(substring(stichtag_aktuell,4,5)=="12")
      m12_input_ueb_istbuchungen <<- matrix(nrow=0,ncol=1) %>% as.data.frame()
    
    # m13_input_rfb_abgabe ----
    m13_input_rfb_abgabe <<- read.xlsx(.pfad_input_dss, sheet = "M13 Input RfB", startRow = 2, cols = c(1:11))
    
    # m13_input_rfb_uebernahme ----
    m13_input_rfb_uebernahme <<- read.xlsx(.pfad_input_dss, sheet = "M13 Input RfB", startRow = 2, cols = c(13:23))
    
    # m15_input_verw_kosten ----
    m15_input_verw_kosten <<- read.xlsx(.pfad_input_dss, sheet = "M15 Input VWK", startRow = 2, cols = c(1:2))
    
    # m15_input_verd_pr_vwk ----
    m15_input_verd_pr_vwk <<- read.xlsx(.pfad_input_dss, sheet = "M15 Input VWK", startRow = 2, cols = c(5:15))
    
    # m32_input_firma_ges ----
    m32_input_firma_ges  <<- read.xlsx(.pfad_input_dss, sheet = "M32 Input Mapping firma_ges", startRow = 2)
    
    # m42_input_verd_praemie_abg ----
    m42_input_verd_praemie_abg <<- read.xlsx(.pfad_input_dss, sheet = "M42 Verd Praemie letzter 4Q", startRow = 2, cols = c(1:13))
    
    # m42_input_verd_praemie_ueb ----
    m42_input_verd_praemie_ueb <<- read.xlsx(.pfad_input_dss, sheet = "M42 Verd Praemie letzter 4Q", startRow = 2, cols = c(17:28))
    
    # m51_input_lob_pr_risiko ----
    m51_input_lob_pr_risiko <<- read.xlsx(.pfad_input_dss, sheet = "M51 Input LoB PrRisiko", startRow = 2)
    
    # m59_input_geb_praemie_abg ---
    m59_input_geb_praemie_abg <<- read.xlsx(.pfad_input_dss, sheet = "M59 Input Gebuchte HGB Praemie", startRow = 2, cols = c(1:11))
    
    # m59_input_geb_praemie_ueb ---
    m59_input_geb_praemie_ueb <<- read.xlsx(.pfad_input_dss, sheet = "M59 Input Gebuchte HGB Praemie", startRow = 2, cols = c(13:23))
}

# Einlesen der Excel-Datei Input_FuV ###################################

if("Input_FuV" %in% auswahl){
    # m14_input_fuv_abgabe ----
      m14_input_fuv_abgabe <<- read.xlsx(.pfad_input_fuv, sheet = "Abgabe", startRow = 1, cols = c(1:19)) %>%
                                  mutate(Vertragsbezeichnung = as.character(NA))
    
    # m14_input_fuv_uebernahme ----
       m14_input_fuv_uebernahme <<- read.xlsx(.pfad_input_fuv, sheet = "Übernahme", startRow = 1, cols = c(1:19)) %>%
         mutate(Vertragsbezeichnung = as.character(NA))
    
}


# Einlesen der Excel-Datei Input_Rating ###############################

if("Input_Rating" %in% auswahl)  {
# m02_input_ratings ----
m02_input_ratings <<- read.xlsx(.pfad_input_rating, sheet= "M02 Input Ratings", startRow = 1, cols=c(1:10))
}

# Einlesen der Excel-Datei Input_RV_Spiegel ###############################

if("Input_RV_Spiegel" %in% auswahl){
    # m06_input_vtrgzuordnung_rvspiegel ----
    m06_input_vtrgzuordnung_rvspiegel <<- read.xlsx(.pfad_input_rv_spiegel, "M06 Input RV-Spiegel", startRow = 2, cols = c(1:7))
    
    # m06_input_vtrgzuordnung_plandaten ----
    m06_input_vtrgzuordnung_plandaten <<- read.xlsx(.pfad_input_rv_spiegel, "M06 Input RV-Spiegel", startRow = 2, cols = c(9:14))
    
    # m06_input_ext_zes_plandaten ----
    m06_input_ext_zes_plandaten <<- read.xlsx(.pfad_input_rv_spiegel, "M06 Input RV-Spiegel", startRow = 2, cols = c(16:18))
    
    # m06_input_praemie_xl_vtgr ----
    m06_input_praemie_xl_vtgr <<- read.xlsx(.pfad_input_rv_spiegel, "M06 Input RV-Spiegel", startRow = 2, cols = c(20:23))
    
    # m06_input_zes_fak_vtrg ----
    m06_input_zes_fak_vtrg <<- read.xlsx(.pfad_input_rv_spiegel, "M06 Input RV-Spiegel", startRow = 2, cols = c(25:27))
    
    # m06_input_ext_gsch_par_ausnahmen ----
    m06_input_ext_gsch_par_ausnahmen <<- read.xlsx(.pfad_input_rv_spiegel, "M06 Input RV-Spiegel", startRow = 2, cols = c(29:31))
}

# Einlesen der Excel-Datei Input_ZAK ###################################

if("Input_ZAK" %in% auswahl){
    # m09_input_zak_bs_mit_ibnr ----
    m09_input_zak_bs_mit_ibnr <<- read.xlsx(.pfad_input_zak, sheet = "M09 Input Schnittstelle ZAK", startRow = 2, cols = c(1:2))
    
    # m09_input_zak_ngq ----
    m09_input_zak_ngq <<- read.xlsx(.pfad_input_zak, sheet = "M09 Input Schnittstelle ZAK", startRow = 2, cols = c(4:5))
    
    # m09_input_zak_zahlungsmuster ----
    m09_input_zak_zahlungsmuster <<- read.xlsx(.pfad_input_zak, sheet = "M09 Input Schnittstelle ZAK", startRow = 2, cols = c(7:57))
    
    # m47_input_risikomind ----
    m47_input_risikomind <<- read.xlsx(.pfad_input_zak, sheet = "M47 Input Risikominderung", startRow = 2)
}

# Einlesen der Excel-Datei Input_ZSK #######################################################

if( "Input_ZSK" %in% auswahl)
# m45_input_zsk ----
m45_input_zsk <<- read.xlsx(.pfad_input_zsk, sheet = "InputCFmodell", startRow = 1)

}


