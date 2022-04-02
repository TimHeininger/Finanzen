# Kontrollen der einzelnen Inputs durchführen 

inputs_validieren <- function(auswahl_validierung){
  
  z <- 0
  
  # Kontrollen der Excel-Datei C_Kennzeichen.xlsx ################################
  
  if("C_Kennzeichen" %in% auswahl_validierung){
    
    z <- z+1
    
    ktr_m54_input_lic_modell_output_01_beschreibung <- "Pruefen, ob die Spalte prop_nonprop in Tabelle m54_input_srs_modell_output nur die Einträge nprop, prop und fak enthält."
    # ktr_m54_input_lic_modell_output_01 ----
    ktr_m54_input_lic_modell_output_01 <- all(m54_input_srs_modell_output[,"prop_nonprop"] %in% c("nprop", "prop", "fak"))
    
    ktr_c_kennzeichen_beschreibung <- "Kontrollvariable fuer die gesamte Excel-Datei"
    ktr_c_kennzeichen <- if_else(ktr_m54_input_lic_modell_output_01, TRUE, FALSE)
    
    
    kontrollen_c_kennzeichen <- matrix(c("","ktr_c_kennzeichen", ktr_c_kennzeichen_beschreibung, ktr_c_kennzeichen,
                                        "M54","ktr_m54_input_lic_modell_output_01",ktr_m54_input_lic_modell_output_01_beschreibung,ktr_m54_input_lic_modell_output_01), byrow = TRUE, ncol = 4) %>%
      as.data.frame() %>%
      setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    
    if(z==1) 
      ergebnistabelle <- kontrollen_c_kennzeichen
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
                            bind_rows(kontrollen_c_kennzeichen)
                        
    
  }
  
  # Kontrollen der Excel-Datei Input_allgemeine_Daten.xlsx #######################

  if("Input_allgemeine_Daten" %in% auswahl_validierung){
    
    z <- z+1
    
    ktr_m03_mapping_prs_01_beschreibung <- "Pruefen, ob die Spalte sparte in Tabelle m03_input_sparte_auf_s2segment identische Eintraege enthaelt"
    # ktr_m03_mapping_prs_01 ----
    ktr_m03_mapping_prs_01 <- if_else(nrow(m03_input_sparte_auf_s2segment) == length(m03_input_sparte_auf_s2segment[,"sparte"] %>% unique), TRUE, FALSE)
    
    ktr_m07_input_erwartete_zession_01_beschreibung <- "Pruefen, dass in der Tabelle m07_input_akt_saufw jede Vertragsbezeichnung nur einmal vorkommt"
    # ktr_m07_input_erwartete_zession_01 ----
    ktr_m07_input_erwartete_zession_01 <- if_else(nrow(m07_input_akt_saufw) == length(m07_input_akt_saufw[,c("vertragsbezeichnung")] %>% unique()), TRUE, FALSE )
    
    ktr_m11_input_ausfallw_keit_01_beschreibung <- "Pruefen, ob einem Rating anhand der Tabelle m11_input_ausfall_wkeit höchstens eine Ausfallwahrscheinlichkeit zugeordnet werden kann"
    # ktr_m11_input_ausfallw_keit_01 ----
    ktr_m11_input_ausfallw_keit_01 <- if_else(nrow(m11_input_ausfall_wkeit) ==
                                                        length(m11_input_ausfall_wkeit[,"rating"] %>% unique), TRUE, FALSE )
    
    
    ktr_m58_input_bta_zuordnung_01_beschreibung <- "Pruefen, dass in Tabelle m58_input_bta_zuordnung die Spalte bta_bezei keine identischen Einträge enthält"
    # ktr_m58_input_bta_zuordnung_01 ----
    ktr_m58_input_bta_zuordnung_01 <- if_else(nrow(m58_input_bta_zuordnung) == nrow(m58_input_bta_zuordnung %>% select(bta_bezei) %>% unique()), TRUE, FALSE)
    
    
    ktr_input_allgemeine_daten_beschreibung <- "Kontrollvariable fuer die gesamte Excel-Datei"
    ktr_input_allgemeine_daten <- if_else(ktr_m03_mapping_prs_01 & ktr_m07_input_erwartete_zession_01 & ktr_m11_input_ausfallw_keit_01 & ktr_m58_input_bta_zuordnung_01, TRUE, FALSE)
    
    kontrollen_input_allgemeine_daten <- matrix(c("","ktr_input_allgemeine_daten", ktr_input_allgemeine_daten_beschreibung, ktr_input_allgemeine_daten,
                                                  "M03","ktr_m03_mapping_prs_01",ktr_m03_mapping_prs_01_beschreibung,ktr_m03_mapping_prs_01,
                                                  "M07", "ktr_m07_input_erwartete_zession_01", ktr_m07_input_erwartete_zession_01_beschreibung, ktr_m07_input_erwartete_zession_01,
                                                  "M11", "ktr_m11_input_ausfallw_keit_01", ktr_m11_input_ausfallw_keit_01_beschreibung, ktr_m11_input_ausfallw_keit_01,
                                                  "M58", "ktr_m58_input_bta_zuordnung_01", ktr_m58_input_bta_zuordnung_01_beschreibung, ktr_m58_input_bta_zuordnung_01), byrow = TRUE, ncol = 4) %>%
                                           as.data.frame() %>%
                                           setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    
    if(z==1) 
      ergebnistabelle <- kontrollen_input_allgemeine_daten
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
      bind_rows(kontrollen_input_allgemeine_daten)
    
    
  }
  
  # Kontrollen der Excel-Datei Input_Controlling #######################
  
  if("Input_Controlling" %in% auswahl_validierung){
    
    z <- z+1
    
    ktr_m36_input_planung_01_beschreibung <- "Pruefen, dass die Tabelle m36_input_planung nicht leer ist"
    # ktr_m36_input_planung_01 ----
    ktr_m36_input_planung_01 <- if_else(nrow(m36_input_planung) == 0, FALSE, TRUE)
    
    ktr_m36_input_planung_02_beschreibung <- "Pruefen, dass die Spalte prop_nonprop der Tabelle m36_input_planung nur die Einträge nprop, prop und fak enthält."
    # ktr_m36_input_planung_02 ----
    ktr_m36_input_planung_02 <- all(m36_input_planung[,"prop_nonprop"] %in% c("nprop", "prop", "fak"))

    # m05_input_piv ----
    if(substring(stichtag_aktuell,4,5)!="12"){
      ktr_m05_input_piv_01_beschreibung <- "Pruefen, dass die Spalte prop_nonprop der Tabelle m05_input_piv nur die Einträge nprop, prop und fak enthält."
      # ktr_m05_input_piv_01 ----
      ktr_m05_input_piv_01 <- all(m05_input_piv[,"prop_nonprop"] %in% c("nprop", "prop", "fak"))
      
      ktr_m05_input_piv_02_beschreibung <- "Pruefen, dass das Feld betragsart der Tabelle m05_input_piv nur die Ausprägungen <GJ-Reserve>, <gebuchter Beitrag>, <GJ-Zahlung> und <Provision> besitzt."
      # ktr_m05_input_piv_02 ----
      ktr_m05_input_piv_02 <- all(m05_input_piv[,"betragsart"] %in% c("GJ-Reserve", "gebuchter Beitrag", "GJ-Zahlung", "Provision"))
    }
    
    # m08_input_piv_vq  ----
    if(substring(stichtag_aktuell,4,5)!="12"){
      ktr_m08_input_piv_vq_01_beschreibung <- "Pruefen, dass die Spalte prop_nonprop der Tabelle m08_input_piv_vq nur die Einträge nprop, prop und fak enthält."
      # ktr_m08_input_piv_vq_01 ----
      ktr_m08_input_piv_vq_01 <- all(m08_input_piv_vq[,"prop_nonprop"] %in% c("nprop", "prop", "fak"))
      
      ktr_m08_input_piv_vq_02_beschreibung <- "Pruefen, dass das Feld betragsart der Tabelle m08_input_piv_vq nur die Ausprägung <verdienter Beitrag> besitzt."
      # ktr_m08_input_piv_vq_02 ----
      ktr_m08_input_piv_vq_02 <- all(m08_input_piv_vq[,"betragsart"] %in% c("verdienter Beitrag"))
    }
    
    
    if(substring(stichtag_aktuell,4,5)!="12"){
    ktr_input_controlling_beschreibung <- "Kontrollvariable fuer die gesamte Excel-Datei"
    ktr_input_controlling <- if_else(ktr_m36_input_planung_01 & ktr_m36_input_planung_02 & ktr_m05_input_piv_01 & ktr_m08_input_piv_vq_01 , TRUE, FALSE)
    
    kontrollen_input_controlling <- matrix(c("","ktr_input_controlling",ktr_input_controlling_beschreibung,ktr_input_controlling,
                                             "M36","ktr_m36_input_planung_01",ktr_m36_input_planung_01_beschreibung,ktr_m36_input_planung_01,
                                             "M36","ktr_m36_input_planung_02",ktr_m36_input_planung_02_beschreibung,ktr_m36_input_planung_02,
                                             "M05","ktr_m05_input_piv_01",ktr_m05_input_piv_01_beschreibung,ktr_m05_input_piv_01,
                                             "M08","ktr_m08_input_piv_vq_01",ktr_m08_input_piv_vq_01_beschreibung,ktr_m08_input_piv_vq_01,
                                             "M08","ktr_m08_input_piv_vq_02",ktr_m08_input_piv_vq_02_beschreibung,ktr_m08_input_piv_vq_02), byrow = TRUE, ncol = 4) %>%
      as.data.frame() %>%
      setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    }
    
    if(substring(stichtag_aktuell,4,5)=="12"){
      ktr_input_controlling_beschreibung <- "Kontrollvariable fuer die gesamte Excel-Datei"
      ktr_input_controlling <- if_else(ktr_m36_input_planung_01 & ktr_m36_input_planung_02, TRUE, FALSE)
      
      kontrollen_input_controlling <- matrix(c("","ktr_input_controlling",ktr_input_controlling_beschreibung,ktr_input_controlling,
                                               "M36","ktr_m36_input_planung_01",ktr_m36_input_planung_01_beschreibung,ktr_m36_input_planung_01,
                                               "M36","ktr_m36_input_planung_02",ktr_m36_input_planung_02_beschreibung,ktr_m36_input_planung_02), byrow = TRUE, ncol = 4) %>%
        as.data.frame() %>%
        setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    }
    
    if(z==1) 
      ergebnistabelle <- kontrollen_input_controlling
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
      bind_rows(kontrollen_input_controlling)
    
    
  }
  
  # Kontrollen der Excel-Datei Input_DSS ###############################
  
  if("Input_DSS" %in% auswahl_validierung){
    
    z <<- z+1
    
    ktr_m04_input_gesellschaftsnummer_01_beschreibung <- "Pruefen, ob die Spalte gesellschaft in Tabelle m04_input_gesellschaft_nr identische Eintraege enthaelt"
    # ktr_m04_input_gesellschaftsnummer_01 ----
    ktr_m04_input_gesellschaftsnummer_01 <- if_else(nrow(m04_input_gesellschaft_nr) == length(m04_input_gesellschaft_nr[,"gesellschaft"] %>% unique), TRUE, FALSE)
    
    
    if(substring(stichtag_aktuell,1,6) != "31.12."){
      ktr_m12_input_ist_buchungen_01_beschreibung <- "Pruefen, ob in der Spalte betragsart der Tabellen m12_input_ueb_istbuchungen nur die Eintraege \"IST Prämien GJ\", \"IST Provisionen GJ\" und \"IST Zahlungen GJ\" vorkommen"
      # ktr_m12_input_ist_buchungen_01 ----
      ktr_m12_input_ist_buchungen_01 <- all(m12_input_ueb_istbuchungen[,"betragsart"] %in% c("IST Prämien GJ", "IST Provisionen GJ", "IST Zahlungen GJ"))
      
      ktr_m12_input_ist_buchungen_02_beschreibung <-  "Pruefen, ob in der Spalte betragsart der Tabellen m12_input_abg_istbuchungen nur die Eintraege \"IST Prämien GJ\", \"IST Provisionen GJ\" und \"IST Zahlungen GJ\" vorkommen"
      # ktr_m12_input_ist_buchungen_02 ----
      ktr_m12_input_ist_buchungen_02 <- all(m12_input_abg_istbuchungen[,"betragsart"] %in% c("IST Prämien GJ", "IST Provisionen GJ", "IST Zahlungen GJ"))
      
      ktr_m12_input_ist_buchungen_03_beschreibung <- "Pruefen, ob in der Spalte p_np_fak der Tabellen m12_input_abg_istbuchungen nur die Eintraege \"prop\", \"nprop\" vorkommen"
      # ktr_m12_input_ist_buchungen_03 ----
      ktr_m12_input_ist_buchungen_03 <- all(m12_input_abg_istbuchungen[,"p_np_fak"] %in% c("prop", "nprop"))
      
      ktr_m12_input_ist_buchungen_04_beschreibung <- "Pruefen, ob in der Spalte p_np_fak der Tabellen m12_input_ueb_istbuchungen nur die Eintraege \"prop\", \"nprop\" vorkommen"
      # ktr_m12_input_ist_buchungen_04 ----
      ktr_m12_input_ist_buchungen_04 <- all(m12_input_ueb_istbuchungen[,"p_np_fak"] %in% c("prop", "nprop"))
      
      ktr_m12_input_ist_buchungen_05_beschreibung <- "Pruefen, ob die Tabelle m12_input_ueb_istbuchungen noch Zeilen mit zedent_nr = 531 enthält (wenn ja dann FALSE)"
      # ktr_m12_input_ist_buchungen_05 ----
      ktr_m12_input_ist_buchungen_05 <- if_else(any(m12_input_ueb_istbuchungen[,"zedent_nr"] %in% c("531")), FALSE, TRUE)
      
      ktr_m12_input_ist_buchungen_06_beschreibung <- "Pruefen, ob die Tabelle m12_input_ueb_istbuchungen noch Zeilen mit zedent_nr = 040 und zessionaer_nr = 001 enthält (wenn ja dann FALSE)"
      # ktr_m12_input_ist_buchungen_06 ----
      ktr_m12_input_ist_buchungen_06 <-  nrow(m12_input_ueb_istbuchungen %>%
                                                # neue Spalte help, die angibt, ob zedent_nr=040 und gleichzeitig auch zessionaer_nr = 001 ist
                                                mutate(help = if_else(zedent_nr == "040" & zessionaer_nr == "001", TRUE, FALSE)) %>%
                                                filter(help==TRUE)) == 0
      
    }
    
    
    ktr_m13_input_rfb_01_beschreibung <- "Pruefen, ob die Spalte p_np_fak der Tabellen m13_input_rfb_abgabe und m13_input_rfb_uebernahme nur die Eintraege \"prop\", \"nprop\" und \"fak\" enthaelt"
    # ktr_m13_input_rfb_01 ----
    ktr_m13_input_rfb_01 <- all(m13_input_rfb_abgabe[,"p_np_fak"] %in% c("prop","nprop","fak"))
    
    ktr_m13_input_rfb_02_beschreibung <- "Pruefen, ob die Spalte geschaeftsart der Tabellen m13_input_rfb_abgabe nur die Eintraege \"20\" und \"40\" enthaelt"
    # ktr_m13_input_rfb_02 ----
    ktr_m13_input_rfb_02 <- all(m13_input_rfb_abgabe[,"geschaeftsart"] %in% c("20","40"))
    
    
    ktr_m15_input_vwk_01_beschreibung <- "Pruefen, ob die Spalte p_np_fak der Tabelle m15_input_verd_pr_vwk nur aus den Eintraegen prop und nprop besteht"
    # ktr_m15_input_vwk_01
    ktr_m15_input_vwk_01 <- all(m15_input_verd_pr_vwk[,"p_np_fak"] %in% c("prop","nprop"))
    
    ktr_m42_verd_praemie_letzter_4q_01_beschreibung <- "Pruefen, ob es leere Eintraege in der Tabelle m42_input_verd_praemie_abg gibt (ausgenommen die Spalte kges)"
    # ktr_m42_verd_praemie_letzter_4q_01 ----
    ktr_m42_verd_praemie_letzter_4q_01 <- if_else(anyNA(m42_input_verd_praemie_abg %>% 
                                                          select(-kges)), FALSE, TRUE)
    
    ktr_m42_verd_praemie_letzter_4q_02_beschreibung <- "Pruefen, ob es leere Eintraege in der Tabelle m42_input_verd_praemie_ueb gibt"
    # ktr_m42_verd_praemie_letzter_4q_02 ----
    ktr_m42_verd_praemie_letzter_4q_02 <- if_else(anyNA(m42_input_verd_praemie_ueb), FALSE, TRUE)
    
    ktr_m59_input_gebuchte_hgb_praemie_01_beschreibung <- "Pruefen das die Spalte p_np aus Tabelle m59_input_geb_praemie_abg nur die Einträge prop und nprop enthält"
    # ktr_m59_input_gebuchte_hgb_praemie_01 ----
    ktr_m59_input_gebuchte_hgb_praemie_01 <- if_else(nrow(m59_input_geb_praemie_abg %>%
                                                            filter(p_np == "")) == 0, TRUE, FALSE)
    
    ktr_m59_input_gebuchte_hgb_praemie_02_beschreibung <- "Pruefen das die Spalte p_np aus Tabelle m59_input_geb_praemie_ueb nur die Einträge prop und nprop enthält"
    # ktr_m59_input_gebuchte_hgb_praemie_02 ----
    ktr_m59_input_gebuchte_hgb_praemie_02 <- if_else(nrow(m59_input_geb_praemie_ueb %>%
                                                            filter(p_np == "")) == 0, TRUE, FALSE)
    
    
    
    if(substring(stichtag_aktuell,1,6) != "31.12."){
      
      ktr_input_dss_beschreibung <- "Kontrollvariable fuer die gesamte Excel-Datei"
      ktr_input_dss <- if_else(ktr_m04_input_gesellschaftsnummer_01 & ktr_m12_input_ist_buchungen_01 & ktr_m12_input_ist_buchungen_02 & ktr_m12_input_ist_buchungen_03 &
                                ktr_m12_input_ist_buchungen_04 & ktr_m12_input_ist_buchungen_05 & ktr_m12_input_ist_buchungen_06 & ktr_m13_input_rfb_01 & ktr_m13_input_rfb_02 &
                                ktr_m15_input_vwk_01 & ktr_m42_verd_praemie_letzter_4q_01 & ktr_m42_verd_praemie_letzter_4q_02 & ktr_m59_input_gebuchte_hgb_praemie_01 &
                                  ktr_m59_input_gebuchte_hgb_praemie_02, TRUE, FALSE)
    
      kontrollen_input_dss <- matrix(c("","ktr_input_dss",ktr_input_dss_beschreibung,ktr_input_dss,
                                           "M04","ktr_m04_input_gesellschaftsnummer_01",ktr_m04_input_gesellschaftsnummer_01_beschreibung,ktr_m04_input_gesellschaftsnummer_01,
                                           "M12","ktr_m12_input_ist_buchungen_01",ktr_m12_input_ist_buchungen_01_beschreibung,ktr_m12_input_ist_buchungen_01,
                                           "M12","ktr_m12_input_ist_buchungen_02",ktr_m12_input_ist_buchungen_02_beschreibung,ktr_m12_input_ist_buchungen_02,
                                           "M12","ktr_m12_input_ist_buchungen_03",ktr_m12_input_ist_buchungen_03_beschreibung,ktr_m12_input_ist_buchungen_03,
                                           "M12","ktr_m12_input_ist_buchungen_04",ktr_m12_input_ist_buchungen_04_beschreibung,ktr_m12_input_ist_buchungen_04,
                                           "M12","ktr_m12_input_ist_buchungen_05",ktr_m12_input_ist_buchungen_05_beschreibung,ktr_m12_input_ist_buchungen_05,
                                           "M12","ktr_m12_input_ist_buchungen_06",ktr_m12_input_ist_buchungen_06_beschreibung,ktr_m12_input_ist_buchungen_06,
                                           "M13","ktr_m13_input_rfb_01",ktr_m13_input_rfb_01_beschreibung,ktr_m13_input_rfb_01,
                                           "M13","ktr_m13_input_rfb_02",ktr_m13_input_rfb_02_beschreibung,ktr_m13_input_rfb_02,
                                           "M15","ktr_m15_input_vwk_01",ktr_m15_input_vwk_01_beschreibung,ktr_m15_input_vwk_01,
                                           "M42","ktr_m42_verd_praemie_letzter_4q_01",ktr_m42_verd_praemie_letzter_4q_01_beschreibung,ktr_m42_verd_praemie_letzter_4q_01,
                                           "M42","ktr_m42_verd_praemie_letzter_4q_02",ktr_m42_verd_praemie_letzter_4q_02_beschreibung,ktr_m42_verd_praemie_letzter_4q_02,
                                           "M59","ktr_m59_input_gebuchte_hgb_praemie_01",ktr_m59_input_gebuchte_hgb_praemie_01_beschreibung,ktr_m59_input_gebuchte_hgb_praemie_01,
                                           "M59","ktr_m59_input_gebuchte_hgb_praemie_02",ktr_m59_input_gebuchte_hgb_praemie_02_beschreibung,ktr_m59_input_gebuchte_hgb_praemie_02), byrow = TRUE, ncol = 4) %>%
                  as.data.frame() %>%
                  setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    }
        
    if(substring(stichtag_aktuell,1,6) == "31.12."){
      
      ktr_input_dss_beschreibung <- "Kontrollvariable fuer die gesamte Excel-Datei"
      ktr_input_dss <- if_else(ktr_m04_input_gesellschaftsnummer_01 &  ktr_m13_input_rfb_01 & ktr_m13_input_rfb_02 & ktr_m15_input_vwk_01 &
                                  ktr_m42_verd_praemie_letzter_4q_01 & ktr_m42_verd_praemie_letzter_4q_02 & ktr_m59_input_gebuchte_hgb_praemie_01 &
                                  ktr_m59_input_gebuchte_hgb_praemie_02, TRUE, FALSE)
      
      kontrollen_input_dss <- matrix(c("","ktr_input_dss",ktr_input_dss_beschreibung,ktr_input_dss,
                                       "M04","ktr_m04_input_gesellschaftsnummer_01",ktr_m04_input_gesellschaftsnummer_01_beschreibung,ktr_m04_input_gesellschaftsnummer_01,
                                       "M13","ktr_m13_input_rfb_01",ktr_m13_input_rfb_01_beschreibung,ktr_m13_input_rfb_01,
                                       "M13","ktr_m13_input_rfb_02",ktr_m13_input_rfb_02_beschreibung,ktr_m13_input_rfb_02,
                                       "M15","ktr_m15_input_vwk_01",ktr_m15_input_vwk_01_beschreibung,ktr_m15_input_vwk_01,
                                       "M42","ktr_m42_verd_praemie_letzter_4q_01",ktr_m42_verd_praemie_letzter_4q_01_beschreibung,ktr_m42_verd_praemie_letzter_4q_01,
                                       "M42","ktr_m42_verd_praemie_letzter_4q_02",ktr_m42_verd_praemie_letzter_4q_02_beschreibung,ktr_m42_verd_praemie_letzter_4q_02,
                                       "M59","ktr_m59_input_gebuchte_hgb_praemie_01",ktr_m59_input_gebuchte_hgb_praemie_01_beschreibung,ktr_m59_input_gebuchte_hgb_praemie_01,
                                       "M59","ktr_m59_input_gebuchte_hgb_praemie_02",ktr_m59_input_gebuchte_hgb_praemie_02_beschreibung,ktr_m59_input_gebuchte_hgb_praemie_02), byrow = TRUE, ncol = 4) %>%
      as.data.frame() %>%
      setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    }
    
    if(z==1) 
      ergebnistabelle <- kontrollen_input_dss
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
      bind_rows(kontrollen_input_dss)
    
    }
    


  
  # Kontrollen der Excel-Datei Input_FuV ###################################
  
  if("Input_FuV" %in% auswahl_validierung){
    
    z <- z+1
    
    kontrollen_input_fuv <- matrix(c("","ktr_input_fuv ", "Dummy", TRUE), byrow = TRUE, ncol = 4) %>%
      as.data.frame() %>%
      setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    
    if(z==1) 
      ergebnistabelle <- kontrollen_input_fuv
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
      bind_rows(kontrollen_input_fuv)
    

  }
  
  
  # Kontrollen der Excel-Datei Input_Rating ###############################
  
  if("Input_Rating" %in% auswahl_validierung){
    
    z <<- z+1
    
    ktr_m02_input_ratings_01_beschreibung <-  "Pruefen, ob einer zessionaer_nr anhand der Tabelle m02_input_ratings höchstens ein Rating zugeordnet werden kann" 
    # ktr_m02_input_ratings_01 ----
    ktr_m02_input_ratings_01 <- if_else(nrow(m02_input_ratings) ==
                                                   length(m02_input_ratings[,"zessionaer_nr"] %>% unique), TRUE, FALSE )
    
    ktr_input_rating_beschreibung <- "Kontrollvariable fuer die gesamte Excel-Datei"
    ktr_input_rating <- if_else(ktr_m02_input_ratings_01, TRUE, FALSE)
    
    kontrollen_input_rating <- matrix(c("","ktr_input_rating", ktr_input_rating_beschreibung, ktr_input_rating,
                                        "M02","ktr_m02_input_ratings_01",ktr_m02_input_ratings_01_beschreibung,ktr_m02_input_ratings_01), byrow = TRUE, ncol = 4) %>%
      as.data.frame() %>%
      setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    
    if(z==1) 
      ergebnistabelle <- kontrollen_input_rating
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
      bind_rows(kontrollen_input_rating)
    
  } 

  
  # Kontrollen der Excel-Datei Input_RV_Spiegel ###############################
  
  if("Input_RV_Spiegel" %in% auswahl_validierung){
    
    z <- z+1
    
    ktr_m06_input_rv_spiegel_01_beschreibung <- "Pruefen, ob in Tabelle m06_input_vtrgzuordnung_plandaten %>% filter(geschaeftsart=='30') die Spalten \"p_np_fak\",\"zedent_nr\",\"zessionaer_nr\",\"sparte\" eine eindeutige Zuordnung einer vertragsbezeichnung gewährleisten"   
    # ktr_m06_input_rv_spiegel_01 ----
    ktr_m06_input_rv_spiegel_01 <- if_else(nrow(m06_input_vtrgzuordnung_plandaten %>% filter(geschaeftsart=="30")) == nrow((m06_input_vtrgzuordnung_plandaten %>% filter(geschaeftsart=="30")) [,c("p_np_fak","zedent_nr","zessionaer_nr","sparte")] %>% unique), TRUE, FALSE)
    
    ktr_m06_input_rv_spiegel_02_beschreibung <- "Pruefen, ob bei Tabelle m06_input_vtrgzuordnung_plandaten anhand der Spalten p_np_fak, zedent_nr, zessionaer_nr, sparte eine eindeutige Zuordnung auf eine vertragsbezeichnung gewaehrleistet ist"
    # ktr_m06_input_rv_spiegel_02 ----
    ktr_m06_input_rv_spiegel_02 <- if_else(nrow(m06_input_vtrgzuordnung_plandaten) == nrow(m06_input_vtrgzuordnung_plandaten[,c("p_np_fak", "zedent_nr", "zessionaer_nr", "sparte")] %>% unique), TRUE, FALSE)
    
    ktr_m06_input_rv_spiegel_03_beschreibung <- "Pruefen, ob in Tabelle m06_input_praemie_xl_vtgr jede vertragsbezeichnung nur einmal vorkommt"
    # ktr_m06_input_rv_spiegel_03 ----
    ktr_m06_input_rv_spiegel_03 <- if_else(nrow(m06_input_praemie_xl_vtgr) == nrow(m06_input_praemie_xl_vtgr %>%
                                                                                             select(vertragsbezeichnung) %>%
                                                                                             unique()), TRUE, FALSE)
    
    
    ktr_input_rv_spiegel_beschreibung <- "Kontrollvariable fuer die gesamte Excel-Datei"
    ktr_input_rv_spiegel <- if_else(ktr_m06_input_rv_spiegel_01 & ktr_m06_input_rv_spiegel_02 & ktr_m06_input_rv_spiegel_03, TRUE, FALSE)
    
    
    kontrollen_input_rv_spiegel <- matrix(c("","ktr_input_rv_spiegel",ktr_input_rv_spiegel_beschreibung,ktr_input_rv_spiegel,
                                                  "M06","ktr_m06_input_rv_spiegel_01",ktr_m06_input_rv_spiegel_01_beschreibung,ktr_m06_input_rv_spiegel_01,
                                                  "M06","ktr_m06_input_rv_spiegel_02",ktr_m06_input_rv_spiegel_02_beschreibung,ktr_m06_input_rv_spiegel_02,
                                                  "M06","ktr_m06_input_rv_spiegel_03",ktr_m06_input_rv_spiegel_03_beschreibung,ktr_m06_input_rv_spiegel_03), byrow = TRUE, ncol = 4) %>%
      as.data.frame() %>%
      setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    
    if(z==1) 
      ergebnistabelle <- kontrollen_input_rv_spiegel
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
      bind_rows(kontrollen_input_rv_spiegel)
    
    
   
  }
  
  # Kontrollen der Excel-Datei Input_ZAK ###################################
  
  if("Input_ZAK" %in% auswahl_validierung){
    
    z <<- z+1
    
    kontrollen_input_zak <- matrix(c("","ktr_input_zak", "Dummy", TRUE), byrow = TRUE, ncol = 4) %>%
      as.data.frame() %>%
      setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    
    if(z==1) 
      ergebnistabelle <- kontrollen_input_zak
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
      bind_rows(kontrollen_input_zak)
    

  }
  

  # Kontrollen mit Abhängigkeit von der Excel-Datei Input_allgemeine_Daten.xlsx und der Excel-Datei Input_Controlling  #######################
  
  if("Input_allgemeine_Daten" %in% auswahl_validierung & "Input_Controlling" %in% auswahl_validierung){
    
    z <- z+1
    
    if(substring(stichtag_aktuell,4,5)!="12"){
    ktr_m03_m05_m08_m36_01_beschreibung <- "Pruefen, ob alle Sparten, die in den Tabellen m05_input_piv, m08_input_piv_vq und m36_input_planung enthalten sind, auch in der Tabelle m03_input_sparte_auf_s2segment enthalten sind."
    # ktr_m03_m05_m08_m36_01 ----
    ktr_m03_m05_m08_m36_01 <- all((m05_input_piv %>% select(sheet_name) %>%
                                    bind_rows(m08_input_piv_vq %>% select(sheet_name)) %>%
                                    bind_rows(m36_input_planung %>% select(sheet_name)))[,"sheet_name"] %in%
                                    m03_input_sparte_auf_s2segment[,"sparte"])
    }
    
    if(substring(stichtag_aktuell,4,5)=="12"){
      ktr_m03_m36_01_beschreibung <- "Pruefen, ob alle Sparten, die in Tabelle  m36_input_planung enthalten sind, auch in der Tabelle m03_input_sparte_auf_s2segment enthalten sind."
      # ktr_m03_m36_01 ----
      ktr_m03_m36_01 <- all(m36_input_planung[,"sheet_name"] %in%
                                      m03_input_sparte_auf_s2segment[,"sparte"])
    }
   
    if(substring(stichtag_aktuell,4,5)!="12")
    kontrollen_input_allgemeine_daten_input_controlling <- matrix(c( "M03 & M05 & M08 & M36","ktr_m03_m05_m08_m36_01",ktr_m03_m05_m08_m36_01_beschreibung,ktr_m03_m05_m08_m36_01), byrow = TRUE, ncol = 4) %>%
      as.data.frame() %>%
      setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    
    if(substring(stichtag_aktuell,4,5)=="12")
      kontrollen_input_allgemeine_daten_input_controlling <- matrix(c( "M03 & M36","ktr_m03_m36_01",ktr_m03_m36_01_beschreibung, ktr_m03_m36_01), byrow = TRUE, ncol = 4) %>%
        as.data.frame() %>%
        setNames(c("Modul","Name_der_Kontrolle", "Beschreibung", "Status"))
    
    
    if(z==1) 
      ergebnistabelle <- kontrollen_input_allgemeine_daten_input_controlling
    if(z>1)
      ergebnistabelle <- ergebnistabelle %>%
      bind_rows(kontrollen_input_allgemeine_daten_input_controlling)
    
    
  }
  
  
   
  
  # Outpput Tabelle mit allen Ergebnissen der Validierung
  return(ergebnistabelle)
  

  
}
