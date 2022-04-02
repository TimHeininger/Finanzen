function(input, output, session){
  
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Input Daten ####
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$pfad_inputs,{
    pfad_berechnungsordner <<- choose.dir(caption="Wählen Sie den Ordner der Input Dateien aus",default="C:\\IFRS\\Planung_Hochrechnung\\SPL_2022")
    output$pfad_berechnungsordner <- renderText({pfad_berechnungsordner})  
  })
    
  
  observeEvent(input$pfad_R_code,{
    pfad_R_code<<- choose.dir(caption="Wählen Sie den Ordner der R Dateien aus",default="C:\\GitHub\\Planung_Hochrechnung\\Shiny_Projekt\\Module")
    output$pfad_R_code <- renderText({pfad_R_code}) 
    # source(paste(pfad_R_code,"function_I_Planung_LIC.R", sep = "\\"))
    # source(paste(pfad_R_code,"function_II_Planung_LIC.R", sep = "\\"))
    # source(paste(pfad_R_code,"function_III_Planung_LRC.R", sep = "\\"))
    # source(paste(pfad_R_code,"function_IV_Bilanz.R", sep = "\\"))
    # source(paste(pfad_R_code,"function_V_csv_Dateien_PlaTo.R", sep = "\\"))
    # source(paste(pfad_R_code,"function_VI_Steuerung.R", sep = "\\"))
  })
  
  
  observeEvent(input$pfad_R_lib,{
    pfad_R_lib <<- choose.dir(caption="Wählen Sie den Ordner der R Library aus",default="C:\\GitHub\\Planung_Hochrechnung\\renv\\library\\R-4.1\\x86_64-w64-mingw32")
    output$pfad_R_lib <- renderText({pfad_R_lib})
  })
  
  
  observeEvent(input$einloggen_dss,{
    con<<-dbConnect(
      odbc::odbc(),
      paste("Greenplum DSS",input$PROD_TEST, sep = ""),
      uid = input$xv_nummer,
      pwd = input$password,
      encoding = "windows-1252"
    )
    con_TEST<<-dbConnect(
      odbc::odbc(),
      paste("Greenplum DSS","TEST", sep = ""),
      uid = input$xv_nummer,
      pwd = input$password,
      encoding = "windows-1252"
    )
    output$verbindung_dss<-renderText({"Datenbankverbindung erfolgreich hergestellt"})
  })
  
  
  # stichtag_daten<-ymd("2020-12-31")
  # stichtag<-ymd("2020-12-31")
  # schema<-"ft_ifrs7_2012"
  # name_Cockpit<-"Cockpit_Planung_v5_8.xlsm"
  # I_Planung_LIC(pfad_berechnungsordner,stichtag_daten,stichtag,schema,name_Cockpit,pfad_R_lib,con)
  
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # LIC Berechnung ####
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  observeEvent(input$r_code_LIC_I, {
    source(paste(pfad_R_code,"function_I_Planung_LIC.R", sep = "/"))
    I_Planung_LIC(pfad_berechnungsordner,input$stichtag_daten, input$stichtag,input$schema, input$name_Cockpit,pfad_R_lib,con)
    
    output$validierung_planung_lic<-renderText({"Die Datei zur Validierung der LIC Faktoren ist erstellt"})
  })
  
  
  observeEvent(input$r_code_LIC_II, {
    source(paste(pfad_R_code,"function_II_Planung_LIC.R", sep = "/"))
    II_Planung_LIC(pfad_berechnungsordner,
                   input$stichtag_daten, 
                   input$stichtag,
                   input$schema, 
                   input$name_Cockpit,
                   pfad_R_lib,
                   input$Pversion,
                   5,
                   pfad_R_code,
                   con,
                   con_TEST)
    
    output$r_code_LIC_II<-renderText({"Cashflows für LIC wurden erfolgreich berechnet"})
    
    
  })
  
 

  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # LRC (PAA) Berechnung ####
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  observeEvent(input$r_code_LRC_III, {
    source(paste(pfad_R_code,"function_III_Planung_LRC.R", sep = "/"))
    III_Planung_LRC(pfad_berechnungsordner,
                    input$stichtag_daten, 
                    input$stichtag,
                    input$schema, 
                    input$name_Cockpit,
                    pfad_R_lib,
                    input$Pversion,
                    input$faktor_lrc_paa,
                    con)
    output$r_code_LRC_III<-renderText({"Cashflows für LRC - PAA wurden erfolgreich erzeugt"})
    
  })
  
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Bilanz und GuV ####
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  observeEvent(input$r_code_IV, {
    source(paste(pfad_R_code,"function_IV_Bilanz.R", sep = "/"))
    Datenbank_Planung<<-IV_Bilanz(pfad_berechnungsordner,
                                  input$name_Cockpit,
                                  pfad_R_lib,
                                  input$stichtag,
                                  input$Pversion)
    output$r_code_IV<-renderText({"Bilanz und GuV Daten erzeugt"})
    
    #' Filter auslesen
    
    
    Datenbank_Planung %>% select(gesch_art) %>% distinct %>% unlist %>% unname ->> .liste_gesch_art
    Datenbank_Planung %>% select(prop_nonprop) %>% distinct %>% unlist %>% unname ->> .liste_prop_nonprop
    Datenbank_Planung %>% select(fak_obl) %>% distinct %>% unlist %>% unname ->> .liste_fak_obl
    Datenbank_Planung %>% select(gsch_bil) %>% distinct %>% unlist %>% unname ->> .liste_gsch_bil
    Datenbank_Planung %>% select(basis_pf) %>% distinct %>% unlist %>% unname ->> .liste_basis_pf
    Datenbank_Planung %>% select(anfall_jahr) %>% distinct %>% unlist %>% unname ->> .liste_anfall_jahr
    Datenbank_Planung %>% select(gsch_par) %>% distinct %>% unlist %>% unname ->> .liste_gsch_par
    
  })

  
  
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Steuerung ####
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  #' Dropdown Menu
  
  # gesch_art ####
  
  #' Sorting asc
  observeEvent(input$steuerung_gesch_art_a2z, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_gesch_art", choices = c(20,30,40), selected = input$steuerung_gesch_art
    )
  })
  #' Sorting desc
  observeEvent(input$steuerung_gesch_art_z2a, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_gesch_art", choices = c(20,30,40), selected = input$steuerung_gesch_art
    )
  })
  output$res2 <- renderPrint({
    input$check2
  })
  #' Select all / Unselect all
  observeEvent(input$steuerung_gesch_art_all, {
    if (is.null(input$steuerung_gesch_art)) {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_gesch_art", selected = c(20,30,40)
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_gesch_art", selected = ""
      )
    }
  })
  
  
  
  # prop_nonprop ####
  
  
  #' Sorting asc
  observeEvent(input$steuerung_prop_nonprop_a2z, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_prop_nonprop", choices = c("prop_nonporp","prop"), selected = input$steuerung_prop_nonprop
    )
  })
  #' Sorting desc
  observeEvent(input$steuerung_prop_nonprop_z2a, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_prop_nonprop", choices = c("prop_nonporp","prop"), selected = input$steuerung_prop_nonprop
    )
  })
  output$res2 <- renderPrint({
    input$check2
  })
  #' Select all / Unselect all
  observeEvent(input$steuerung_prop_nonprop_all, {
    if (is.null(input$steuerung_prop_nonprop)) {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_prop_nonprop", selected = c("prop_nonporp","prop")
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_prop_nonprop", selected = ""
      )
    }
  })
  
  
  # fak_obl ####
  
  
  #' Sorting asc
  observeEvent(input$steuerung_fak_obl_a2z, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_fak_obl", choices = c("fak","obl"), selected = input$steuerung_fak_obl
    )
  })
  #' Sorting desc
  observeEvent(input$steuerung_fak_obl_z2a, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_fak_obl", choices = c("fak","obl"), selected = input$steuerung_fak_obl
    )
  })
  output$res2 <- renderPrint({
    input$check2
  })
  #' Select all / Unselect all
  observeEvent(input$steuerung_fak_obl_all, {
    if (is.null(input$steuerung_fak_obl)) {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_fak_obl", selected = c("fak","obl")
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_fak_obl", selected = ""
      )
    }
  })
  
  
  # gsch_bil ####
  
  
  #' Sorting asc
  observeEvent(input$steuerung_gsch_bil_a2z, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_gsch_bil", choices = c("001","010","023","025","040","052","325","531"), selected = input$steuerung_gsch_bil
    )
  })
  #' Sorting desc
  observeEvent(input$steuerung_gsch_bil_z2a, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_gsch_bil", choices = c("001","010","023","025","040","052","325","531"), selected = input$steuerung_gsch_bil
    )
  })
  output$res2 <- renderPrint({
    input$check2
  })
  #' Select all / Unselect all
  observeEvent(input$steuerung_gsch_bil_all, {
    if (is.null(input$steuerung_gsch_bil)) {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_gsch_bil", selected = c("001","010","023","025","040","052","325","531")
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_gsch_bil", selected = ""
      )
    }
  })
  
  
  # gsch_par ####
  
  #' Sorting asc
  observeEvent(input$steuerung_gsch_par_a2z, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_gsch_par", choices = c("001","010","023","025","040","052","325","531","ext"), selected = input$steuerung_gsch_par
    )
  })
  #' Sorting desc
  observeEvent(input$steuerung_gsch_par_z2a, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_gsch_par", choices = c("001","010","023","025","040","052","325","531","ext"), selected = input$steuerung_gsch_par
    )
  })
  output$res2 <- renderPrint({
    input$check2
  })
  #' Select all / Unselect all
  observeEvent(input$steuerung_gsch_par_all, {
    if (is.null(input$steuerung_gsch_par)) {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_gsch_par", selected = c("001","010","023","025","040","052","325","531","ext")
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_gsch_par", selected = ""
      )
    }
  })
  
  
  # BE_RA ####
  
  #' Sorting asc
  observeEvent(input$steuerung_BE_RA_a2z, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_BE_RA", choices = c("BE","RA"), selected = input$steuerung_BE_RA
    )
  })
  #' Sorting desc
  observeEvent(input$steuerung_BE_RA_z2a, {
    updateCheckboxGroupInput(
      session = session, inputId = "steuerung_BE_RA", choices = c("BE","RA"), selected = input$steuerung_BE_RA
    )
  })
  output$res2 <- renderPrint({
    input$check2
  })
  #' Select all / Unselect all
  observeEvent(input$steuerung_BE_RA_all, {
    if (is.null(input$steuerung_BE_RA)) {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_BE_RA", selected = c("BE","RA")
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "steuerung_BE_RA", selected = ""
      )
    }
  })
  
  
  # Bilanz erstellen ###
  
  
  observeEvent(input$steuerung_bilanz_erstellen,{
                source(paste(pfad_R_code,"function_VI_Steuerung.R", sep = "/"))
               VI_Steuerung(input$steuerung_gesch_art,
                            input$steuerung_prop_nonprop,
                            input$steuerung_fak_obl,
                            input$steuerung_gsch_bil,
                            input$steuerung_gsch_par,
                            input$steuerung_BE_RA,
                            pfad_R_lib,
                            pfad_berechnungsordner) ->
              table_Bilanz
               
              table_Bilanz %>% 
                kbl(caption = "Bilanz und GuV IFRS 17") %>%
                kable_styling(bootstrap_options = c("condensed"), font_size = 8) %>%
                kable_material_dark(full_width = F, html_font = "Arial") ->
                table_Bilanz

              #  output$steuerung_bilanz_table<-renderTable({table_Bilanz})
               
               
               
               # output$steuerung_bilanz_table<-renderPlot({table_Bilanz})
               # output$steuerung_bilanz_table<-table_Bilanz
               output$steuerung_bilanz_table <- function(){table_Bilanz}
               
               }
               )
  
  
  
  
  
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # csv Dateien fuer PlaTo erstellen ####
  #' ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$r_code_V, {
    source(paste(pfad_R_code,"function_V_csv_Dateien_PlaTo.R", sep = "/"))
    Datenbank_Planung<<-V_Bilanz(pfad_berechnungsordner,
                                  input$name_Cockpit,
                                  pfad_R_lib,
                                  input$name_vorlage,
                                  input$sheet_vorlage)
    output$r_code_V<-renderText({"CSV Dateien erzeugt"})
  })
  

  
  
  
  
  # observeEvent(input$validierung_kontrollen_pr_anzeige_cell_edit, {
  #   
  #   validierung_kontrollen_pr_filtern[input$validierung_kontrollen_pr_anzeige_cell_edit$row, 
  #                                        input$validierung_kontrollen_pr_anzeige_cell_edit$col + 1] <<- input$validierung_kontrollen_pr_anzeige_cell_edit$value
  #   
  #   validierung_kontrollen_pr <<- validierung_kontrollen_pr %>%
  #     left_join(validierung_kontrollen_pr_filtern %>% 
  #                 select(Name_der_Kontrolle, Kommentar) %>%
  #                 rename(Kommentar_neu = Kommentar), by=c("Name_der_Kontrolle")) %>%
  #     mutate(Kommentar = if_else(is.na(Kommentar_neu), Kommentar, Kommentar_neu)) %>%
  #     select(-Kommentar_neu)
  #   
  #   write.xlsx(validierung_kontrollen_pr, paste0(pfad_berechnungsordner,"\\04 Praemienrisiko\\Kontrollen_Praemienrisiko.xlsx"), overwrite= TRUE)
  #   
  # })
  # 
  # 
  # observeEvent(input$pr_ergebnisse_speichern, {
  #   
  #   write.csv2(m40_fp_future_ueb_vtrg, file = paste0(paste0(.pfad_pr,"//m40_fp_future_ueb_vtrg_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
  #   write.csv2(m43_fp_future_nach_retro, file = paste0(paste0(.pfad_pr,"//m43_fp_future_nach_retro_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
  #   write.csv2(m39_ps_abgabe_nach_retro, file = paste0(paste0(.pfad_pr,"//m39_ps_abgabe_nach_retro_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
  #   write.csv2(m37_ps_uebernahme_vtrg, file = paste0(paste0(.pfad_pr,"//m37_ps_uebernahme_vtrg_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
  #   write.csv2(m44_p_last_nach_retro, file = paste0(paste0(.pfad_pr,"//m44_p_last_nach_retro_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
  #   write.csv2(m42_input_verd_praemie_ueb, file = paste0(paste0(.pfad_pr,"//m42_input_verd_praemie_ueb_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
  #   
  #   write.xlsx(validierung_kontrollen_pr, paste0(pfad_berechnungsordner,"\\04 Praemienrisiko\\Kontrollen_Praemienrisiko_",stichtag_aktuell,".xlsx"), overwrite= TRUE)
  #   
  #   
  #   output$pr_speichern <- renderText("Das Speichern der Ergebnisse ist abgeschlossen.")
  # 
  # })
  
#   observeEvent(input$srs_aufbereiten_berechnen,{
#     source("./Module/m17_fkt_prop_anteile_retro.R")
#     source("./Module/m19_fuv_aufb_retro_aufteilung.R", encoding = "utf8")
#     source("./Module/m57_ausfallw_keiten_aufbereiten.R")
#     source("./Module/m67_zinskurve_aufbereiten.R", encoding = "utf8")
#     source("./Module/m53_diskontfaktoren_aufbereiten.R")
#     source("./Module/m55_srs_renten_u_bta.R")
#     source("./Module/m56_srs_erwarteter_ausfall.R")
#     source("./Module/rshiny_04_srs_kontrollen.R")
# 
#     output$srs_ist_berechnet <- renderText("Die Berechnung der Schadenrückstellungen ist abgeschlossen.")
#     
#     tabelle_kontrollen_srs <- kontrollen_srs() 
#     
#     if(nrow(tabelle_kontrollen_srs %>% filter(Status == FALSE))==0)
#       output$kontrollen_srs_alles_auf_true <- renderText("Alle Kontrollen sind auf TRUE.")
#     
#     z <- 0
#     
#     if(file.exists(paste0(pfad_berechnungsordner,"\\02 Schadenrueckstellungen\\Kontrollen_SRS.xlsx")))
#     {
#       z <- 1
#       kontrolle_srs_alt <- read.xlsx(paste0(pfad_berechnungsordner,"\\02 Schadenrueckstellungen\\Kontrollen_SRS.xlsx"))
#       validierung_kontrollen_srs <<- tabelle_kontrollen_srs %>%
#         left_join(kontrolle_srs_alt %>%
#                     select(Name_der_Kontrolle, Kommentar), by=c("Name_der_Kontrolle")) %>%
#         mutate(Kommentar = if_else(is.na(Kommentar), "", Kommentar))
#        }
#     else{
#       validierung_kontrollen_srs <<- tabelle_kontrollen_srs %>% mutate(Kommentar = "")
#       }
#     
#     write.xlsx(validierung_kontrollen_srs,paste0(pfad_berechnungsordner,"\\02 Schadenrueckstellungen\\Kontrollen_SRS.xlsx"), overwrite= TRUE)
#     
#     validierung_kontrollen_srs_filtern <<- validierung_kontrollen_srs %>% filter(Status == FALSE)
#     
#     output$validierung_kontrollen_srs_anzeige <- renderDT(validierung_kontrollen_srs_filtern, 
#                                                           options = list(language = list(url = "German.json"),
#                                                                          pageLength = 15),
#                                                           rownames = FALSE,
#                                                           editable = TRUE)
#     
#     output$kontrollen_srs_gespeichert <- renderText(paste0("Die Kontrollen wurden unter ",paste0(paste0(pfad_berechnungsordner,"\\02 Schadenrueckstellungen\\Kontrollen_SRS.xlsx")," gespeichert.")))
#     
#   })
#   
#   
#   observeEvent(input$validierung_kontrollen_srs_anzeige_cell_edit, {
#     
#     validierung_kontrollen_srs_filtern[input$validierung_kontrollen_srs_anzeige_cell_edit$row, 
#                                       input$validierung_kontrollen_srs_anzeige_cell_edit$col + 1] <<- input$validierung_kontrollen_srs_anzeige_cell_edit$value
#     
#     validierung_kontrollen_srs <<- validierung_kontrollen_srs %>%
#       left_join(validierung_kontrollen_srs_filtern %>% 
#                   select(Name_der_Kontrolle, Kommentar) %>%
#                   rename(Kommentar_neu = Kommentar), by=c("Name_der_Kontrolle")) %>%
#       mutate(Kommentar = if_else(is.na(Kommentar_neu), Kommentar, Kommentar_neu)) %>%
#       select(-Kommentar_neu)
#     
#     write.xlsx(validierung_kontrollen_srs,paste0(pfad_berechnungsordner,"\\02 Schadenrueckstellungen\\Kontrollen_SRS.xlsx"), overwrite= TRUE)
#     
#   })
#   
# 
#    observeEvent(input$srs_ergebnisse_speichern, {
#      
#     write.csv2(m56_abgabe_becf_nach_erw_ausf, file = paste0(paste0(.pfad_srs,"//m56_abgabe_becf_nach_erw_ausf_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m56_cf_erw_ausf, file = paste0(paste0(.pfad_srs,"//m56_cf_erw_ausf_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m55_srs_cf, file = paste0(paste0(.pfad_srs,"//m55_srs_cf_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m19_fuv_srs_abgabe, file = paste0(paste0(.pfad_srs,"//m19_fuv_srs_abgabe_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m19_fuv_srs_uebernahme, file = paste0(paste0(.pfad_srs,"//m19_fuv_srs_uebernahme_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m53_diskontfaktoren, file = paste0(paste0(.pfad_srs,"//m53_diskontfaktoren_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     
#     output$srs_speichern <- renderText("Das Speichern der Ergebnisse ist abgeschlossen.")
#   })
#    
#    observeEvent(input$input_laufzeitbaender_speichern, {
#      source("./Module/m66_laufzeitbaender.R", encoding = "utf8")
#      
#      output$laufzeitbaender_speichern <- renderText("Das Speichern der Input-Tabellen für die Laufzeitbänder ist abgeschlossen.")
#    })
#   
#   observeEvent(input$prs_aufbereiten_berechnen,{
#     
#     m56_abgabe_becf_nach_erw_ausf <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m56_abgabe_becf_nach_erw_ausf_"),paste0(stichtag_aktuell,".csv")), col_names  = TRUE, locale = locale("de", encoding="latin1")) %>%
#                                             select(-1)
#     m55_srs_cf <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m55_srs_cf_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1"))%>%
#       select(-1)
# 
#     
#     source("./Module/m10_gj_srs_aufbereiten.R")
#     source("./Module/m17_fkt_prop_anteile_retro.R")
#     source("./Module/m18_vwk_aufbereiten.R")
#     source("./Module/m19_fuv_aufb_retro_aufteilung.R", encoding = "utf8")
#     source("./Module/m20_ist_buchungen_retro_aufteilung.R")
#     source("./Module/m22_controlling_ag_aufbereiten.R")
#     source("./Module/m23_erwartete_zession_aufbereiten.R")
#     source("./Module/m24_prs_modellinput_abgabe.R")
#     source("./Module/m25_controlling_ueg_aufbereitet.R")
#     source("./Module/m57_ausfallw_keiten_aufbereiten.R")
#     source("./Module/m26_prs_abgabe_berechnung.R")
#     source("./Module/m27_prs_uebernahme_berechnung.R")
#     source("./Module/m28_prs_retro_aufteilung.R")
#     source("./Module/m29_prs_cashflows_uebernahme.R")
#     source("./Module/m30_prs_cashflows_abgabe.R")
#     source("./Module/m31_prs_erwarteter_ausfall.R")
#     source("./Module/m48_prs_cf_abgabe_gesamt.R")
#     source("./Module/m49_prs_cf_uebernahme_gesamt.R")
#     source("./Module/rshiny_05_prs_kontrollen.R")
#     
#     wb <- createWorkbook()
#     header_st <- createStyle(textDecoration = "Bold")
#     num_format <- createStyle(numFmt = "COMMA")
#     # Tabellenblatt SRS befüllen
#     addWorksheet(wb,"Abgabe")
#     writeData(wb,"Abgabe",m48_prs_abgabe_gesamt_validierung, startCol = 1, startRow = 1)
#     addStyle(wb, sheet = "Abgabe", header_st, rows = 1, cols = 1:90, gridExpand = TRUE)
#     addStyle(wb, sheet = "Abgabe", num_format, rows = 2:6000, cols = 1:90, gridExpand = TRUE)
#     addWorksheet(wb,"Uebernahme")
#     writeData(wb,"Uebernahme",m49_prs_uebernahme_gesamt_validierung, startCol = 1, startRow = 1)
#     addStyle(wb, sheet = "Uebernahme", header_st, rows = 1, cols = 1:90, gridExpand = TRUE)
#     addStyle(wb, sheet = "Uebernahme", num_format, rows = 2:6000, cols = 1:90, gridExpand = TRUE)
#     addWorksheet(wb,"M24 PRS Abgabe doppelter Aufw")
#     writeData(wb,"M24 PRS Abgabe doppelter Aufw",m24_controlling_abg_dop_aufw, startCol = 1, startRow = 1)
#     addStyle(wb, sheet = "M24 PRS Abgabe doppelter Aufw", header_st, rows = 1, cols = 1:90, gridExpand = TRUE)
#     addStyle(wb, sheet = "M24 PRS Abgabe doppelter Aufw", num_format, rows = 2:6000, cols = 1:90, gridExpand = TRUE)
#     addWorksheet(wb,"M25 Uebernahme doppelter Aufw")
#     writeData(wb,"M25 Uebernahme doppelter Aufw",m25_controlling_ueb_dop_aufw, startCol = 1, startRow = 1)
#     addStyle(wb, sheet = "M25 Uebernahme doppelter Aufw", header_st, rows = 1, cols = 1:90, gridExpand = TRUE)
#     addStyle(wb, sheet = "M25 Uebernahme doppelter Aufw", num_format, rows = 2:6000, cols = 1:90, gridExpand = TRUE)
#     
#     saveWorkbook(wb, file = paste0(paste0(paste0(.pfad_prs,"/Input_Validierung_PRS_"),stichtag_aktuell),".xlsx"), overwrite = TRUE)
#     
#     
#     output$prs_ist_berechnet <- renderText("Die Berechnung der Prämienrückstellungen ist abgeschlossen.")
# 
#     tabelle_kontrollen_prs <- kontrollen_prs() 
#     
#     if(nrow(tabelle_kontrollen_prs %>% filter(Status == FALSE))==0)
#       output$kontrollen_prs_alles_auf_true <- renderText("Alle Kontrollen sind auf TRUE.")
# 
#     z <- 0
#     
#     if(file.exists(paste0(pfad_berechnungsordner,"\\03 Praemienrueckstellungen\\Kontrollen_PRS.xlsx")))
#     {
#       z <- 1
#       kontrolle_prs_alt <- read.xlsx(paste0(pfad_berechnungsordner,"\\03 Praemienrueckstellungen\\Kontrollen_PRS.xlsx"))
#       validierung_kontrollen_prs <<- tabelle_kontrollen_prs %>%
#         left_join(kontrolle_prs_alt %>%
#                     select(Name_der_Kontrolle, Kommentar), by=c("Name_der_Kontrolle")) %>%
#         mutate(Kommentar = if_else(is.na(Kommentar), "", Kommentar))
#     }
#     else{
#       validierung_kontrollen_prs <<- tabelle_kontrollen_prs %>% mutate(Kommentar = "")
#        }
#     
#     write.xlsx(validierung_kontrollen_prs,paste0(pfad_berechnungsordner,"\\03 Praemienrueckstellungen\\Kontrollen_PRS_tmp.xlsx"), overwrite= TRUE)
#     
#     validierung_kontrollen_prs_filtern <<- validierung_kontrollen_prs %>% filter(Status == FALSE)
#     
#     output$validierung_kontrollen_prs_anzeige <-   renderDT(validierung_kontrollen_prs_filtern, 
#                                                             options = list(language = list(url = "German.json"),
#                                                                            pageLength = 15),
#                                                             rownames = FALSE,
#                                                             editable = TRUE)
#     
#     output$kontrollen_prs_gespeichert <- renderText(paste0("Die Kontrollen wurden unter ",paste0(paste0(pfad_berechnungsordner,"\\03 Praemienrueckstellungen\\Kontrollen_PRS.xlsx")," gespeichert.")))
#     
#   })
#   
#   
#   observeEvent(input$validierung_kontrollen_prs_anzeige_cell_edit, {
#     
#     validierung_kontrollen_prs_filtern[input$validierung_kontrollen_prs_anzeige_cell_edit$row, 
#                                        input$validierung_kontrollen_prs_anzeige_cell_edit$col + 1] <<- input$validierung_kontrollen_prs_anzeige_cell_edit$value
#     
#     validierung_kontrollen_prs <<- validierung_kontrollen_prs %>%
#       left_join(validierung_kontrollen_prs_filtern %>% 
#                   select(Name_der_Kontrolle, Kommentar) %>%
#                   rename(Kommentar_neu = Kommentar), by=c("Name_der_Kontrolle")) %>%
#       mutate(Kommentar = if_else(is.na(Kommentar_neu), Kommentar, Kommentar_neu)) %>%
#       select(-Kommentar_neu)
#     
#     write.xlsx(validierung_kontrollen_prs,paste0(pfad_berechnungsordner,"\\03 Praemienrueckstellungen\\Kontrollen_PRS.xlsx"), overwrite= TRUE)
#     
#   })
#   
#   
#   observeEvent(input$prs_ergebnisse_speichern, {
#     write.csv2(m48_prs_cf_abgabe_gesamt, file = paste0(paste0(.pfad_prs,"//m48_prs_cf_abgabe_gesamt_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m49_prs_cf_uebernahme_gesamt, file = paste0(paste0(.pfad_prs,"//m49_prs_cf_uebernahme_gesamt_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m48_prs_cf_abgabe_gesamt_lieferung, file = paste0(paste0(.pfad_prs,"//m48_prs_cf_abgabe_gesamt_lieferung_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m49_prs_cf_uebernahme_gesamt_lieferung, file = paste0(paste0(.pfad_prs,"//m49_prs_cf_uebernahme_gesamt_lieferung_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m48_prs_cf_abgabe, file = paste0(paste0(.pfad_prs,"//m48_prs_cf_abgabe"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     write.csv2(m31_cf_erw_ausf, file = paste0(paste0(.pfad_prs,"//m31_cf_erw_ausf"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#     
#     write.xlsx(m30_abgabe_becf_v_erw_ausf, file = paste0(paste0(.pfad_prs,"//m30_abgabe_becf_v_erw_ausf_"),paste0(stichtag_aktuell,".xlsx")), overwrite = TRUE)
#     
#     write.xlsx(validierung_kontrollen_prs,paste0(pfad_berechnungsordner,"\\03 Praemienrueckstellungen\\Kontrollen_PRS_",stichtag_aktuell,".xlsx"), overwrite= TRUE)
#     
#     
#     output$prs_speichern <- renderText("Das Speichern der Ergebnisse ist abgeschlossen.")
#   })
# 
# 
# observeEvent(input$ausfallrisiko_aufbereiten_berechnen,{
#   
#     m55_srs_cf <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m55_srs_cf_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#       select(-1)
#     m48_prs_cf_abgabe_gesamt <<- read_csv2(file = paste0(paste0(.pfad_prs,"//m48_prs_cf_abgabe_gesamt_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#       select(-1)
#     m56_abgabe_becf_nach_erw_ausf <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m56_abgabe_becf_nach_erw_ausf_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#       select(-1)
#     
#     source("./Module/m17_fkt_prop_anteile_retro.R")
#     source("./Module/m67_zinskurve_aufbereiten.R", encoding = "utf8")
#     source("./Module/m53_diskontfaktoren_aufbereiten.R", encoding = "utf8")
#     source("./Module/m19_fuv_aufb_retro_aufteilung.R", encoding = "utf8")
#     source("./Module/m46_ausfallrisiko.R", encoding = "utf8")
#     source("./Module/m34_leis_aufbereiten.R", encoding = "utf8")
#     source("./Module/m21_schnittstelle_krb.R", encoding = "utf8")
#     source("./Module/rshiny_06_ausfallrisiko_kontrollen.R")
#     
#     output$ausfallrisiko_ist_berechnet <- renderText("Die Berechnung des Ausfallrisikos ist abgeschlossen.")
# 
#     tabelle_kontrollen_ausfallrisiko <- kontrollen_ausfallrisiko()
#     
#     if(nrow(tabelle_kontrollen_ausfallrisiko %>% filter(Status == FALSE))==0)
#       output$kontrollen_ausfallrisiko_alles_auf_true <- renderText("Alle Kontrollen sind auf TRUE.")
# 
#     z <- 0
#     
#     if(file.exists(paste0(pfad_berechnungsordner,"\\05 Ausfallrisiko\\Kontrollen_Ausfallrisiko.xlsx")))
#     {
#       z <- 1
#       kontrolle_ausfallrisiko_alt <- read.xlsx(paste0(pfad_berechnungsordner,"\\05 Ausfallrisiko\\Kontrollen_Ausfallrisiko.xlsx"))
#       validierung_kontrollen_ausfallrisiko <<- tabelle_kontrollen_ausfallrisiko %>%
#         left_join(kontrolle_ausfallrisiko_alt %>%
#                     select(Name_der_Kontrolle, Kommentar), by=c("Name_der_Kontrolle")) %>%
#         mutate(Kommentar = if_else(is.na(Kommentar), "", Kommentar))
#      }
#     else{
#       validierung_kontrollen_ausfallrisiko <<- tabelle_kontrollen_ausfallrisiko %>% mutate(Kommentar = "")
#     }
#     
#     write.xlsx(validierung_kontrollen_ausfallrisiko,paste0(pfad_berechnungsordner,"\\05 Ausfallrisiko\\Kontrollen_Ausfallrisiko.xlsx"), overwrite= TRUE)
#     
#     validierung_kontrollen_ausfallrisiko_filtern <<- validierung_kontrollen_ausfallrisiko %>% filter(Status == FALSE)
#     
#     output$validierung_kontrollen_ausfallrisiko_anzeige <-  renderDT(validierung_kontrollen_ausfallrisiko_filtern, 
#                                                              options = list(language = list(url = "German.json"),
#                                                                             pageLength = 15),
#                                                              rownames = FALSE,
#                                                              editable = TRUE)
#     
#     output$kontrollen_ausfallrisiko_gespeichert <- renderText(paste0("Die Kontrollen wurden unter ",paste0(paste0(pfad_berechnungsordner,"\\05 Ausfallrisiko\\Kontrollen_Ausfallrisiko.xlsx")," gespeichert.")))
#     
#   })
# 
# 
# observeEvent(input$validierung_kontrollen_ausfallrisiko_anzeige_cell_edit, {
#   
#   validierung_kontrollen_ausfallrisiko_filtern[input$validierung_kontrollen_ausfallrisiko_anzeige_cell_edit$row, 
#                                      input$validierung_kontrollen_ausfallrisiko_anzeige_cell_edit$col + 1] <<- input$validierung_kontrollen_ausfallrisiko_anzeige_cell_edit$value
#   
#   validierung_kontrollen_ausfallrisiko <<- validierung_kontrollen_ausfallrisiko %>%
#     left_join(validierung_kontrollen_ausfallrisiko_filtern %>% 
#                 select(Name_der_Kontrolle, Kommentar) %>%
#                 rename(Kommentar_neu = Kommentar), by=c("Name_der_Kontrolle")) %>%
#     mutate(Kommentar = if_else(is.na(Kommentar_neu), Kommentar, Kommentar_neu)) %>%
#     select(-Kommentar_neu)
#   
#   write.xlsx(validierung_kontrollen_ausfallrisiko,paste0(pfad_berechnungsordner,"\\05 Ausfallrisiko\\Kontrollen_Ausfallrisiko.xlsx"), overwrite= TRUE)
#   
# })
# 
# 
# observeEvent(input$ausfallrisiko_ergebnisse_speichern, {
#   write.csv2(m46_lgd_berechnen, file = paste0(paste0(.pfad_ausfallrisiko,"//m46_lgd_berechnen_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
# 
#   output$ausfallrisiko_speichern <- renderText("Das Speichern der Ergebnisse ist abgeschlossen.")
# })
# 
# 
# observeEvent(input$stornorisiko_aufbereiten_berechnen,{
#    
#   m49_prs_cf_uebernahme_gesamt <<- read_csv2(file = paste0(paste0(.pfad_prs,"//m49_prs_cf_uebernahme_gesamt_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   
#    source("./Module/m53_diskontfaktoren_aufbereiten.R")
#    source("./Module/m61_stornorisiko.R", encoding = "utf8")
#   
#   output$stornorisiko_ist_berechnet <- renderText("Die Berechnung des Stornorisikos ist abgeschlossen.")
#   
# })
# 
# observeEvent(input$stornorisiko_ergebnisse_speichern, {
#   write.csv2(m61_stornorisiko_gesamt, file = paste0(paste0(.pfad_stornorisiko,"//m61_stornorisiko_gesamt_"),paste0(stichtag_aktuell,".csv")), quote = TRUE)
#   
#   output$stornorisiko_speichern <- renderText("Das Speichern der Ergebnisse ist abgeschlossen.")
# })
# 
# observeEvent(input$schnittstellen_befuellen, {
#   
#   m40_fp_future_ueb_vtrg <<- read_csv2(file = paste0(paste0(.pfad_pr,"//m40_fp_future_ueb_vtrg_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m43_fp_future_nach_retro <<- read_csv2(file = paste0(paste0(.pfad_pr,"//m43_fp_future_nach_retro_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m39_ps_abgabe_nach_retro <<- read_csv2(file = paste0(paste0(.pfad_pr,"//m39_ps_abgabe_nach_retro_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m37_ps_uebernahme_vtrg <<- read_csv2(file = paste0(paste0(.pfad_pr,"//m37_ps_uebernahme_vtrg_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m44_p_last_nach_retro <<- read_csv2(file = paste0(paste0(.pfad_pr,"//m44_p_last_nach_retro_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m42_input_verd_praemie_ueb <<- read_csv2(file = paste0(paste0(.pfad_pr,"//m42_input_verd_praemie_ueb_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   
#   m55_srs_cf <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m55_srs_cf_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m56_abgabe_becf_nach_erw_ausf <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m56_abgabe_becf_nach_erw_ausf_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m19_fuv_srs_abgabe <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m19_fuv_srs_abgabe_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m19_fuv_srs_uebernahme <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m19_fuv_srs_uebernahme_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m53_diskontfaktoren <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m53_diskontfaktoren_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
# 
#   m48_prs_cf_abgabe_gesamt_lieferung <<- read_csv2(file = paste0(paste0(.pfad_prs,"//m48_prs_cf_abgabe_gesamt_lieferung_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   m49_prs_cf_uebernahme_gesamt_lieferung <<- read_csv2(file = paste0(paste0(.pfad_prs,"//m49_prs_cf_uebernahme_gesamt_lieferung_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
# 
#   m61_stornorisiko_gesamt <<- read_csv2(file = paste0(paste0(.pfad_stornorisiko,"//m61_stornorisiko_gesamt_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
# 
#   m46_lgd_berechnen <<- read_csv2(file = paste0(paste0(.pfad_ausfallrisiko,"//m46_lgd_berechnen_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#     select(-1)
#   ss_befuellen <<- TRUE
#   source("./Module/m17_fkt_prop_anteile_retro.R")
#   source("./Module/m34_leis_aufbereiten.R", encoding= "utf8")
#   source("./Module/m60_geb_praemie_retro_auft.R", encoding = "utf8")
#   source("./Module/m50_schnittstelle_zak.R", encoding = "utf8")
#   source("./Module/m52_schnittstelle_rh.R", encoding = "utf8")
#   source("./Module/m62_schnittstelle_rec_rvv_u_fh.R", encoding = "utf8")
#   source("./Module/m64_stresstest.R", encoding = "utf8")
#   source("./Module/m65_qrt_rh_schnittstelle_final.R", encoding = "utf8")
#   source("./Module/rshiny_07_schnittstellen_kontrollen.R", encoding = "utf8")
#   
#   write.xlsx(m50_pr_risiko_schnittstelle, file = paste0(paste0(.pfad_pr,"//m50_pr_risiko_schnittstelle_"),paste0(stichtag_aktuell,".xlsx")), overwrite = TRUE)
#   
#   output$schnittstellen_befuellt <- renderText("Die Schnittstellen Dateien sind erzeugt worden.")
#   
#   tabelle_kontrollen_schnittstellen <- kontrollen_schnittstellen()
#   
#   if(nrow(tabelle_kontrollen_schnittstellen %>% filter(Status == FALSE))==0)
#     output$kontrollen_schnittstellen_alles_auf_true <- renderText("Alle Kontrollen sind auf TRUE.")
#   
#   z <- 0
#   
#   if(file.exists(paste0(pfad_berechnungsordner,"\\07 Schnittstellen\\Kontrollen_Schnittstellen.xlsx")))
#   {
#     z <- 1
#     kontrolle_schnittstellen_alt <- read.xlsx(paste0(pfad_berechnungsordner,"\\07 Schnittstellen\\Kontrollen_Schnittstellen.xlsx"))
#     validierung_kontrollen_schnittstellen <<- tabelle_kontrollen_schnittstellen %>%
#       left_join(kontrolle_schnittstellen_alt %>%
#                   select(Name_der_Kontrolle, Kommentar), by=c("Name_der_Kontrolle")) %>%
#       mutate(Kommentar = if_else(is.na(Kommentar), "", Kommentar))
#     }
#   else{
#     validierung_kontrollen_schnittstellen <<- tabelle_kontrollen_schnittstellen %>% mutate(Kommentar = "")
#      }
#   
#   write.xlsx(validierung_kontrollen_schnittstellen,paste0(pfad_berechnungsordner,"\\07 Schnittstellen\\Kontrollen_Schnittstellen.xlsx"), overwrite= TRUE)
#   
#   validierung_kontrollen_schnittstellen_filtern <<- validierung_kontrollen_schnittstellen %>% filter(Status == FALSE)
#   
#   output$validierung_kontrollen_schnittstellen_anzeige <-  renderDT(validierung_kontrollen_schnittstellen_filtern, 
#                                                            options = list(language = list(url = "German.json"),
#                                                                           pageLength = 15),
#                                                            rownames = FALSE,
#                                                            editable = TRUE)
#   
#   output$kontrollen_schnittstellen_gespeichert <- renderText(paste0("Die Kontrollen wurden unter ",paste0(paste0(pfad_berechnungsordner,"\\07 Schnittstellen\\Kontrollen_Schnittstellen.xlsx")," gespeichert.")))
#   
# })
# 
# 
# observeEvent(input$validierung_kontrollen_schnittstellen_anzeige_cell_edit, {
#   
#   validierung_kontrollen_schnittstellen_filtern[input$validierung_kontrollen_schnittstellen_anzeige_cell_edit$row, 
#                                                input$validierung_kontrollen_schnittstellen_anzeige_cell_edit$col + 1] <<- input$validierung_kontrollen_schnittstellen_anzeige_cell_edit$value
#   
#   validierung_kontrollen_schnittstellen <<- validierung_kontrollen_schnittstellen %>%
#     left_join(validierung_kontrollen_schnittstellen_filtern %>% 
#                 select(Name_der_Kontrolle, Kommentar) %>%
#                 rename(Kommentar_neu = Kommentar), by=c("Name_der_Kontrolle")) %>%
#     mutate(Kommentar = if_else(is.na(Kommentar_neu), Kommentar, Kommentar_neu)) %>%
#     select(-Kommentar_neu)
#   
#   write.xlsx(validierung_kontrollen_schnittstellen,paste0(pfad_berechnungsordner,"\\07 Schnittstellen\\Kontrollen_Schnittstellen.xlsx"), overwrite= TRUE)
#   
# })
# 
# observeEvent(input$input_vertraege_mit_rv_abfragen, {
#   
#   source("./Module/m68_input_vertraege_mit_rv.R", encoding = "utf8")
#   
#   output$input_vertraege_mit_rv_abgefragt <- renderText("Die Tabelle Input_Vertraege_mit_RV ist abgefragt worden und liegt im Ordner 'Ausweis aller Gegenparteien'")
# })
# 
# 
# 
# observeEvent(input$ausweis_gegenparteien_inputs_einlesen, {
#    
#      m70_input_vertraege_mit_rv_prs <<- read.xlsx(paste0(.pfad_ausweis_aller_gegenparteien,"\\Input_Makleraufteilung_PRS_",stichtag_aktuell,".xlsx"), sheet = "Ausschnitt Tabelle Vertraege") %>%
#                                           left_join(m32_input_firma_ges, by=c("firma")) %>%
#                                           rename(zedent_nr = gesellschaft)
#      
#      m68_input_vertraege_mit_rv <<- read.xlsx(paste0(.pfad_ausweis_aller_gegenparteien,"\\Input_Vertraege_mit_RV_",stichtag_aktuell,".xlsx"), sheet = "M68 Input Vertraege mit RV")
#        
#      m55_srs_cf <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m55_srs_cf_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#        select(-1)
#      
#      m56_cf_erw_ausf <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m56_cf_erw_ausf_"),paste0(stichtag_aktuell,".csv")), col_names  = TRUE, locale = locale("de", encoding="latin1")) %>%
#        select(-1)
#      
#      m19_fuv_srs_abgabe <<- read_csv2(file = paste0(paste0(.pfad_srs,"//m19_fuv_srs_abgabe_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#        select(-1)
#      
#      m48_prs_cf_abgabe <<- read_csv2(file = paste0(paste0(.pfad_prs,"//m48_prs_cf_abgabe_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#        select(-1)
#      
#      m31_cf_erw_ausf <<- read_csv2(file = paste0(paste0(.pfad_prs,"//m31_cf_erw_ausf_"),paste0(stichtag_aktuell,".csv")), col_names = TRUE, locale = locale("de", encoding="latin1")) %>%
#        select(-1)
#      
#      output$ausweis_gegenparteien_inputs_sind_eingelesen <- renderText("Die Input Tabellen sind eingelesen.")
#    
#   })
# 
# observeEvent(input$ausweis_gegenparteien_berechnen, {
#   
#   source("./Module/m53_diskontfaktoren_aufbereiten.R")
#   source("./Module/m69_ausweis_aller_gegenparteien.R", encoding= "utf8")
#   source("./Module/rshiny_08_ausweis_aller_gegenparteien_kontrollen.R", encoding = "utf8")
#   
#   output$ausweis_gegenparteien_berechnet <- renderText("Die Makleraufteilung wurde berechnet und ist im Ordner 'Ausweis aller Gegenparteien' in der Excel-Datei 'Makleraufteilung_31.12.JJJJ.xlsx' abgespeichert.")
#   
#   tabelle_kontrollen_ausweis_aller_gegenparteien <- kontrollen_ausweis_aller_gegenparteien()
#   
#   if(nrow(tabelle_kontrollen_ausweis_aller_gegenparteien %>% filter(Status == FALSE))==0)
#     output$kontrollen_ausweis_aller_gegenparteien_alles_auf_true <- renderText("Alle Kontrollen sind auf TRUE.")
#   
#   z <- 0
#   
#   if(file.exists(paste0(.pfad_ausweis_aller_gegenparteien,"\\Kontrollen_Ausweis_aller_Gegenparteien_",stichtag_aktuell,".xlsx")))
#   {
#     z <- 1
#     kontrolle_ausweis_aller_gegenparteien_alt <- read.xlsx(paste0(.pfad_ausweis_aller_gegenparteien,"\\Kontrollen_Ausweis_aller_Gegenparteien_",stichtag_aktuell,".xlsx"))
#     validierung_kontrollen_ausweis_aller_gegenparteien <<- tabelle_kontrollen_ausweis_aller_gegenparteien %>%
#       left_join(kontrolle_ausweis_aller_gegenparteien_alt %>%
#                   select(Name_der_Kontrolle, Kommentar), by=c("Name_der_Kontrolle")) %>%
#       mutate(Kommentar = if_else(is.na(Kommentar), "", Kommentar))
#   }
#   else{
#     validierung_kontrollen_ausweis_aller_gegenparteien <<- tabelle_kontrollen_ausweis_aller_gegenparteien %>% mutate(Kommentar = "")
#   }
#   
#   write.xlsx(validierung_kontrollen_ausweis_aller_gegenparteien,paste0(.pfad_ausweis_aller_gegenparteien,"\\Kontrollen_Ausweis_aller_Gegenparteien_",stichtag_aktuell,".xlsx"), overwrite= TRUE)
#   
#   validierung_kontrollen_ausweis_aller_gegenparteien_filtern <<- validierung_kontrollen_ausweis_aller_gegenparteien %>% filter(Status == FALSE)
#   
#   output$validierung_kontrollen_ausweis_aller_gegenparteien_anzeige <-  renderDT(validierung_kontrollen_ausweis_aller_gegenparteien_filtern, 
#                                                                     options = list(language = list(url = "German.json"),
#                                                                                    pageLength = 15),
#                                                                     rownames = FALSE,
#                                                                     editable = TRUE)
#   
#   output$kontrollen_ausweis_aller_gegenparteien_gespeichert <- renderText(paste0("Die Kontrollen wurden unter ",paste0(.pfad_ausweis_aller_gegenparteien,"\\Kontrollen_Ausweis_aller_Gegenparteien_",stichtag_aktuell,".xlsx")," gespeichert."))
#   
# })
# 
# 
# observeEvent(input$validierung_kontrollen_ausweis_aller_gegenparteien_anzeige_cell_edit, {
#   
#   validierung_kontrollen_ausweis_aller_gegenparteien_filtern[input$validierung_kontrollen_ausweis_aller_gegenparteien_anzeige_cell_edit$row, 
#                                                 input$validierung_kontrollen_ausweis_aller_gegenparteien_anzeige_cell_edit$col + 1] <<- input$validierung_kontrollen_ausweis_aller_gegenparteien_anzeige_cell_edit$value
#   
#   validierung_kontrollen_ausweis_aller_gegenparteien <<-validierung_kontrollen_ausweis_aller_gegenparteien %>%
#     left_join(validierung_kontrollen_ausweis_aller_gegenparteien_filtern %>% 
#                 select(Name_der_Kontrolle, Kommentar) %>%
#                 rename(Kommentar_neu = Kommentar), by=c("Name_der_Kontrolle")) %>%
#     mutate(Kommentar = if_else(is.na(Kommentar_neu), Kommentar, Kommentar_neu)) %>%
#     select(-Kommentar_neu)
#   
#   write.xlsx(validierung_kontrollen_ausweis_aller_gegenparteien,paste0(.pfad_ausweis_aller_gegenparteien,"\\Kontrollen_Ausweis_aller_Gegenparteien_",stichtag_aktuell,".xlsx"), overwrite= TRUE)
#   
# })


  
}