
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************
# Grafische Nutzeroberfläche
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************


#****************************************************************************************************************************************************
# Einbinden von weiteren R-Skripten (Funktionen) ----
#****************************************************************************************************************************************************

source("tabs.R", encoding = "utf-8") # beinhaltet die Definitionen der einzelnen Reiter
# source("C:\\GitHub\\Planung_Hochrechnung\\Shiny_Projekt\\tabs.R", encoding="utf-8")
# source("Module/start_function_dropdown.R", encoding = "utf-8")

tagList(
  
  #****************************************************************************************************************************************************
  # Einbinden von Funktionalitäten ----
  #****************************************************************************************************************************************************
  
  # Einbinden ShinyFeedback
  useShinyFeedback(feedback = TRUE, toastr = TRUE),
  
  # Einbinden Shinyalert
  useShinyalert(),
  
  # Einbinden Shinyjs
  useShinyjs(),
  
  #****************************************************************************************************************************************************
  # Dashboard Grundgerüst ----
  #****************************************************************************************************************************************************
  
  dashboardPage(title = "Abrrechnungssystem Tim Heininger",
                
                # Kopfzeile
                dashboardHeader(title = div(img(src = "Logo.PNG", height = "25%", width = "25%", style = "margin: 0px 0px"), "Abrrechnungssystem Tim Heininger"),
                                tags$li(a(href = 'https://ruvnet.ruv.de/start/Seiten/default.aspx', img(src = "RuV_RGB_weiss.png", style = "margin: 0 0 0px; height: 50px"),
                                          style = "padding-top:0px; padding-bottom:0px; padding-right:4px;padding-left:0px; margin: 0 0 0px"),
                                        class = "dropdown",
                                        style = "padding-right: 20px,margin-bottom: 0px;")),
                
                # Seiten-Menü
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Buchungen erstellen", tabName = "buchungen_erstellen", icon = icon("book-open", class = NULL, lib = "font-awesome"), selected = TRUE)
                    # menuItem("Hinweise Planung/Hochrechnung (test)", tabName = "anleitung", icon = icon("book-open", class = NULL, lib = "font-awesome"), selected = TRUE),
                    # menuItem("Inputs generieren", tabName = "inputs_generieren", icon = icon("user-edit", class = NULL, lib = "font-awesome")),
                    # menuItem("IFRS 17 - Bewertung LIC (PAA)", tabName = "bewertung_lic_paa", icon = icon("wrench", class = NULL, lib = "font-awesome")),
                    # menuItem("IFRS 17 - Bewertung LRC (PAA)", tabName = "bewertung_lrc_paa", icon = icon("wrench", class = NULL, lib = "font-awesome")),
                    # menuItem("IFRS 17 - Bilanz und GuV", tabName = "bilanz_und_guv", icon = icon("wrench", class = NULL, lib = "font-awesome")),
                    # menuItem("IFRS 17 - Steuerung", tabName = "steuerung", icon = icon("wrench", class = NULL, lib = "font-awesome")),
                    # menuItem("IFRS 17 - CSV Dateien PlaTo", tabName = "output_plato", icon = icon("wrench", class = NULL, lib = "font-awesome"))
                  )
                ),
                
                # Hauptanzeige
                dashboardBody(
                  
                  # Corporate Design einbinden
                  tags$head( 
                    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                  ),
                  # Fixiere Kopf- und Seitenleiste
                  tags$script(HTML("$('body').addClass('fixed');")),
                  # Zeige weiterhin Logos beim Verstecken der Seitenleiste
                  tags$script(HTML("$('body').addClass('sidebar-mini');")),
                  
                  # Inhalt
                  tabItems(
                    .tab_buchungen_erstellen
                    # .tab_about,
                    # .tab_inputs_generieren,
                    # # .tab_inputs_einlesen,
                    # .tab_bewertung_lic_paa,
                    # .tab_bewertung_lrc_paa,
                    # .tab_bilanz_und_guv,
                    # .tab_steuerung,
                    # .tab_plato
                  
                ),
                skin = "blue"
  ))
  
)
