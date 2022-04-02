
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************
# Tab-Definitionen
#****************************************************************************************************************************************************
#****************************************************************************************************************************************************


#****************************************************************************************************************************************************
# Dropdown Menue ----
#****************************************************************************************************************************************************





dropdownButton <<- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}





#****************************************************************************************************************************************************
# Dashboard Über ----
#****************************************************************************************************************************************************

# .tab_about <- tabItem(tabName = "anleitung",
#                       h2("Hinweise zur Durchführung der Planung/Hochrechnung"),
#                       br(),
#                       fluidRow(
#                         column(width = 8,
#                                p("In der Datei"),
#                                strong("'Arbeitshinweise SII- und ICAAP-Quartals- und Jahresberechnung'"), 
#                                p("ist der genaue Ablauf der Berechnung dokumentiert.")
#                         )
#                         ),
#                       br(),
#                       fluidRow(
#                         column(width = 8,
#                                p("Die Excel-Datei"),
#                                    strong("'Checkliste SII- und ICAAP-Berechnung'"), 
#                                    p("aus dem Unterordner '09 Details Berechnung' dient ebenfalls als Orientierungshilfe und ist während der Berechnung zu befüllen.")
#                         )
#                       )
#                       
# )

#****************************************************************************************************************************************************
# Buchungen erstellen ----
#****************************************************************************************************************************************************

.tab_buchungen_erstellen<- tabItem(
  tabName = "buchungen_erstellen",
  fluidPage(
    fluidRow(
      #h2("Buchungen erstellen"),
      column(width = 3,
             dateInput("buchung_erstellen_tag",p("Tag der Buchung"), value = today()),
             numericInput("buchung_erstellen_betrag",p("Betrag der Buchung"), value = "")
      ),
      column(width = 6,
             textInput("buchung_erstellen_label",p("Label der Buchung"), value = "")
             ),
      column(width = 3,
             selectInput("buchung_erstellen_datenbank_1",p("Datenbank 1"),choices = c("","ungeplante Umsätze","Nachhilfe","Auto","Konto","Wohnung","Urlaub/Freizeit")),
             selectInput("buchung_erstellen_datenbank_2",p("Datenbank 2"),choices = c("","ungeplante Umsätze","Nachhilfe","Auto","Konto","Wohnung","Urlaub/Freizeit"))
             )
    )
  ),
  fluidPage(
    fluidRow(
      box(title = "Buchungen der letzten Zeit",
          width = 18,
          # tableOutput("buchungen_erzeugen_table_buchungen")
          dataTableOutput("buchungen_erzeugen_table_buchungen")
      )
    )
  )
)





#****************************************************************************************************************************************************

#****************************************************************************************************************************************************
# Dashboard Input Daten ----
#****************************************************************************************************************************************************



# stichtag_daten = ymd(paste(year(today()),month(today()),"1", sep = "-"))-1
# stichtag = ymd(paste(year(today())-1,"-12-31", sep = ""))
# plandatum = rollforward(today())


.tab_inputs_generieren <- tabItem(tabName = "inputs_generieren",
                      h2("Inputs generieren"),
                      br(),
                      p("   Aussage der Stichtage"),
                      p("   1. Datenstand für SQL Abfragen"),
                      p("   2. Stichtag aus Sicht dem die Planung betrachtet wird"),
                      p("   3. Stichtag für den die Planung durchgeführt wird (OPL/SPL)"),
                      p("   4. Planungsversion strategische Planung (SPL) oder operative Planung (OPL)"),
                      p("   5. Name des Excel Cokpits in dem Vorberechnungen durchgeführt wurden"),
                      p("   6. Schema auf dem die SQL Abfragen durchgeführt werden sollen"),
                      br(),
                      br(),
                      fluidRow(
                        column(width = 3,
                               dateInput("stichtag_daten", p("Stichtag des Datenstandes"), value = rollforward(ymd(paste(year(today()),month(today()),"01", sep = "-"))) ) # ymd(paste(year(today()),"12","31", sep = "-"))
                               
                        ),
                        column(width = 3,
                               dateInput("stichtag", p("Stichtag der Berechnung"), value = rollforward(ymd(paste(year(today()),month(today()),"01", sep = "-"))) ) # ymd(paste(year(today())-1,"12","31", sep = "-"))
                      ),
                      column(width = 3,
                             dateInput("plandatum", p("Stichtag der Planung"), value = ymd(paste(year(today()),"12","31", sep = "-")) ) # rollforward(ymd(paste(year(today()),month(today()),"01", sep = "-")))
                      )
                      ),
                      fluidRow(
                        column(width = 3,
                               selectInput("Pversion",p("Planungsversion"), choices = c("OPL","SPL"))
                      ),
                      column(width = 3,
                             textInput("name_Cockpit", p("Name Excel Cockpit"), value="Cockpit_Planung_v5_8.xlsm")
                      ),
                      column(width = 3,
                             textInput("schema", p("Schema Abfragen"), value="daisy")
                      )
                      ),
                      br(),
                      fluidRow(
                        
                                box(title = "Pfade auswählen",
                                    column(width = 6,
                                          actionButton("pfad_inputs","Berechnungsordner auswählen"),
                                          br(),
                                          textOutput("pfad_berechnungsordner")
                                          ),
                                    column(width = 6,
                                           actionButton("pfad_R_code","Pfad R Scripte auswählen"),
                                           br(),
                                           textOutput("pfad_R_code")
                                    ),
                                    br(),
                                    br(),
                                    column(width = 6,
                                           actionButton("pfad_R_lib","Pfad R Library auswählen"),
                                           br(),
                                           textOutput("pfad_R_lib")
                                    )
                                   
                               )),
                      br(),
                      br(),
                      
                      fluidRow(box(
                        title = "DSS Verbindung",
                        column(width = 3,
                               textInput("xv_nummer","xv-Nummer", value = if(substr(systemUsername(),1,2)=="XV"){paste("xv",substr(systemUsername(),3,nchar(systemUsername())),sep = "")}else{systemUsername()}       )),
                        column(width = 3,
                               passwordInput("password","Passwort")),
                        column(width = 3,
                               actionButton("einloggen_dss","Verbindung DSS aufbauen")),
                        column(width = 3,
                               selectInput("PROD_TEST","Datenbankwahl",choices = c("TEST","PROD"))),
                        br(),
                        textOutput("verbindung_dss")
                      ))
                      
                      
                      
                      
              )

#****************************************************************************************************************************************************
# Bewertung LIC (PAA) ----
#****************************************************************************************************************************************************

.tab_bewertung_lic_paa <- tabItem(tabName = "bewertung_lic_paa",
                               h2("Bewertung LIC (PAA)"),
                               br(),
                               br(),
                               fluidRow(box(title = "Validierungs Datei Faktoren erstellen",
                                 column(width = 3,
                                        actionButton("r_code_LIC_I","Validierungsdatei für LIC Datenerstellen", icon = icon("earlybirds", class = NULL, lib = "font-awesome"))
                                        ),
                                        br(),
                                 column(width = 3,
                                        br(),
                                        textOutput("validierung_planung_lic")
                                 ))),
                               fluidRow(box(title = "Cashflows LIC (PAA)",
                                 column(width = 3,
                                        actionButton("r_code_LIC_II","Cashflows LIC erzeugen",icon = icon("earlybirds", class = NULL, lib = "font-awesome"))),
                                 br(),
                               column(width = 3,
                                      br(),
                                      textOutput("r_code_LIC_II")
                               )
                              
                               )))


#****************************************************************************************************************************************************
# Bewertung LRC (PAA) ----
#****************************************************************************************************************************************************

.tab_bewertung_lrc_paa <- tabItem(tabName = "bewertung_lrc_paa",
                                  h2("Bewertung LRC (PAA)"),
                                  br(),
                                  br(),
                                  fluidRow(box(title = "Cashflows LRC (PAA)",
                                           column(width = 3,
                                                  actionButton("r_code_LRC_III","Cashflows LRC (PAA)",icon = icon("earlybirds", class = NULL, lib = "font-awesome"))
                                                  ),
                                           br(),
                                           column(width = 3,
                                                  textOutput("r_code_LRC_III")
                                                  )
                                           )),
                                  br(),
                                  fluidRow(column(width = 3,
                                                  numericInput("faktor_lrc_paa","Faktor LRC (PAA)", value = 0.996))
                                  )
                                  )




#****************************************************************************************************************************************************
# Bilanz und GuV ----
#****************************************************************************************************************************************************

.tab_bilanz_und_guv <- tabItem(tabName = "bilanz_und_guv",
                                  h2("IFRS17 Bilanz und GuV"),
                                  br(),
                                  br(),
                                  fluidRow(box(title = "Bilanz und GuV - Vorläufig",
                                               column(width = 3,
                                                      actionButton("r_code_IV","Bilanz und GuV Daten",icon = icon("earlybirds", class = NULL, lib = "font-awesome"))
                                               ),
                                               br(),
                                               column(width = 3,
                                                      textOutput("r_code_IV")
                                               )
                                  ))
)

#****************************************************************************************************************************************************
# Bilanz und GuV ----
#****************************************************************************************************************************************************

.tab_steuerung<- tabItem(tabName = "steuerung",
                         fluidPage( 
                           fluidRow(box(title = "IFRS 17 Bilanz und GuV",
                             column(
                               width = 2,
                                 dropdownButton(
                                   label = "gesch_art", status = "default", width = 80,
                                   actionButton(inputId = "steuerung_gesch_art_a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
                                   actionButton(inputId = "steuerung_gesch_art_z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
                                   br(),
                                   actionButton(inputId = "steuerung_gesch_art_all", label = "(Un)select all"),
                                   checkboxGroupInput(inputId = "steuerung_gesch_art", label = "Choose", choices = c(20,30,40), selected = c(20,30,40))
                                 )),
                               column(
                                 width = 2,
                                 dropdownButton(
                                   label = "prop_nonprop", status = "default", width = 80,
                                   actionButton(inputId = "steuerung_prop_nonprop_a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
                                   actionButton(inputId = "steuerung_prop_nonprop_z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
                                   br(),
                                   actionButton(inputId = "steuerung_prop_nonprop_all", label = "(Un)select all"),
                                   checkboxGroupInput(inputId = "steuerung_prop_nonprop", label = "Choose", choices = c("prop_nonprop","prop"), selected = c("prop_nonprop","prop"))
                                 )),
                             column(
                               width = 2,
                               dropdownButton(
                                 label = "fak_obl", status = "default", width = 80,
                                 actionButton(inputId = "steuerung_fak_obl_a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
                                 actionButton(inputId = "steuerung_fak_obl_z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
                                 br(),
                                 actionButton(inputId = "steuerung_fak_obl_all", label = "(Un)select all"),
                                 checkboxGroupInput(inputId = "steuerung_fak_obl", label = "Choose", choices = c("fak","obl"), selected = c("fak","obl"))
                               )),
                             column(
                               width = 2,
                               dropdownButton(
                                 label = "gsch_bil", status = "default", width = 80,
                                 actionButton(inputId = "steuerung_gsch_bil_a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
                                 actionButton(inputId = "steuerung_gsch_bil_z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
                                 br(),
                                 actionButton(inputId = "steuerung_gsch_bil_all", label = "(Un)select all"),
                                 checkboxGroupInput(inputId = "steuerung_gsch_bil", label = "Choose", choices = c("001","010","023","025","040","052","325","531"), selected = c("001","010","023","025","040","052","325","531"))
                               )),
                             column(
                               width = 2,
                               dropdownButton(
                                 label = "gsch_par", status = "default", width = 80,
                                 actionButton(inputId = "steuerung_gsch_par_a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
                                 actionButton(inputId = "steuerung_gsch_par_z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
                                 br(),
                                 actionButton(inputId = "steuerung_gsch_par_all", label = "(Un)select all"),
                                 checkboxGroupInput(inputId = "steuerung_gsch_par", label = "Choose", choices = c("001","010","023","025","040","052","325","531","ext"), selected = c("001","010","023","025","040","052","325","531","ext"))
                               )),
                             column(
                               width = 2,
                               dropdownButton(
                               label = "BE_RA", status = "default", width = 80,
                               actionButton(inputId = "steuerung_BE_RA_a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
                               actionButton(inputId = "steuerung_BE_RA_z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
                               br(),
                               actionButton(inputId = "steuerung_BE_RA_all", label = "(Un)select all"),
                               checkboxGroupInput(inputId = "steuerung_BE_RA", label = "Choose", choices = c("BE","RA"), selected = c("BE","RA"))
                               )),
                          br(),
                          br(),
                          br(),
                          column(width = 3,
                                 actionButton(inputId = "steuerung_bilanz_erstellen","Filter auswerten",icon = icon("earlybirds", class = NULL, lib = "font-awesome")))
                           )
                         )
        ),
        fluidPage(box(title = "Ergebnisse", width = 10,
                      fluidRow(
                        column(width = 10,
                            # gt_output("steuerung_bilanz_table")
                            tableOutput("steuerung_bilanz_table")
                            # plotOutput("steuerung_bilanz_table")
                          )
                        ) 
                      )
                  )
        
        
        
)



#****************************************************************************************************************************************************
# Output PlaTo Tool ----
#****************************************************************************************************************************************************


.tab_plato <- tabItem(tabName = "output_plato",
                      fluidRow(
                        box(
                          title = "Inputparameter für csv Dateien PlaTo Tool",
                          textInput("name_vorlage","Name Datei Variablen Vorlage",value = "Vorlage_PlaTo_CSV.xlsx"),
                          textInput("sheet_vorlage","Sheet in dem die Vorlage steht",value = "Vorlage_PlaTo")
                        )
                      ),
                      
                      
                        fluidRow(
                          box(
                            title = "Output CSV Dateien für PlaTo Tool erzeugen",
                            column(
                              width = 3,
                              actionButton("r_code_V","CSV Dateien erzeugen",icon = icon("earlybirds", class = NULL, lib = "font-awesome"))
                            ),
                            br(),
                            column(width = 3,
                                   textOutput("r_code_V")
                                   )
                          )
                        )
                      )













