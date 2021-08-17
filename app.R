
# Ideas: Add distribution plots with solid line for result and table with averages and result

library(shiny)
library(dplyr)
library(plotly)
library (googlesheets4)
library(Dict)
library(gridExtra)

# Set up and read google sheet

## get your token to access google drive
shiny_token <- gs4_auth('correlaid2@gmail.com')
saveRDS(shiny_token, "shiny_app_token.rds")

## read data
base_df <- as.data.frame(read_sheet(ss = '10YlWyJzaDFer6rqKFTAYkXLuhcC9JgynSRmSUa5AnUk', sheet = "Daten"))

## convert to numeric
base_df[3:length(base_df)] <- lapply(base_df[3:length(base_df)], function(x) as.numeric(x) )

## calculate dimensions
advanced_df <- base_df %>%
    mutate("inhaltlich" = rowMeans(base_df[3:6], na.rm =TRUE)) %>%
    mutate("systemisch" = rowMeans(base_df[7:10], na.rm = TRUE)) %>%
    mutate("rechtlich" = rowMeans(base_df[11:14], na.rm = TRUE)) %>%
    mutate("organisatorisch" = rowMeans(base_df[15:18], na.rm = TRUE)) %>%
    mutate("gesellschaftlich" = rowMeans(base_df[19:22], na.rm = TRUE))
advanced_df[23:length(advanced_df)] <- lapply(advanced_df[23:length(advanced_df)], function(x) round(x,2) )

## get means for viz
final_values <- colMeans(advanced_df[3:length(advanced_df)])



# Set up data maturity model

## Dateninhalt (Inhaltlich)

### Relevanz und Vollständigkeit
q1_relevant <- Dict$new(
    "A) Zwischen tatsächlicher Datenlage und Datenbedarf liegt eine hohe Diskrepanz vor" = 1, 
    "B) Einige Datenpunkte sind relevant aber zentrale Datendimensionen fehlen" = 2, 
    "C) Datenpunkte geben bereits Auskunft über zentrale Fragen der Programmlogik, aber für eine qualifizierte Problemlösung fehlen noch Informationen" = 3,
    "D) Daten geben vollständig Auskunft über relevante Fragestellungen und können gezielt eingesetzt werden, um Lösungsvorschläge zu entwickeln, Programme zu überwachen und diese zu evaluieren" = 4)

### Granularität
q2_granularity <- Dict$new(
    "A) Gesamtheitlich aggregiert (z.B. absolute Teilnehmerzahl)" = 1,
    "B) Pro Gruppe aggregiert (z.B. pro Geschlecht)" = 2,
    "C) Entitätenebene (z.B. pro Person)" = 3,
    "D) Ereignisebene (z.B. eine Person über mehrere Veranstaltungen)" = 4)

### Erhebungsfrequenz
q3_collection <- Dict$new(
    "A) Einmalig" = 1,
    "B) Jährlich" = 2,
    "C) Regelmäßig" = 3,
    "D) Durchgängig" = 4)

### Qualität
q4_quality <- Dict$new(
    "A) Es fehlen Datenpunkte (Individuen oder Einheiten)" = 1,
    "B) Es fehlen Variablen (Spalten)" = 2,
    "C) Es fehlen keine Daten, allerdings finden sich Fehler, die u.a. durch mangelnde Einschränkungen bei der Dateneingabe entstehen" = 3,
    "D) Es fehlen keine Daten und Fehler bei der Dateneingabe werden durch umfassende Maßnahmen beschränkt" = 4)


## Datenverarbeitende Systeme (Systemisch)
### Speicherformat
q5_storage <- Dict$new(
    "A) Papier" = 1,
    "B) PDFs/Bilder" = 2,
    "C) Textdateien (CSV, XML, …)" = 3,
    "D) Datenbanken" = 4)

### Analyse
q6_analysis <- Dict$new(
    "A) Keine oder nur rudimentäre Analysen" = 1,
    "B) Einfache manuelle Analysen in Excel o.ä." = 2,
    "C) Automatisierte Analysen mithilfe externer Software" = 3,
    "D) Anspruchsvolle, selbst programmierten Analysen und Automatisierungen" = 4)

### Zugang
q7_access <- Dict$new(
    "A) Nur in der Anwendung verfügbar, in der die Daten erhoben werden" = 1,
    "B) Daten können in einfachen Formaten (z.B. PDF) extrahiert werden" = 2,
    "C) Daten können in maschinell lesbaren Formaten extrahiert werden (CSV, JSON, XML, Datenbankextrakt)" = 3,
    "D) Alle maschinell lesbaren Formate sind verfügbar. Zudem ist die Datenextraktion über eine API möglich" = 4)

### Integration
q8_integration <- Dict$new(
    "A) Daten sind in individuellen Anwendungen verfügbar" = 1,
    "B) Daten werden extrahiert und anschließend manuell importiert" = 2,
    "C) Zentrales Datawarehouse (Datenspeicherort) mit automatisierter Aggregation und Verknüpfung" = 3,
    "D) Zusätzlich werden externe Daten abgerufen und integriert" = 4)


## Rechtliche Infrastruktur
### DSGVO
q9_dsgvo <- Dict$new(
    "A) Nicht implementiert" = 1,
    "B) Partiell implementiert (z.B. Passwortschutz für sensible Dateien und Systeme)" = 2,
    "C) Weitestgehend implementiert (z.B. zuständige Person beauftragt, Datenverarbeitungsverzeichnis angelegt, erste organisatorische und technische Hürden eingeführt)" = 3,
    "D) Vollständig implementiert (z.B. zert. Datenschutzbeauftragte:n ernannt, Datenverarbeitungsverzeichnis und Maßnahmenplan angelegt, hohe organisatorische und technische Hürden implementiert)" = 4)

### Dokumentation
q10_doku <- Dict$new(
    "A) Keine Dokumentation zu Variablen" = 1,
    "B) Codebuch mit Variablen und Kategorien" = 2,
    "C) Codebuch mit Metadaten und Erhebungsmethoden" = 3,
    "D) Codebuch mit Metadaten, Erhebungsmethoden, Annahmen, Ausschlusskriterien und potenziellen Verzerrungen" = 4)

### Datennutzungsrechte
q11_user <- Dict$new(
    "A) Es gibt kein Konzept zur Datennutzung und zu Zugangsrechten" = 1,
    "B) Es gibt partielle Konzepte zur Datennutzung und zu Zugangsrechten, die sich noch nicht über die gesamte Datenlandschaft erstrecken" = 2,
    "C) Es gibt interne Konzepte für Datennutzung und Zugangsrechte, die in standardisierten Prozessen vergeben werden" = 3,
    "D) Es gibt neben einer internen Datennutzungsstrategie auch Konzepte, ob und wie Daten mit externen Organisationen geteilt werden dürfen" = 4)

### Historie
q12_history <- Dict$new(
    "A) Historische Daten werden gelöscht" = 1,
    "B) Historische Daten werden bei Updates überschrieben" = 2,
    "C) Personenbezogene Daten werden bis zu 3J. nach Erlöschung der Zweckbindung gespeichert und bei Updates aller Datentypen neue Datenpunkte angefügt" = 3,
    "D) Personenbezogene Daten werden 3J. nach Zweckbindungserlöschung aggregiert, Analysen abgespeichert und bei Updates lassen sich Änderungen nachverfolgen" = 4)

## Organisation
### Organisation insgesamt
q13_orga <- Dict$new(
    "A) Arbeitnehmer:innen wissen, dass Daten existieren, aber noch nicht, was diese enthalten und wozu diese genutzt werden können" = 1,
    "B) Daten werden von einzelnen Arbeitnehmer:innen bedarfsorientiert genutzt, um Förderberichte zu erstellen" = 2,
    "C) Daten werden bereits von einigen Abteilungen gezielt genutzt, um Qualitätsmanagement zu betreiben, programmatische Entscheidungen zu treffen, Berichtanforderungen von Fördenden gerecht zu werden und Skalierungen zu planen" = 3,
    "D) Die Organisation verfügt über eine gesamtheitliche Datenstrategie, die sowohl in der Unternehmenskultur verankert ist als auch als Basis für sämtliche Managemententscheidungen dient" = 4)

### Management
q14_mgmt <- Dict$new(
    "A) Dem Management ist der Nutzen von Daten noch unklar" = 1,
    "B) Das Management sieht die Relevanz von Daten insbesondere im Bereich von Förderungen" = 2,
    "C) Das Management überlässt es Abteilungen, inwieweit sie neben den Basisanforderungen der Berichterstattung Daten nutzen und toleriert den Einsatz von kleineren Projektbudgets für solche Vorhaben" = 3,
    "D) Das Management unterstützt Abteilungen bei der Durchführung ihrer Datenerhebungen und -analyse, stellt notwendige Ressourcen bereit und fordert bei programmatischen Entscheidungen datenorientierte Begründungen ein" = 4)

### Programmatisches Personal
q15_pm <- Dict$new(
    "A) Arbeitnehmer:innen, die für die Erhebung von Daten zuständig sind, sehen diese als notwendige Verpflichtung, der sie nur unregelmäßig nachkommen" = 1,
    "B) Arbeitnehmer:innen, die für die Erhebung von Daten zuständig sind, erfüllen diese Anforderungen pflichtbewusst" = 2,
    "C) Arbeitnehmer:innen, die für die Erhebung von Daten zuständig sind, erfüllen diese Anforderungen pflichtbewusst und erhalten in fixierten Zeitabständen Analysen, die für ihre programmatische Arbeit relevant sind" = 3,
    "D) Datenerhebende Arbeitnehmer:innen erhalten durch Analysetools in Echtzeit Einblicke in ihre programmatische Arbeit, aus denen sie Handlungs-empfehlungen ableiten können, und machen eigenständig Vorschläge zur Datenstrategie" = 4)

### Analytisches PersonalAnal
q16_quant <- Dict$new(
    "A) Der Organisation steht kein Personal zur Verfügung, das mit erhobenen Daten arbeiten kann" = 1,
    "B) Ehrenamtliche Mitarbeitende unterstützen die Organisation bei der Nutzung ihrer Daten durch Analysen oder Bereitstellung einiger Infrastruktur" = 2,
    "C) Hauptamtliches Personal übernimmt wichtige Aufgaben der Analyse und ist fähig, genutzte Anwendungen und Tools in Stand zu halten" = 3,
    "D) Hauptamtliches Personal führt Analysen selbstständig durch und managt notwendige Anwendungen und Tools selbst" = 4)

## Gesellschaftliche Einbettung
### Fördernde
q17_funding <- Dict$new(
    "A) Fördernde verlangen lediglich qualitative Berichte und Zahlen zu oberflächlichen Indikatoren" = 1,
    "B) Fördernde verlangen eine Analyse von Indikatoren bis hin zur Outcome-Ebene" = 2,
    "C) Fördernde stellen für tiefgreifende Analysen Mittel bereit" = 3,
    "D) Fördernde setzen Datenorientierung voraus und stellen dafür ausreichend Mittel bereit" = 4)

### Partnerschaften
q18_partners <- Dict$new(
    "A) Es gibt keine Partnerschaften" = 1,
    "B) Partnerschaften existieren lediglich auf programmatischer Ebene" = 2,
    "C) In Partnerschaften werden punktuell Daten geteilt" = 3,
    "D) Der Austausch von Daten und Analysen ist Bestandteil von Partnerschaften" = 4)

### Dienstleitende
q19_extern <- Dict$new(
    "A) Es gibt keinerlei externe Unterstützung" = 1,
    "B) Externe bieten punktuell pro-bono technische/analytische Unterstützung" = 2,
    "C) Externe bieten langfristig pro-bono technische/analytische Unterstützung" = 3,
    "D) Externe haben einen langfristigen vergü-teten Betreuungsauftrag (alt.: alles ist Inhouse)" = 4)

### Speicherformat
q20_education <- Dict$new(
    "A) Es gibt keine Bildungsangebote rund um das Thema Daten/IT" = 1,
    "B) Es wird unterstützt, wenn Mitarbeiten-de sich extern zu Daten/IT fortbilden" = 2,
    "C) Mitarbeitende werden für Daten/IT-Fortbildungen vergütet freigestellt" = 3,
    "D) Mitarbeitende werden für Daten/IT-Fortbildungen vergütet freigestellt und diese bezahlt" = 4)



# User Interface
ui <- fluidPage(

    # Titel
    titlePanel("Datenreifegrad - Wo steht Deine Organisation?"),
    hr(),
    # Layout
    sidebarLayout(
        sidebarPanel(width = 4,
            textOutput('hinweis'),
            hr(),
            
            ## Dateninhalt
            ### Relevanz
            selectInput("relevant",
                        "Dateninhalt - Relevanz und Vollständigkeit",
                        choices = q1_relevant$keys,
                        selected = "A) Zwischen tatsächlicher Datenlage und Datenbedarf liegt eine hohe Diskrepanz vor"
                        ),
            ### Granularität
            selectInput("granularity",
                        "Dateninhalt - Granularität",
                        choices = q2_granularity$keys,
                        selected = "A) Gesamtheitlich aggregiert (z.B. absolute Teilnehmerzahl)"
            ),
            ### Erhebung
            selectInput("collection",
                        "Dateninhalt - Erhebungsfrequenz",
                        choices = q3_collection$keys,
                        selected = "A) Einmalig"
            ),
            
            ### Qualität
            selectInput("quality",
                        "Dateninhalt - Qualität",
                        choices = q4_quality$keys,
                        selected = "A) Es fehlen Datenpunkte (Individuen oder Einheiten)"
            ),
            
            ## Datenverarbeitende Systeme
            selectInput("storage",
                        "Datenverarbeitende Systeme - Speicherung",
                        choices = q5_storage$keys,
                        selected = "A) Papier"
            ),
            
            ### Analyse
            selectInput("analyse",
                        "Datenverarbeitende Systeme - Analyse",
                        choices = q6_analysis$keys,
                        selected = "A) Keine oder nur rudimentäre Analysen"
            ),
            
            ### Speicherung
            selectInput("storage",
                        "Datenverarbeitende Systeme - Zugang",
                        choices = q7_access$keys,
                        selected = "A) Nur in der Anwendung verfügbar, in der die Daten erhoben werden"
            ),
            
            ### Integration
            selectInput("integration",
                        "Datenverarbeitende Systeme - Integration",
                        choices = q8_integration$keys,
                        selected = "A) Daten sind in individuellen Anwendungen verfügbar"
            ),
            
            ### Datenschutz
            selectInput("dsgvo",
                        "Rechtliche Infrastruktur - Allgemeiner Datenschutz",
                        choices = q9_dsgvo$keys,
                        selected = "A) Nicht implementiert"
            ),
            
            ### Dokumentation
            selectInput("doku",
                        "Rechtliche Infrastruktur - Dokumentation von Daten",
                        choices = q10_doku$keys,
                        selected = "A) Keine Dokumentation zu Variablen"
            ),
            
            ### Datennutzungsrechte
            selectInput("user",
                        "Rechtliche Infrastruktur - Datennutzungsrechte",
                        choices = q11_user$keys,
                        selected = "A) Es gibt kein Konzept zur Datennutzung und zu Zugangsrechten"
            ),
            
            ### History
            selectInput("history",
                        "Rechtliche Infrastruktur - Historie",
                        choices = q12_history$keys,
                        selected = "A) Historische Daten werden gelöscht"
            ),
            
            ### Organisation
            selectInput("orga",
                        "Organisation - insgesamt",
                        choices = q13_orga$keys,
                        selected = "A) Arbeitnehmer:innen wissen, dass Daten existieren, aber noch nicht, was diese enthalten und wozu diese genutzt werden können"
            ),
            
            ### Management
            selectInput("mgmt",
                        "Organisation - Management",
                        choices = q14_mgmt$keys,
                        selected = "A) Dem Management ist der Nutzen von Daten noch unklar"
            ),
            
            ### Programmatisches Personal
            selectInput("pm",
                        "Organisation - Programmatisches Personal",
                        choices = q15_pm$keys,
                        selected = "A) Arbeitnehmer:innen, die für die Erhebung von Daten zuständig sind, sehen diese als notwendige Verpflichtung, der sie nur unregelmäßig nachkommen"
            ),
            
            ### Analytisches/technisches Personal
            selectInput("quant",
                        "Organisation - Analytisches/technisches Personal",
                        choices = q16_quant$keys,
                        selected = "A) Der Organisation steht kein Personal zur Verfügung, das mit erhobenen Daten arbeiten kann"
            ),
            
            ### Fördernde
            selectInput("funding",
                        "Gesellschaftliche Einbettung - Fördernde",
                        choices = q17_funding$keys,
                        selected = "A) Fördernde verlangen lediglich qualitative Berichte und Zahlen zu oberflächlichen Indikatoren"
            ),
            
            ### Partnerschaften
            selectInput("partners",
                        "Gesellschaftliche Einbettung - Partnerschaften",
                        choices = q18_partners$keys,
                        selected = "A) Es gibt keine Partnerschaften"
            ),
            
            ### Dienstleistende
            selectInput("external",
                        "Gesellschaftliche Einbettung - Dienstleistende",
                        choices = q19_extern$keys,
                        selected = "A) Es gibt keinerlei externe Unterstützung"
            ),
            
            ### Bildungsangebote
            selectInput("education",
                        "Gesellschaftliche Einbettung - Bildungsangebote",
                        choices = q20_education$keys,
                        selected = "A) Es gibt keine Bildungsangebote rund um das Thema Daten/IT"
            ),
            
            hr(),
            # Textinput Organisation
            textInput('name', 'Gib hier den Namen deiner Organisation an:', value = 'unbekannte Organisation'), 
        
            # Arbeitsbreich auswählen
            selectInput('type', 'In welchem Bereich ist deine Organisation aktiv?', selected = 'Sonstige', 
                        choices = c('Beschäftigung', 'Bildung und Forschung', 'Gesundheit', ' Katastrophenhilfe',
                                    'Kommunale Entwicklung und Wohnen', 'Kunst, Kultur und Sport', 'Migration',
                                    'Philantrophische Intermediaries und Förderung des Ehrenamts', 'Recht, Advocacy und Politik',
                                    'Religion', 'Soziale Dienstleistungen', 'Umwelt', 'Sonstige')),
            
            # Einfügen eines Submit-Buttons
            actionButton("ergebnisse", "Ergebnisse absenden"),
            
            # Einfügen eines Hilfefensters
            actionButton("hilfe", "Hilfe"),
        
            hr(),
            hr(),
        
            # Textoutput Organisation
            textOutput('orga')
            ),
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput('radarplot'),
            plotlyOutput('tabelle')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session){
    
    # Hinweis
    output$orga <- renderText({
        paste('Ergebnisse von ', input$name, ' am ', format(Sys.time(), " %d.%m.%Y"), '.', sep ='')
    })
    
    # Datum und Organisation
    output$hinweis <- renderText({
        paste('Wählt in den folgenden 20 Fragen die Antwortoption aus, die am meisten auf Euch zutrifft')
    })
    
    # Ergebnisse abschicken
    observeEvent(input$ergebnisse, {
        
        # Daten in DF abspeichern
        ergebnisse <- as.data.frame(rbind(c(input$name, input$type,
                                            q1_relevant$get(input$relevant),
                                            q2_granularity$get(input$granularity),
                                            q3_collection$get(input$collection),
                                            q4_quality$get(input$quality),
                                            q5_storage$get(input$storage),
                                            q6_analysis$get(input$analysis),
                                            q7_access$get(input$access),
                                            q8_integration$get(input$integration),
                                            q9_dsgvo$get(input$dsgvo),
                                            q10_doku$get(input$doku),
                                            q11_user$get(input$user),
                                            q12_history$get(input$history),
                                            q13_orga$get(input$orga),
                                            q14_mgmt$get(input$mgmt),
                                            q15_pm$get(input$pm),
                                            q16_quant$get(input$quant),
                                            q17_funding$get(input$funding),
                                            q18_partners$get(input$partners),
                                            q19_extern$get(input$extern),
                                            q20_education$get(input$education)
                                        )))
        sheet_append(ss = '10YlWyJzaDFer6rqKFTAYkXLuhcC9JgynSRmSUa5AnUk', data = ergebnisse, sheet = "Daten")
    })
    
    # Bedienungshilfe
    hilfe_text <- "Bei Anmerkungen oder Fragen wendet Euch an: nina.h@correlaid.org"
    observeEvent(input$hilfe, {
        showModal(modalDialog(hilfe_text, title = "Bedienungshilfe", footer = mYesodalButton("Schließen")))
    })
    
    # Ergebnisse
    ergebnisse <- reactive({
        
        # get answers and value
        a1 <- q1_relevant$get(input$relevant)
        a2 <- q2_granularity$get(input$granularity)
        a3 <- q3_collection$get(input$collection)
        a4 <- q4_quality$get(input$quality)
        a5 <- q5_storage$get(input$storage)
        a6 <- q6_analysis$get(input$analysis)
        a7 <- q7_access$get(input$access)
        a8 <- q8_integration$get(input$integration)
        a9 <- q9_dsgvo$get(input$dsgvo)
        a10 <- q10_doku$get(input$doku)
        a11 <- q11_user$get(input$user)
        a12 <- q12_history$get(input$history)
        a13 <- q13_orga$get(input$orga)
        a14 <- q14_mgmt$get(input$mgmt)
        a15 <- q15_pm$get(input$pm)
        a16 <- q16_quant$get(input$quant)
        a17 <- q17_funding$get(input$funding)
        a18 <- q18_partners$get(input$partners)
        a19 <- q19_extern$get(input$extern)
        a20 <- q20_education$get(input$education)
        
        # calculate values
        inhaltlich <- mean(c(a1, a2, a3, a4))
        systemisch <- mean(c(a5, a6, a7, a8))
        rechtlich <- mean(c(a9, a10, a11, a12))
        organisatorisch <- mean(c(a13, a14, a15, a16))
        gesellschaftlich <- mean(c(a17, a18, a19, a20))
        
        # liste kreiieren
        liste <- list(inhaltlich=inhaltlich, systemisch=systemisch, rechtlich=rechtlich, organisatorisch=organisatorisch)
        
        return(liste)
    })
    
    #Plot
    radarplot <- renderPlotly({
        #make plot
        plot_ly(
            type = 'scatterpolar',
            mode   = 'markers',
            fill = 'toself'
        ) %>%
            # averages
            add_trace(
                r = c(final_values['systemisch'], final_values['inhaltlich'], final_values['organisatorisch'], final_values['gesellschaftlich'], final_values['rechtlich']),
                theta = c('Systemisch','Inhaltlich','Organisatorisch', 'Gesellschaftlich', 'Rechtlich'),
                name = 'Durchschnitt',
                color = I("light gray")
            ) %>%
            # results
            add_trace(
                r = c(ergebnisse()$systemisch, ergebnisse()$inhaltlich, ergebnisse()$organisatorisch, ergebnisse()$gesellschaftlich, ergebnisse()$rechtlich),
                theta = c('Systemisch','Inhaltlich','Organisatorisch', 'Gesellschaftlich', 'Rechtlich'),
                name = 'Dein Ergebnis',
                mode   = 'markers',
                color = I("dodgerblue")
            ) %>%
            # layout
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,4), 
                        angle = 0 #begin of axis
                    )
                )
            )
    })
        
    # Tabelle
    tabelle <- renderPlotly({
        # Tabellenergebnisse
        df <- matrix(c(round(final_values['systemisch'],2), round(final_values['inhaltlich'],2), 
                       round(final_values['organisatorisch'],2), round(final_values['gesellschaftlich'],2), round(final_values['rechtlich'],2),
                       ergebnisse()$systemisch, ergebnisse()$inhaltlich, ergebnisse()$organisatorisch, ergebnisse()$gesellschaftlich, ergebnisse()$rechtlich), 
                     nrow=5, byrow=FALSE) # in DataFrame konvertieren (notwendige für den Grid)
        colnames(df) <- c('Durchschnitt', 'Dein Ergebnis') # Spaltennamen anpassen
        rownames(df) <- c('Systemisch','Inhaltlich','Organisatorisch', 'Gesellschaftlich', 'Rechtlich')
        
        # Tabelle designen
        plot_ly(
            type = 'table',
            columnwidth = c(50, 50, 50),
            columnorder = c(0, 1, 2),
            header = list(
                values = c('Indikator', 'Durchschnitt', 'Dein Ergebnis'),
                align = c("center", "center", "center"),
                line = list(width = 1, color = 'black'),
                fill = list(color = c("white","lightgray", "dodgerblue")),
                font = list(family = "Arial", size = 14, color = "white")
            ),
            cells = list(
                values = rbind(rownames(df), df[,1], df[,2]),
                align = c("center", "center", "center"),
                line = list(color = "black", width = 1),
                font = list(family = "Arial", size = 12, color = c("black"))
            ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
