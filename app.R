
library(shiny)
library(leaflet)
library(classInt)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(sp)
library(openxlsx)
library(scales)
library(ggrepel)

geoLleida = readRDS("Shapefiles/Barris_Lleida.Rds")
geoPerfil = readRDS("Shapefiles/Perfil_Lleida.Rds")
geoCat = readRDS("Shapefiles/Cat.Rds")
poblacio = read.xlsx("data.xlsx",
                     sheet = "DataOrigen")

Years_list = unique(poblacio$A)
Barris_list = unique(poblacio$Barri)
dataApp = "27/07/21"


ui <- tagList(
    
    #CSS
    tags$head(
        tags$style(type = "text/css",
                   HTML(
                       "@import url('https://fonts.googleapis.com/css2?family=Lexend:wght@400&display=swap');
                        @import url('https://fonts.googleapis.com/css2?family=Otomanopee+One&display=swap');

                                body {
                                    font-family: 'Lexend', sans-serif;
                                }
                                    
                                .navbar-brand {
                                    font-family: 'Otomanopee One', sans-serif;
                                }
                                h2 {
                                    font-family: 'Otomanopee One', sans-serif;
                                    text-align: center;
                                    padding-bottom: 2%;
                                }
                                
                                a {
                                text-decoration: none;
                                color: #7C83FD;
                                }
                                
                                a:link {
                                text-decoration: none;
                                color: #7C83FD;
                                }
                                
                                a:hover {
                                text-decoration: none;
                                color: #185ADB;
                                font-weight: bold;
                                }
                                
                                a:visited {
                                text-decoration: none;
                                color: #7C83FD;
                                
                                }
                                
                                a:active {
                                text-decoration: none;
                                color: #185ADB;
                                font-weight: bold;
                                
                                }
                                
                                p {
                                
                                font-family: 'Lexend', sans-serif;
                                margin-left: 10%;
                                margin-right: 10%;}
                       
                       .navbar-default .navbar-nav > .active > a:hover {
                                background-color: white;
                                font-weight: bold;
                                color: #0A1931;}
                                
                       .navbar-default .navbar-nav > li > a:hover {
                                background-color: white;
                                font-weight: bold;
                                color: #0A1931;}"
                       
                   ))
        
        ),
    
    ##############################
    
    navbarPage(title = "Població Lleida",
               
               tabPanel(title = "Mapa",
                        icon = icon("map-marked-alt"),
                        
                        sidebarPanel(
                            selectInput("Years",
                                        label = "Any",
                                        choices = Years_list,
                                        selected = max(Years_list)),
                            selectInput("Indicator",
                                        label = "Indicador",
                                        choices = c("Població total"),
                                        selected = "Població total"),
                            
                            radioButtons("SexeInput",
                                         label = "Sexe",
                                         choices = c("Total", "Dona", "Home"),
                                         selected = "Total",
                                         inline = T),
                            h2(verbatimTextOutput("PobTotal"), align = "center"),
                            plotOutput("IndicadorHist",
                                       height = "210"),
                            plotOutput("EdatsDist",
                                       height = "235")
                        ),
                        mainPanel(
                            tags$style(type = "text/css", "#mapa {height: calc(100vh - 80px) !important;}"),
                            leafletOutput("mapa")
                        )
                        
                        ),
               tabPanel(title = "Info",
                        icon = icon("info"),
                        
                        h2("L'aplicació"),
                        p("La següent App Web busca visualitzar les dades de població per barris del municipi de Lleida, unes dades que s'ofereixen de forma oberta al portal de la ",
                          a(href = "https://www.paeria.cat/opendata/ca/index.asp", "Paeria de Lleida", target="_blank")),
                        p("Tot el tauler funciona de forma integrada, és a dir, tan el mapa com els propis filtres serveixen per seleccionar i visualitzar les dades d'interés que apareixeran a les gràfiques. Per exemple, si cliqueu en algun polígon del mapa, les dades que es mostraran seran les pròpies d'aquell barri; per retornar al total, cliqueu en l'àrea fora de la ciutat."),
                        p("Ara com ara, l'únic indicador que s'ofereix és el recompte de la població total i la distribució per sexes. En un futur s'afegiran d'altres indicadors demogràfics"),
                        p("Les dades mostren algunes incoherències: (1) les xifres no coincideixen amb les que ofereixen altres portals com IDESCAT o INE i (2) els grups d'edat són dispars entre els anys."),
                        h2("L'autor"),
                        p("Hola! Em dic Marc i si voleu podeu seguir-me per ",
                          a(href = "https://twitter.com/marc_coca", "Twitter ", target="_blank"),
                          "on comparteixo aplicacions com aquestes i visualitzacions."),
                        h2("Fonts"),
                        p("· ", 
                          a(href = "https://www.paeria.cat/opendata/ca/index.asp", "Paeria de Lleida", target="_blank")),
                        p("· Última data d'actualització: ",
                          dataApp),
                        h2("Nota"),
                        p("Aquesta aplicació, com moltes d'altres que publico, es troba allotjada de forma gratuïta i, per tant, el rendiment és el que és.")
                        
                        )
               
               
               )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$mapa = renderLeaflet({
        
        if (input$SexeInput == "Total"){
            
            filtreSexe = c("Dona", "Home")
            
        }else if (input$SexeInput == "Dona"){
            
            filtreSexe = c("Dona")
            
        }else if (input$SexeInput == "Home"){
            
            filtreSexe = c("Home")
            
        }
        
        dataMap = poblacio%>%
            filter(A == input$Years,
                   Sexe %in% filtreSexe)
        
        if (input$Indicator == "Població total"){
            
            dataMap_pobT = dataMap%>%
                group_by(Barri,
                         Barri_nom)%>%
                summarise(N = sum(Poblacio))%>%
                ungroup()
            
            map_PobT = merge(geoLleida,
                             dataMap_pobT,
                             by.x = "NOM_BARRI",
                             by.y = "Barri")
            
            BinsJenks = unique(classIntervals(map_PobT$N, 5,
                                              style = "jenks",
                                              unique = T, 
                                              samp_prop = 0.5)$brks
                               )
            
            map_jenks = colorBin("viridis", 
                                 domain = map_PobT$N,
                                 bins = BinsJenks,
                                 na.color = "white", 
                                 reverse = T)
            
            leaflet()%>%
                addProviderTiles(providers$CartoDB.Positron)%>%
                
                setView(lng = 0.620015, lat = 41.617592, zoom = 11) %>%
                
                addPolygons(data = geoCat,
                            fillOpacity = 0,
                            opacity = 0,
                            stroke = F,
                            layerId = ~CODICOMAR)%>%
                
                addPolygons(data = geoPerfil,
                            fillOpacity = 0,
                            opacity = 0.8,
                            color = "black",
                            weight = 2)%>%
                
                addPolygons(data = map_PobT,
                            color = ifelse(map_PobT$N<BinsJenks[2], "black", "white"),
                            weight = 1,
                            fillOpacity = 0.5,
                            highlightOptions = highlightOptions(stroke = 5, weight = 4),
                            fillColor = ~map_jenks(map_PobT$N),
                            layerId = ~NOM_BARRI,
                            popup = paste0("<b>Barri:</b> ", 
                                           map_PobT$Barri_nom,
                                           "<br><b>Any:</b> ",
                                           input$Years,
                                           "<br><b>Sexe:</b> ",
                                           input$SexeInput,
                                           "<br><b>Població:</b> ",
                                           format(map_PobT$N, big.mark = ".", decimal.mark = ",") ))%>%
                
                addLegend(pal = map_jenks,
                          values = map_PobT$N,
                          labFormat = labelFormat(big.mark = " ", digits = 0),
                          position = "bottomright",
                          na.label = "",
                          title = paste("<b>Població</b><br><small>(Intervals Jenks)</small>"))
            
            
            
        }else if (input$Indicator == "% Variació any anterior"){
            
            
        }else if (input$Indicator == "% sobre el total de població"){
            
            
        }else if (input$Indicator == "% menors de 5 anys"){
            
            
            
        }else{
            
            leaflet()%>%
                addProviderTiles(providers$CartoDB.Positron)%>%
                addPolygons(data = geoPerfil,
                            fillOpacity = 0,
                            opacity = 1,
                            color = "black",
                            weight = 2)%>%
                addPolygons(data = geoLleida,
                            color = "white",
                            fillColor = "grey",
                            weight = 1,
                            highlightOptions = highlightOptions(stroke = 4, weight = 3),
                            popup = geoLleida$NOM_BARRI)
            
            
        }
        
    })
    
    output$PobTotal = renderText({
        
        dataText_df = dataMap()
        
        vectorBarris = unique(dataText_df$Barri_nom)
        
        if (input$SexeInput == "Total"){
            
            filtreSexe = c("Dona", "Home")
            
        }else if (input$SexeInput == "Dona"){
            
            filtreSexe = c("Dona")
            
        }else if (input$SexeInput == "Home"){
            
            filtreSexe = c("Home")
            
        }

        
        text_data = dataText_df%>%
            filter(
                   Sexe %in% filtreSexe)%>%
            group_by(A)%>%
            summarise(N = sum(Poblacio))%>%
            ungroup()%>%
            filter(A == input$Years)
        
        if (length(vectorBarris)>1){
            barriSelected = "(Total)"
        }else{
            barriSelected = paste0("(",vectorBarris,")")
        }
        
        paste0("Població ",barriSelected,
               ": ",
               format(text_data$N,
                      big.mark = ".",
                      decimal.mark = ","))
            

        
    })
    
    output$IndicadorHist = renderPlot({
        
        dataPlot_df = dataMap()
        
        if (input$SexeInput == "Total"){
            
            filtreSexe = c("Dona", "Home")
            
        }else if (input$SexeInput == "Dona"){
            
            filtreSexe = c("Dona")
            
        }else if (input$SexeInput == "Home"){
            
            filtreSexe = c("Home")
            
        }
        
        dataPlot = dataPlot_df%>%
            filter(Sexe %in% filtreSexe)
        
        if (input$Indicator == "Població total"){
            
            dataPlot_total = dataPlot%>%
                group_by(A)%>%
                summarise(N = sum(Poblacio))%>%
                ungroup()%>%
                mutate(gColor = ifelse(A == input$Years, "black", "#787A91"))
            
            ggplot(dataPlot_total,
                   aes(x = A, y = N))+
                geom_line()+
                geom_point()+
                scale_x_continuous(breaks = pretty_breaks())+
                scale_y_continuous(labels = label_number(big.mark = ".",
                                                         decimal.mark = ","),
                                   limits = c(min(dataPlot_total$N)-1000,
                                              max(dataPlot_total$N)+1000))+
                labs(x = "",
                     y = "",
                     title = "Població")+
                geom_text_repel(aes(x = A,
                                    y = N,
                                    label = format(N, decimal.mark = ",", big.mark = ".") ),
                                segment.curvature = 1,
                                nudge_x = c(-1.5, 1.5, -1.5, 1.5, -1.5, 1.5, -1.5, 1.5, -1.5),
                                nudge_y = c(-500, 500, -500, 500, -500, 500, -500, 500, -500),
                                color = dataPlot_total$gColor,
                                bg.color = "white",
                                bg.r = 0.15)+
                theme_hc()+
                theme(panel.background = element_rect(fill = "transparent"),
                      plot.background = element_rect(fill = "transparent", color = NA),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.background = element_rect(fill = "transparent"),
                      legend.box.background = element_blank())
            
        }else if (input$Indicator == "Població total"){
            
            
        }else if (input$Indicator == "% Variació any anterior"){
            
            
        }else if (input$Indicator == "% sobre el total de població"){
            
            
        }else if (input$Indicator == "% menors de 5 anys"){
            
            
            
        }else{
            
            
        }
    }, bg="transparent")
    
    output$EdatsDist = renderPlot({
        
        dataEdats_df = dataMap()
        
        ggEdats = dataEdats_df%>%
            filter(A == input$Years)%>%
            group_by(Edat_nom,
                     Edat_ordre,
                     Sexe)%>%
            summarise(N = sum(Poblacio))%>%
            ungroup()%>%
            mutate(Edat_nom = gsub("anys", "", Edat_nom))
        
        ggplot(ggEdats,
               aes(x = reorder(Edat_nom, Edat_ordre), 
                   y = N,
                   fill = Sexe))+
            geom_bar(stat = "identity",
                     position = "identity",
                     alpha = 0.5)+
            labs(x = "",
                 y = "",
                 title = "Edats",
                 fill = "")+
            scale_fill_tableau()+
            theme_hc()+
            theme(panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent", color = NA),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.background = element_rect(fill = "transparent"),
                  legend.box.background = element_blank(),
                  axis.text.x = element_text(angle = 90))
        
    }, bg="transparent")
    
    dataMap = eventReactive(input$mapa_shape_click,ignoreNULL = FALSE, {
        
        if (length(input$mapa_shape_click) == 0){
            event = "01"
        }else{
            event = input$mapa_shape_click
        }
        
        if (event[1] == "01"){
            poblacio
        }else{
            poblacio[poblacio$Barri==event[1],]
        }

        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
