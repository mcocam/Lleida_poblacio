library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)

geo_barris = readRDS("Shapefiles/Barris_Lleida.Rds")
geo_perfil = readRDS("Shapefiles/Perfil_Lleida.Rds")

ui <- navbarPage(
    
    title = "Mapa Barcelona Perfil",
    
    tabPanel("Mapa Barcelona",
             
        div(
            tags$head(
                includeCSS("css/styles.css")
            ),
            
            leafletOutput("perfil")))
    

)

server <- function(input, output) {
    
    output$perfil = renderLeaflet(
        leaflet(geo_perfil)%>%
            addProviderTiles(providers$OpenStreetMap)%>%
            addPolygons()
    )
    
}

shinyApp(ui = ui, server = server)
