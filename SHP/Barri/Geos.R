#Es neteja l'entorn
rm(list=ls())

#Llibreries
library(data.table)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)
library(tidyr)
library(ggrepel)
library(scales)
library(glue)
library(ggtext)
library(stringr)
library(geojsonio)
library(leaflet)
library(sp)
library(rgdal)
library(sf)

#Working directory: només amb Rstudio
#Si no s'utilitza Rstudio, especificar amb setwd(...)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#2018, 2017, 2016, 2015
URLs = c("https://opendata-ajuntament.barcelona.cat/data/dataset/5411c8e1-1ede-47d6-92ce-2035141d8721/resource/9d54019f-eb4f-49bc-80cf-f9d9934e5097/download/2018_renda_neta_mitjana_per_persona.csv",
         "https://opendata-ajuntament.barcelona.cat/data/dataset/5411c8e1-1ede-47d6-92ce-2035141d8721/resource/d6bb4f5e-d719-447d-b6e8-5e3be2f710f2/download/2017_renda_neta_mitjana_per_persona.csv",
         "https://opendata-ajuntament.barcelona.cat/data/dataset/5411c8e1-1ede-47d6-92ce-2035141d8721/resource/eedcb124-ff52-475d-97cd-94c48f83fd30/download/2016_renda_neta_mitjana_per_persona.csv",
         "https://opendata-ajuntament.barcelona.cat/data/dataset/5411c8e1-1ede-47d6-92ce-2035141d8721/resource/7c1e5f6a-c7af-4d63-9d8a-f4611da252bf/download/2015_renda_neta_mitjana_per_persona.csv")

data = tribble(
  ~Any, ~Codi_Districte, ~Nom_Districte, ~Codi_Barri, ~Nom_Barri, ~Seccio_Censal, ~Import_Euros
)

for (i in 1:length(URLs)){
  
  data = data%>%
    bind_rows(fread(URLs[i],
                    encoding = "UTF-8"))
  
}

data = data%>%
  mutate(Codi_Districte = str_pad(Codi_Districte, width = 2, side = "left", pad = "0"),
         Codi_Barri = str_pad(Codi_Barri, width = 2, side = "left", pad = "0"),
         Seccio_Censal = str_pad(Seccio_Censal, width = 2, side = "left", pad = "0"))

saveRDS(data, "data/data.Rds")


geo_BCN = rgdal::readOGR("Cat.shp",
                         use_iconv = TRUE, 
                         encoding = "UTF-8")

geo_BCN_t = spTransform(geo_BCN, CRS("+proj=longlat +datum=WGS84 +no_defs"))

leaflet(geo_BCN_t)%>%
  addPolygons()

saveRDS(geo_BCN_t,
        "Cat.Rds")
