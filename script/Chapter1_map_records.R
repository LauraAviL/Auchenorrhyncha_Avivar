rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(skimr,writexl,readr,readxl,data.table)
pacman::p_load(tidyverse,tidylog,summarytools,here,plotrix,
               lubridate, textshape, leaflet, htmlwidgets) 

# Chapter 1----
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
library(leaflet)
#Records map----
df<-read_excel(here("data/records.xlsx")) 

# Agrupar especies por código para que todas aparezcan en el mismo punto
df_grouped <- df %>%
  group_by(code, decimalLatitude, decimalLongitude, province) %>%
  summarise(scientificname = paste(unique(scientificname), collapse = "<br>"), .groups = 'drop')

# Crear un mapa base
mapa <- leaflet(df_grouped) %>%
  addTiles()

# Generar una paleta de colores para provincias
color_pal <- leaflet::colorFactor(palette = "Set1", domain = df_grouped$province)

# Lista de grupos de capas (simulación de jerarquía)
overlay_groups <- c()

# Iterar sobre cada provincia y agregar sus códigos
for (prov in unique(df_grouped$province)) {
  subset_prov <- df_grouped %>% filter(province == prov)
  
  for (c in unique(subset_prov$code)) {
    subset_code <- subset_prov %>% filter(code == c)
    
    mapa <- mapa %>%
      addCircleMarkers(
        data = subset_code,
        lng = ~decimalLongitude,  # Longitud
        lat = ~decimalLatitude,   # Latitud
        popup = ~paste0("<b>Code:</b> ", code, "<br>",
                        "<b>Species:</b> <br><i>", scientificname, "</i>"),  # Cursiva para especies
        label = ~code,
        radius = 6, 
        fillOpacity = 0.8,
        color = ~color_pal(province),
        group = paste0(prov, " → ", c)  # Simulación de jerarquía
      )
    
    overlay_groups <- c(overlay_groups, paste0(prov, " → ", c))
  }
}

# Agregar el control de capas sin `leaflet.extras2`
mapa <- mapa %>%
  addLayersControl(
    overlayGroups = unique(overlay_groups),  # Convertimos a vector único
    options = layersControlOptions(collapsed = TRUE)  # Control desplegable
  ) %>%
  addLegend(
    "bottomright",
    pal = color_pal,
    values = df_grouped$province,
    title = "Province",
    opacity = 1
  )

# Mostrar el mapa
mapa


# Guardar el mapa en la carpeta raíz del proyecto
saveWidget(mapa, here("salidas/map_records.html"), selfcontained = TRUE)


########################################
# Cargar librerías necesarias
library(leaflet)
library(dplyr)
library(readxl)
library(here)
library(DT)
library(shiny)

# Leer los datos desde Excel
df <- read_excel(here("data/records.xlsx"))

# Agrupar especies por código para que todas aparezcan en el mismo punto
df_grouped <- df %>%
  group_by(code, decimalLatitude, decimalLongitude, province) %>%
  summarise(scientificname = paste(unique(scientificname), collapse = "<br>"), .groups = 'drop')

# Crear la paleta de colores para provincias
color_pal <- leaflet::colorFactor(palette = "Set1", domain = df_grouped$province)

# Interfaz de usuario (UI) para el shiny app
ui <- fluidPage(
  titlePanel("Records Map & Species Data"),
  
  sidebarLayout(
    sidebarPanel(
      DTOutput("species_table")  # Tabla interactiva de especies
    ),
    
    mainPanel(
      leafletOutput("mapa", height = "600px")  # Mapa interactivo
    )
  )
)

# Servidor de la aplicación shiny
server <- function(input, output, session) {
  
  # Renderizar el mapa
  output$mapa <- renderLeaflet({
    
    # Crear un mapa base
    mapa <- leaflet(df_grouped) %>%
      addTiles()
    
    # Iterar sobre cada provincia y agregar sus códigos
    for (prov in unique(df_grouped$province)) {
      subset_prov <- df_grouped %>% filter(province == prov)
      
      for (c in unique(subset_prov$code)) {
        subset_code <- subset_prov %>% filter(code == c)
        
        mapa <- mapa %>%
          addCircleMarkers(
            data = subset_code,
            lng = ~decimalLongitude,  # Longitud
            lat = ~decimalLatitude,   # Latitud
            popup = ~paste0("<b>Code:</b> ", code, "<br>",
                            "<b>Species:</b> <br><i>", scientificname, "</i>"),  # Cursiva para especies
            label = ~code,
            radius = 6, 
            fillOpacity = 0.8,
            color = ~color_pal(province),
            group = paste0(prov, " → ", c)  # Simulación de jerarquía
          )
      }
    }
    
    # Agregar el control de capas y leyenda
    mapa <- mapa %>%
      addLayersControl(
        overlayGroups = unique(df_grouped$code),  
        options = layersControlOptions(collapsed = TRUE)  # Control desplegable
      ) %>%
      addLegend(
        "bottomright",
        pal = color_pal,
        values = df_grouped$province,
        title = "Province",
        opacity = 1
      )
    
    return(mapa)
  })
  
  # Renderizar la tabla interactiva con especies
  output$species_table <- renderDT({
    df %>%
      select(code, scientificname, province) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
  })
}

# Ejecutar la aplicación shiny
shinyApp(ui, server)
output$save_map <- downloadHandler(
  filename = function() {
    "maptable_records.html"
  },
  content = function(file) {
    saveWidget(mapa, file, selfcontained = TRUE)
  }
)
