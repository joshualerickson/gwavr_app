library(gwavr)
library(shiny)
library(tidyverse)
library(shinythemes)
library(promises)
library(future)
library(sf)
library(shinyBS)
plan(multisession)

jsStr <- '$(document).ready(function(){
  $("a[data-value=\'About\']").attr({
    "href":"#",
    "data-toggle":"modal",
    "data-target":"#modal1"
  });
})
'

ui = fluidPage(tags$head(tags$script(HTML(jsStr))),
               navbarPage(theme = shinytheme('lumen'),collapsible = TRUE,
                          HTML("<a style='cursor:grab;>jander<text-decoration:none;cursor:default;color:#000000;
                font-size: 30px;
                font-family: 'Montserrat', sans-serif;'
                class='active' href='https://github.com/joshualerickson/gwavr'><b>gwavr!</b></a>"), id="nav",
                windowTitle = "gwavr!",
                tabPanel('About',
                         tags$style(type = 'text/css', '#hydro-ui-leaf_map {height: calc(100vh - 250px) !important;}
                           body {font-family: "Montserrat", sans-serif;}'),
                         nhdplusModUI('hydro-ui'),
                         selectInput('export_format', 'Select Format', choices = c("ESRIShapefile", "KML", 'GeoJSON', 'GPKG', 'csv')),
                         downloadButton("downloadData", label = "Download")
                )
#                 tags$script(HTML(
#                     "
# Shiny.addCustomMessageHandler(
#   'removeleaflet',
#   function(x){
#     console.log('deleting',x)
#     // get leaflet map
#     var map = HTMLWidgets.find('#' + x.elid).getMap();
#     // remove
#     map.removeLayer(map._layers[x.layerid])
#   })
# "
#                 ))
), shinyBS::bsModal("modal1", "Example", "moda",
                                      shiny::uiOutput("about_ui")))

server <- function(input, output, session){

   #modal
  output$about_ui <- renderUI({

    shiny::includeHTML('www/about.html')
  })

    values <- reactiveValues()

    crud_mod <- callModule(
        nhdplusMod,
        'hydro-ui',
        values = values
    )

    observe({crud_mod})

    output$downloadData <- downloadHandler(

        filename = function(){
            paste('my-gwavr', sep = '.', switch(
                input$export_format,
                ESRIShapefile = 'zip',
                KML = 'zip',
                csv = 'zip',
                GeoJSON = 'zip',
                GPKG = 'gpkg'
            ))},
        content = function(file) {

            tmp.path <- dirname(file)
            name.base <- file.path(tmp.path)
            name.glob <- paste0(name.base, ".*")
            name.shp  <- paste0(name.base, ".shp")
            name.zip  <- paste0(name.base, ".zip")

            if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))

            if(input$export_format == "ESRIShapefile"){

                for(i in 1:length(values$hydro_data_list)){
                    sf::st_write(values$hydro_data_list[[i]],
                                 dsn = paste0(name.base,'/',names(values$hydro_data_list[i]), '.shp'),
                                 driver = "ESRI Shapefile",
                                 quiet = TRUE)
                }

                zip::zipr(zipfile = name.zip, files = list.files(name.base,pattern = '.shp|.dbf|.shx|.prj', full.names = TRUE))
                req(file.copy(name.zip, file))

            } else if (input$export_format == "KML"){

                for(i in 1:length(values$hydro_data_list)){
                    sf::st_write(values$hydro_data_list[[i]],
                                 dsn = paste0(name.base,'/',names(values$hydro_data_list[i]), '.kml'),
                                 driver = "KML",
                                 quiet = TRUE)
                }

                zip::zipr(zipfile = name.zip, files = list.files(name.base, pattern = '.kml', full.names = TRUE))
                req(file.copy(name.zip, file))

            } else if (input$export_format == 'csv'){

                for(i in 1:length(values$hydro_data_list)){
                    write.csv(values$hydro_data_list[[i]] %>% sf::st_drop_geometry(),
                              file = paste0(name.base,'/',names(values$hydro_data_list[i]), '.csv'))
                }

                zip::zipr(zipfile = name.zip, files = list.files(name.base, pattern = '.csv', full.names = TRUE))
                req(file.copy(name.zip, file))

            } else if (input$export_format == 'GeoJSON'){

                for(i in 1:length(values$hydro_data_list)){
                    sf::st_write(values$hydro_data_list[[i]],
                                 dsn = paste0(name.base,'/',names(values$hydro_data_list[i]), '.geojson'),
                                 driver = "GeoJSON",
                                 quiet = TRUE)
                }

                zip::zipr(zipfile = name.zip, files = list.files(name.base, pattern = '.geojson', full.names = TRUE))
                req(file.copy(name.zip, file))

            } else if (input$export_format == 'GPKG'){

                for(i in 1:length(values$hydro_data_list)){
                    sf::st_write(values$hydro_data_list[[i]],
                                 dsn = paste0(name.base,'/gwavr.gpkg'),
                                 layer = paste0(names(values$hydro_data_list[i])),
                                 driver = "GPKG",
                                 quiet = TRUE)
                }

                # zip::zipr(zipfile = name.zip, files = list.files(name.base, pattern = '.gpkg'))
                req(file.copy(paste0(name.base,'/gwavr.gpkg'), file))

            }


        }
    )

}

##### RUN APPLICATION #####
shiny::shinyApp(ui, server)
