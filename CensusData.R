#
# Author:   Grace Hwang
# Date:     Jan 10, 2018
# Purpose:  Using Leaflet to produce an interactive 
#           chloropleth map based on the 
#           Census Data 2008-2012


#############################
## Load necessary packages ##
#############################
library( leaflet )
library( dplyr )
library( magrittr )
library( htmltools )
library( htmlwidgets )
library( shiny )
library( shinydashboard )
library( stringr )
library( DT )
library( geojsonio )
library( scales )
library( sp )
library( rgdal )

###########################
## Import necessary data ##
###########################
census.data <- read.csv( file = "/Users/gracehwang/desktop/Census Data/Census_Data.csv"
                         , header = TRUE
                         , stringsAsFactors = FALSE
                         )

# check dim
dim( census.data ) # [1] 78  9

# check colnames
colnames( census.data )
# [1] "Community.Area.Number"                        "COMMUNITY.AREA.NAME"                         
# [3] "PERCENT.OF.HOUSING.CROWDED"                   "PERCENT.HOUSEHOLDS.BELOW.POVERTY"            
# [5] "PERCENT.AGED.16..UNEMPLOYED"                  "PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA"
# [7] "PERCENT.AGED.UNDER.18.OR.OVER.64"             "PER.CAPITA.INCOME"                           
# [9] "HARDSHIP.INDEX"  


##################
## Make the map ##
##################
# Load necessary dataframe
# Save copied link as a character vector
geojson_comarea_url <- "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GEOJSON"

# Transform vector into spatial dataframe
comarea <- readOGR( dsn = geojson_comarea_url
                    , layer = "OGRGeoJSON"
                    , stringsAsFactors = FALSE
)
# OGR data source with driver: GeoJSON 
# Source: "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GEOJSON", layer: "OGRGeoJSON"
# with 77 features
# It has 9 fields

# Merge the 2 dataframes
merged <- sp::merge( comarea
                     , census.data
                     , by.x = "area_numbe"
                     , by.y = "Community.Area.Number"
                     )

# Make colors
# Crowded Housing
color_crowded <- colorRampPalette( c( "#CCCCCC", "#660066" ) )

# Poverty
color_poverty <- colorRampPalette( c( "#CCCCCC", "#660066" ) )

# Unemployed
color_unemployed <- colorRampPalette( c( "#CCCCCC", "#660066" ) )

# Diploma
color_diploma <- colorRampPalette( c( "#CCCCCC", "#660066" ) )

# Age
color_age <- colorRampPalette( c( "#CCCCCC", "#660066" ) )

# Income
color_income <- colorRampPalette( c( "#CCCCCC", "#660066" ) )

# Hardship
color_hardship <- colorRampPalette( c( "#CCCCCC", "#660066" ) )

# Decide how many groups you want
# Crowded Housing
color_ramp_crowded <- color_crowded( 5 )

# Poverty
color_ramp_poverty <- color_poverty( 5 )

# Unemployed
color_ramp_unemployed <- color_unemployed( 5 )

# Diploma
color_ramp_diploma <- color_diploma( 5 )

# Age
color_ramp_age <- color_age( 5 )

# Income
color_ramp_income <- color_income( 5 )

# Harship
color_ramp_hardship <- color_hardship( 5 )

# Assign colors to groups
# Crowded Housing
merged$color_crowded <- as.character(
  cut(
    x = rank( merged$PERCENT.OF.HOUSING.CROWDED )
    , breaks = 5
    , labels = color_ramp_crowded
  )
)

# Poverty
merged$color_poverty <- as.character(
  cut(
    x = rank( merged$PERCENT.HOUSEHOLDS.BELOW.POVERTY )
    , breaks = 5
    , labels = color_ramp_poverty
  )
)

# Unemployed
merged$color_unemployed <- as.character(
  cut(
    x = rank( merged$PERCENT.AGED.16..UNEMPLOYED )
    , breaks = 5
    , labels = color_ramp_unemployed
  )
)

# Diploma
merged$color_diploma <- as.character(
  cut(
    x = rank( merged$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA )
    , breaks = 5
    , labels = color_ramp_diploma
  )
)

# Age
merged$color_age <- as.character(
  cut(
    x = rank( merged$PERCENT.AGED.UNDER.18.OR.OVER.64 )
    , breaks = 5
    , labels = color_ramp_age
  )
)

# Income
merged$color_income <- as.character(
  cut(
    x = rank( merged$PER.CAPITA.INCOME )
    , breaks = 5
    , labels = color_ramp_income
  )
)

# Hardship
merged$color_hardship <- as.character(
  cut(
    x = rank( merged$HARDSHIP.INDEX )
    , breaks = 5
    , labels = color_ramp_hardship
  )
)

# Make hover layer
# Crowded Housing
label_crowded <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , merged$COMMUNITY.AREA.NAME
    , merged$PERCENT.OF.HOUSING.CROWDED
    )
  , HTML)

# Poverty
label_poverty <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , merged$COMMUNITY.AREA.NAME
    , merged$PERCENT.HOUSEHOLDS.BELOW.POVERTY
  )
  , HTML)

# Unemployed
label_unemployed <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , merged$COMMUNITY.AREA.NAME
    , merged$PERCENT.AGED.16..UNEMPLOYED
  )
  , HTML)

# Diploma
label_diploma <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , merged$COMMUNITY.AREA.NAME
    , merged$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
  )
  , HTML)

# Age
label_age <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , merged$COMMUNITY.AREA.NAME
    , merged$PERCENT.AGED.UNDER.18.OR.OVER.64
  )
  , HTML)

# Income
label_income <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s dollars"
    , merged$COMMUNITY.AREA.NAME
    , merged$PER.CAPITA.INCOME
  )
  , HTML)

# Hardship
label_hardship <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s"
    , merged$COMMUNITY.AREA.NAME
    , merged$HARDSHIP.INDEX
  )
  , HTML)

# Make map
census_map <- leaflet( data = merged 
                       , options = leafletOptions( zoomControl = FALSE
                                                   , minZoom = 11
                                                   , maxZoom = 11
                                                   , dragging = FALSE
                                                   )
                       ) %>%
  
  # add background to map
  addTiles( urlTemplate = "https://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png" ) %>%
  
  # set zoom level
  setView( lng = -87.707988
           , lat = 41.832249
           , zoom = 11
  ) %>%
  
  # add title
  addMarkers( "Census Data"
              , position = "topleft"
              ) %>%
  
  # add Crowded Housing polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 0.5
               , fillColor = merged$color_crowded
               , color = "white"
               , label = label_crowded
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              ,textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Crowded Housing"
  ) %>%
  
  # Add Poverty polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 0.5
               , fillColor = merged$color_poverty
               , color = "white"
               , label = label_poverty
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              ,textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Households Below Poverty"
  ) %>%
  
  # Add Unemployed polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 0.5
               , fillColor = merged$color_unemployed
               , color = "white"
               , label = label_unemployed
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              ,textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Aged 16+ Unemployed"
  ) %>%
  
  # Add Diploma polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 0.5
               , fillColor = merged$color_diploma
               , color = "white"
               , label = label_diploma
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              ,textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Aged 25+ Without High School Diploma"
  ) %>%
  
  # Add Age polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 0.5
               , fillColor = merged$color_age
               , color = "white"
               , label = label_age
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              ,textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Aged Under 18 or Over 64"
  ) %>%
  
  # Add Income polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 0.5
               , fillColor = merged$color_income
               , color = "white"
               , label = label_income
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              ,textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Per Capita Income"
  ) %>%
  
  # Add Hardship polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 0.5
               , fillColor = merged$color_hardship
               , color = "white"
               , label = label_hardship
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              ,textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Hardship Index"
  ) %>%
  
  # add Layers control
  addLayersControl( baseGroups = c( "Percent of Crowded Housing"
                                    , "Percent of Households Below Poverty"
                                    , "Percent of Aged 16+ Unemployed"
                                    , "Percent of Aged 25+ Without High School Diploma"
                                    , "Percent of Aged Under 18 or Over 64"
                                    , "Per Capita Income"
                                    , "Hardship Index"
                                    )
                    , position = "topright"
                    , options = layersControlOptions( collapsed = FALSE )
  ) 

######################
## Create dashboard ##
######################
# Create a header
# header <- dashboardHeader( title = "City of Chicago Census Data"
#                            , titleWidth = 300
#                            ) # end of header


# Create a sidebar
# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     menuItem( "Citywide", tabName = "Citywide", icon = icon( "home" ) )
#   )
#   , width = 300
# ) # end of sidebar


# Create a body
# body <- dashboardBody(
#   
#   # initialize tabs
#   tabItems(
#     tabItem(tabName = "Citywide"
#             , fluidRow(
#               box( title = "View Map"
#                    , status = "primary"
#                    , solidHeader = TRUE
#                    , collapsible = FALSE
#                    , width = 12
#                    , column( width = 12
#                              , leaflet::leafletOutput( outputId = "mymap"
#                                                        , height = 1000
#                                                        )
#                              ) # end of column
#                    ) # end of box
#             ) # end of fluidrow
#             ) # end of Citywide tab
#     )
#   ) # end of body


# Create a server
server <- function( input, output ) {
  
  # render leaflet output
  output$mymap <- leaflet::renderLeaflet({
    census_map 
  }) # end of renderleaflet
  
  # Dynamic Legend
   observeEvent( input$mymap_groups,{
   
     mymap <- leafletProxy( "mymap" ) %>% clearControls()
   
     if( input$mymap_groups == "Percent of Crowded Housing" ) {
       mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color_ramp_crowded
                                    , values = merged$PERCENT.OF.HOUSING.CROWDED
                                    , title = "Legend"
                                    , labels = c( "0.0 - 1.8%", "1.9 - 3.2%", "3.3 - 4.5%", "4.6 - 7.4%", "7.5 - 15.8%" )
                                    #, labFormat = labelFormat( suffix = "%" )
                                    , opacity = 1
                                    , group = "Percent of Crowded Housing"
       ) }
     else if( input$mymap_groups == "Percent of Households Below Poverty" ) {
       mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color_ramp_poverty
                                    , values = merged$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                                    , title = "Legend"
                                    , labels = c( "0.0 - 12.3%", "12.4 - 16.9%", "17.0 - 21.7%", "21.8 - 29.6%", "29.7 - 56.5%" )
                                    #, labFormat = labelFormat( suffix = "%" )
                                    , opacity = 1
                                    , group = "Percent of Households Below Poverty"
       ) }
     else if( input$mymap_groups == "Percent of Aged 16+ Unemployed" ) {
       mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color_ramp_unemployed
                                    , values = merged$PERCENT.AGED.16..UNEMPLOYED
                                    , title = "Legend"
                                    , labels = c( "0.0 - 8.7%", "8.8 - 11.7%", "11.8 - 16.5%", "16.6 - 21.1%", "21.2 - 35.9%" )
                                    #, labFormat = labelFormat( suffix = "%" )
                                    , opacity = 1
                                    , group = "Percent of Aged 16+ Unemployed"
       ) }
     else if( input$mymap_groups == "Percent of Aged 25+ Without High School Diploma" ) {
       mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color_ramp_diploma
                                    , values = merged$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                                    , title = "Legend"
                                    , labels = c( "0.0 - 10.9%", "11.0 - 15.9%", "16.0 - 20.8%", "20.9 - 28.5%", "28.6 - 54.8%" )
                                    #, labFormat = labelFormat( suffix = "%" )
                                    , opacity = 1
                                    , group = "Percent of Aged 25+ Without High School Diploma"
       ) }
     else if( input$mymap_groups == "Percent of Aged Under 18 or Over 64" ) {
       mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color_ramp_age
                                    , values = merged$PERCENT.AGED.UNDER.18.OR.OVER.64
                                    , title = "Legend"
                                    , labels = c( "0.0 - 30.7%", "30.8 - 36.4%", "36.5 - 39.0%", "39.1 - 41.0%", "41.1 - 51.5%" )
                                    #, labFormat = labelFormat( suffix = "%" )
                                    , opacity = 1
                                    , group = "Percent of Aged Under 18 or Over 64"
       ) }
     else if( input$mymap_groups == "Per Capita Income" ) {
       mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color_ramp_income
                                    , values = merged$PER.CAPITA.INCOME
                                    , title = "Legend"
                                    , labels = c( "$0 - 14,685", "$14,686 - 17,949", "$17,950 - 23,791", "$23,792 - 33,385", "$33,386 - 88,669" )
                                    #, labFormat = labelFormat( prefix = "$" )
                                    , opacity = 1
                                    , group = "Per Capita Income"
       ) }
     else if( input$mymap_groups == "Hardship Index" ) {
       mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color_ramp_hardship
                                    , values = merged$HARDSHIP.INDEX
                                    , title = "Legend"
                                    , labels = c( "0 - 20", "21 - 39", "40 - 58", "59 - 78", "79 - 98" )
                                    , opacity = 1
                                    , group = "Hardship Index"
       ) } # end of else if
   }
   ) # end of observe event
  
} # closing out server


# Create the UI

ui <- fillPage( leaflet::leafletOutput( outputId = "mymap", height = "100%" ) )

# Preview the UI in the console
shinyApp( ui, server )



