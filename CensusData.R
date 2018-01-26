#
# Author:   Grace Hwang
# Date:     Jan 10, 2018
# Purpose:  Using Leaflet to produce an interactive 
#           chloropleth map based on the 
#           Census Data - Selected socioeconomic indicators in Chicago, 2008  2012
#           See original data here: https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2/data


#############################
## Load necessary packages ##
#############################
library( leaflet )
library( htmltools )
library( shiny )
library( sp )
library( rgdal )

###########################
## Load necessary data ##
###########################
census.data <- read.csv( file = "https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD"
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

# Transform URL into spatial dataframe
comarea <- readOGR( dsn = "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GEOJSON"
                    , layer = "OGRGeoJSON"
                    , stringsAsFactors = FALSE
)

# Merge the non-spatial data frame onto spatial polygon data frame
comarea <- sp::merge( x = comarea
                     , y = census.data
                     , by.x = "area_numbe"
                     , by.y = "Community.Area.Number"
)

# Make colors
color.function <- colorRampPalette( c( "#CCCCCC", "#660066" ) )
color.ramp <- color.function( n = 5 )

# Assign colors to groups
# Crowded Housing
comarea$color_crowded <- as.character(
  cut(
    x = rank( comarea$PERCENT.OF.HOUSING.CROWDED )
    , breaks = 5
    , labels = color.ramp
  )
)

# Poverty
comarea$color_poverty <- as.character(
  cut(
    x = rank( comarea$PERCENT.HOUSEHOLDS.BELOW.POVERTY )
    , breaks = 5
    , labels = color.ramp
  )
)

# Unemployed
comarea$color_unemployed <- as.character(
  cut(
    x = rank( comarea$PERCENT.AGED.16..UNEMPLOYED )
    , breaks = 5
    , labels = color.ramp
  )
)

# Diploma
comarea$color_diploma <- as.character(
  cut(
    x = rank( comarea$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA )
    , breaks = 5
    , labels = color.ramp
  )
)

# Age
comarea$color_age <- as.character(
  cut(
    x = rank( comarea$PERCENT.AGED.UNDER.18.OR.OVER.64 )
    , breaks = 5
    , labels = color.ramp
  )
)

# Income
comarea$color_income <- as.character(
  cut(
    x = rank( comarea$PER.CAPITA.INCOME )
    , breaks = 5
    , labels = color.ramp
  )
)

# Hardship
comarea$color_hardship <- as.character(
  cut(
    x = rank( comarea$HARDSHIP.INDEX )
    , breaks = 5
    , labels = color.ramp
  )
)

# Make hover labels
# Courtesy of https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Build%20App/leaflet_ny.r#L36
# Crowded Housing
label_crowded <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.OF.HOUSING.CROWDED
  )
  , HTML)

# Poverty
label_poverty <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.HOUSEHOLDS.BELOW.POVERTY
  )
  , HTML)

# Unemployed
label_unemployed <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.AGED.16..UNEMPLOYED
  )
  , HTML)

# Diploma
label_diploma <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
  )
  , HTML)

# Age
label_age <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.AGED.UNDER.18.OR.OVER.64
  )
  , HTML)

# Income
label_income <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s dollars"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PER.CAPITA.INCOME
  )
  , HTML)

# Hardship
label_hardship <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$HARDSHIP.INDEX
  )
  , HTML)

# Make map
census_map <- 
  leaflet( data = comarea
           , options = leafletOptions( zoomControl = FALSE
                                                   , minZoom = 11
                                                   , maxZoom = 11
                                                   , dragging = FALSE
                                                   ) ) %>%
  
  # add background to map
  addTiles( urlTemplate = "https://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png" ) %>%
  
  # set zoom level
  setView( lng = -87.707988
           , lat = 41.832249
           , zoom = 11
  ) %>%
  
  # add Crowded Housing polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 1
               , fillColor = comarea$color_crowded
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
               , fillOpacity = 1
               , fillColor = comarea$color_poverty
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
               , fillOpacity = 1
               , fillColor = comarea$color_unemployed
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
               , fillOpacity = 1
               , fillColor = comarea$color_diploma
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
               , fillOpacity = 1
               , fillColor = comarea$color_age
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
               , fillOpacity = 1
               , fillColor = comarea$color_income
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
               , fillOpacity = 1
               , fillColor = comarea$color_hardship
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

# Create the UI

ui <- fillPage( leaflet::leafletOutput( outputId = "mymap", height = "100%" ) )


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
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 1.8%", "1.9 - 3.2%", "3.3 - 4.5%", "4.6 - 7.4%", "7.5 - 15.8%" )
                                    , opacity = 1
                                    , group = "Percent of Crowded Housing"
      ) }
    else if( input$mymap_groups == "Percent of Households Below Poverty" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 12.3%", "12.4 - 16.9%", "17.0 - 21.7%", "21.8 - 29.6%", "29.7 - 56.5%" )
                                    , opacity = 1
                                    , group = "Percent of Households Below Poverty"
      ) }
    else if( input$mymap_groups == "Percent of Aged 16+ Unemployed" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 8.7%", "8.8 - 11.7%", "11.8 - 16.5%", "16.6 - 21.1%", "21.2 - 35.9%" )
                                    , opacity = 1
                                    , group = "Percent of Aged 16+ Unemployed"
      ) }
    else if( input$mymap_groups == "Percent of Aged 25+ Without High School Diploma" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 10.9%", "11.0 - 15.9%", "16.0 - 20.8%", "20.9 - 28.5%", "28.6 - 54.8%" )
                                    , opacity = 1
                                    , group = "Percent of Aged 25+ Without High School Diploma"
      ) }
    else if( input$mymap_groups == "Percent of Aged Under 18 or Over 64" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0.0 - 30.7%", "30.8 - 36.4%", "36.5 - 39.0%", "39.1 - 41.0%", "41.1 - 51.5%" )
                                    , opacity = 1
                                    , group = "Percent of Aged Under 18 or Over 64"
      ) }
    else if( input$mymap_groups == "Per Capita Income" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "$0 - 14,685", "$14,686 - 17,949", "$17,950 - 23,791", "$23,792 - 33,385", "$33,386 - 88,669" )
                                    , opacity = 1
                                    , group = "Per Capita Income"
      ) }
    else if( input$mymap_groups == "Hardship Index" ) {
      mymap <- mymap %>% addLegend( "bottomright"
                                    , colors = color.ramp
                                    , title = "Legend"
                                    , labels = c( "0 - 20", "21 - 39", "40 - 58", "59 - 78", "79 - 98" )
                                    , opacity = 1
                                    , group = "Hardship Index"
      ) } # end of else if
  }
  ) # end of observe event
  
} # closing out server


# Run the Shiny app object
shinyApp( ui, server )

# end of script #
