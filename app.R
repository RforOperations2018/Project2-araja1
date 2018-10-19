#
# Name: Ashok Raja
# Project # 2
# Andrew ID: araja1


# Loading libraries

library(shiny)
library(dplyr)
library(plyr)
library(plotly)
library(tibble)
library(shinydashboard)
library(reshape2)
library(shinythemes)
library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)
library(leaflet)
library(rgdal)



ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for neighborhood Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}
id="e03a89dd-134a-4ee8-a2bd-62c40aeebc6f"
neigh <- ckanUniques(id, "INCIDENTNEIGHBORHOOD")
race <- ckanUniques(id, "RACE")
age<- ckanUniques(id, "AGE")


header <- dashboardHeader(title = "Pittsburgh Arrest Records")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Stats", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Map", icon = icon("map"), tabName = "plot2"),
    menuItem("Data", icon = icon("table"), tabName = "table"),
    selectInput("neigh_select",
                "Neighborhood:",
                choices = neigh,
                multiple = FALSE,
                selectize = TRUE,
                selected = 'Manchester'
    ),
    # Selecting the Race
    selectInput("race_select",
                "Race:",
                choices = race,
                multiple = FALSE
                
    ),
    #selecting the age using a slider
    sliderInput("age_select",
                "Age:",
                min = 10,
                max = 99,
                value = c(10, 99),
                step = 1
    ),
    #selecting the gender
    checkboxGroupInput("sex_select",
                       "Sex:",
                       choices = c('M','F'),
                       selected=1
    )
  )
)



body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("neighborhoods"),
            valueBoxOutput("offences"),
            valueBoxOutput("pk")
          ),
          fluidRow(
            # Rendering the  differnt plots in tabs
           tabBox(title = "Plot1",
                   width = 12,
                   tabPanel("Crimes by Age", plotlyOutput("plot")),
                   tabPanel("Crime Timeline", plotlyOutput("plot2"))

            )
          )
  ),
tabItem("plot2",
        fluidPage(
          # Rendering the  map
          leafletOutput("map")
        )
), 



 tabItem("table",
         inputPanel(
           # Download Button
           downloadButton("downloadData","Download Crime Data")
         ), 
         # Rendering the table
          fluidPage(
            box(title = "Crime Data", DT::dataTableOutput("table"), width=300))
  )
)
)

# passing the ui to the dashboard
ui <- dashboardPage(header, sidebar, body)

# Define server logic 
server <- function(input, output, session=session) 
{
  # Caputing the inputs for reactive functions
  swInput <- reactive({
    
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%22", id, "%22%20WHERE%20%22INCIDENTNEIGHBORHOOD%22=%20%27",URLencode(input$neigh_select),"%27%20AND%20%22RACE%22%20=%20%27",input$race_select,"%27%20AND%20%22AGE%22%20%3E%27",input$age_select[1],"%27%20AND%20%22AGE%22%20%3C%27",input$age_select[2],"%27")
    crime=ckanSQL(url)
    if(length(input$sex_select)>0){
      crime <- subset(crime, GENDER %in% input$sex_select)  
    }
    return(crime)
  })
 
  
  # Plot for # of crimes by age
  output$plot <- renderPlotly({
    crime=swInput()
    ggplotly(ggplot(data=crime,aes(x=AGE,colour=GENDER))+
               geom_bar()+
               labs(title="Crimes by Age",x="AGE",y="# Crimes",colour="Gender")
    )
  })
  # Plot the crime timeline
  output$plot2 <- renderPlotly({
    crime=swInput()
    ggplotly(ggplot(data=crime,aes(x=as.Date(ARRESTTIME)))+
               geom_freqpoly(aes(colour=GENDER))+
               labs(title="OffenceTimeline",x="Timeline",y="# of Offences",colour="Gender")
             
    )
  })
  # Plot the Incident zones and Gender
  output$plot3 <- renderPlotly({
    crime=swInput()
    ggplotly(ggplot(data=crime,aes(x=INCIDENTZONE,colour=GENDER))+
               geom_bar()+ coord_flip()+
               labs(title="Offenses by Incident Zone",x="Zone",y="# of Offenses",colour="Gender")
    )
  })
  # Data table 
  output$table <- DT::renderDataTable(crime <-swInput(), options = list(scrollX = TRUE))

  # Total CCR info box
  output$neighborhoods <- renderInfoBox({
    crime= swInput()
    num <- length(unique(crime$CCR))
    
    infoBox("# CCR", value = num, icon = icon("balance-scale"), color = "purple")
    
  })
  
#   Total offences per selection
  output$offences <- renderValueBox({
    crime=swInput()
    num <- length(unique(crime$OFFENSES))
    valueBox(subtitle = "Unique Offences", value = num, icon = icon("sort-numeric-asc"), color = "green")
  }) 
  
  # Total crimes per selection
  output$pk <- renderValueBox({
    crime=swInput()
    num <- length(crime$PK)
    valueBox(subtitle = "# PK", value = num, icon = icon("sort-numeric-asc"), color = "red")
  }) 
  
# Render Crime Map
  output$map <- renderLeaflet({
    crime <- swInput()
    url <- paste0("https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/PGHWebNeighborhoods/FeatureServer/0/query?where=HOOD+%3D+%27",URLencode(input$neigh_select),"%27&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=&returnGeometry=true&returnCentroid=false&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnDistinctValues=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=geojson&token=")
    pitt=readOGR(url)
    sp=spTransform(pitt,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    # Call Data and Build Map
    leaflet() %>%
      setView(lng = -80.0004, lat = 40.4418, zoom = 12)%>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data=sp,color = "#444444", weight = 1, smoothFactor = 0.5,  group="HOOD")%>%
      addCircleMarkers(data = crime, lng = ~X, lat = ~Y,radius = 1.5, color = "#E7298A", group="crime", popup = ~OFFENSES)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("download", ".csv", sep = "")
    },
    content = function(file) {
      crime <- swInput() 
      write.csv(crime, file)
    }  
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

