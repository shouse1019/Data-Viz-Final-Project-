#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(sf)
library(sp)
library(leaflet)
library(ggmap)
library(tidyverse)


load("./data/final_project.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
    includeCSS("www/style.css"),
    # Application title
    navbarPage(title=tags$div(
                         tags$img(id="sb_icon",src="sb_flag_embiggened-1.png",height="30px"),
                         "South Bend, IN"),
               tabPanel("Quality of Life Indicators"),
               windowTitle = "South Bend, IN - Quality of Like Indicators"
               ),
    fluidRow(class="container-fluid",
        wellPanel(
                checkboxGroupInput("council_members", 
                                   h4("Council Member Filter"), 
                                   choiceNames = levels(council_census_df$Council_Member),
                                   choiceValues = unique_council_members_parks,
                                   selected = unique_council_members_parks,
                                   inline = TRUE
                )
        )
    ),
    fluidRow(height="100%",
        mainPanel(width=12,
            tabsetPanel(type="tabs",
                tabPanel("Schools",
                    fluidRow(style = "height: calc(100vh - 50px - 20px -  1px);",
                      column(width = 2,
                          checkboxGroupInput("school_types", 
                                           h4("Type Of School"), 
                                           choices = uniq_schools,
                                            selected = uniq_schools
                          )
                          
                      ),
                      column(width = 5,style = "height: 75%;",
                         leafletOutput("schoolsMap",height="100%")
                      ),
                      column(width= 5,style = "height: 75%;",
                             fluidRow(style = "height: 50%; padding: 10px;",
                                  plotOutput("studentsPlot",height="100%")
                             ),
                             tags$hr(class="plot_hr"),
                             fluidRow(style = "height: 50%; padding: 10px;",
                                  plotOutput("schoolsPlot",height="100%")
                             )
                      )
                    )
                ),
                tabPanel("Facilities",
                     fluidRow(style = "height: calc(100vh - 50px - 20px -  1px);",
                         column(width = 2, 
                            checkboxGroupInput("facility_type", 
                                            h4("Facility Type Filter"), 
                                            choices = unique_facilities,
                                            selected = unique_facilities
                            )
                         ),
                         column(width = 5, style = "height: 75%;",
                                leafletOutput("facMap", width = "100%", height = "100%")),
                         column(width = 5, style = "height: 75%;",
                                div(style = "height:12.5%;"), plotOutput("facHistPlot", width = "100%", height = "75%"))
                    )
                ),
                tabPanel("Parks",
                    fluidRow(style = "height: calc(100vh - 50px - 20px -  1px);",
                         column(width = 2, 
                                checkboxGroupInput("park_type", 
                                                   h4("Park Type Filter"), 
                                                   choices = unique_parks,
                                                   selected = unique_parks
                                )
                         ),
                         column(width = 5, 
                                style = "height: 75%;",
                                leafletOutput("parksMap", width = "100%", height = "100%")
                         ),
                         column(width = 5, 
                                style = "height: 75%;",
                                div(style = "height:12.5%;"),
                                plotOutput("parksHistPlot", width = "100%", height = "75%")
                         )
                    )
                ),
                tabPanel("Code Violations",
                    fluidRow(style = "height: calc(100vh - 50px - 20px -  1px);",
                           column(width = 2, 
                           checkboxGroupInput("violation_type", 
                                                     h4("Violation Type Filter"), 
                                                     choices = unique_codes,
                                                     selected = unique_codes
                                  )
                           ),
                           column(width = 5, 
                                  style = "height: 75%;",
                                  leafletOutput("codesMap", width = "100%", height = "100%")
                           ),
                           column(width = 5, 
                                  style = "height: 75%;",
                                  div(style = "height:12.5%;"),
                                  plotOutput("linePlot", width = "100%", height = "75%")
                           )
                    )
                )
            )
                        
        )  
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    council.subset <- reactive({
        council_census_df %>% 
            filter(Council_Me %in% input$council_members) %>% 
            arrange(Num)
    })
    
    council_schools.subset <- reactive({
        council_schools_df %>% 
            filter(Council_Me %in% input$council_members) %>% 
            filter(SchoolType %in% input$school_types) %>% 
            arrange(desc(Num))
    })
    
    council.summary <- reactive({
        council.subset() %>% 
            summarise_at(vars(col_rename$target_name),sum)
        
    })

    schools.subset <- reactive({
      council_schools.subset() %>% 
        filter(SchoolType %in% input$school_types) 
    })
    
    school_color.filter <- reactive({
      if(length(input$school_types) == 0) {
        c("#5F1709","#0C2340")
      }
      else {
        if("Private" %in% input$school_types) {
            c("#0C2340","#5F1709") 
        }
        else {
            c("#5F1709","#0C2340")
        }
      }
    })
    
    output$schoolsPlot <- renderPlot({
       if(nrow(council_schools.subset()) > 0) {
          council_schools.subset() %>% 
              ggplot(aes(x=reorder(Council_Member,-as.integer(Num)),fill=SchoolType)) +
              scale_fill_manual(values = school_color.filter()) + 
              scale_y_continuous(breaks=c(1,3,5,7,9,11,13)) +
              geom_histogram(stat = "count") +
              coord_flip() +
              labs(fill="Schools Per District By Type",
                   y="Number Of Schools") +
              theme_minimal(base_size = 16) +
              theme(axis.title.y = element_blank(),
                    legend.position = "top",
                    legend.title = element_text(size=20,face="bold"))
       }
    })
    output$studentsPlot <- renderPlot({
       if(nrow(council.subset()) > 0) {
          st_drop_geometry(council.subset()) %>% 
             select(Council_Member,population,pop_lt_5:pop_15_to_17) %>% 
             gather(key="pop",value="students",-Council_Member,-population) %>% 
             #browser()
             ggplot(aes(x=pop,
                 y=students,
                 color=Council_Member,#))+
                 size=population))+
             geom_jitter(stat = 'unique',
                         height=0,
                         width=.1,
                         alpha=.6) +
             labs(size="Total Population",
                  color="Council Member",
                  x="Age Group",
                  title = "Students Per District By Age Group") +
             scale_color_manual(limits = as.character(council_census_df$Council_Member),
                                values = c("#E41A1C", "#377EB8", 
                                        "#4DAF4A", "#984EA3",
                                        "#FF7F00", "#b4b400")) +
             scale_x_discrete(limits=c("pop_lt_5","pop_5_to_9","pop_10_to_14","pop_15_to_17"),
                              labels=c("< 5", "5 - 9", "10 - 14","15 - 17")) + 
             scale_size_continuous(range = c(3,9), 
                                   limits = c(14000,19000)
                                   ) +
             guides(color = guide_legend(override.aes = list(size=5,shape=15))) +
             theme_minimal(base_size = 16) +
             theme(
                   axis.title.y = element_blank(),
                   legend.position = "left",
                   plot.title = element_text(size=20,face="bold"))
        }
    })
    
    output$schoolsMap <- renderLeaflet({
        leaflet() %>% 
            setView(lng=-86.250, lat=41.676, zoom=12 )  %>% 
            addProviderTiles(providers$Stamen.Watercolor, group = "Art") %>% 
            addPolygons(data=council.subset(),
                    layerId = ~Council_Member,
                    popup = ~districts.popup,
                    labelOptions = labelOptions(textsize = "12px"), 
                    fillColor = ~council_pal(Council_Member), 
                    color = "#444444",
                    weight = 4, smoothFactor = .5, fillOpacity = .5, opacity = 1) %>% 
            addPolygons(data=council_schools.subset(),
                   layerId = ~School,
                   popup = ~schools.popup,#label = ifelse(is.null(.$School),"",~School),
                   labelOptions = labelOptions(textsize = "12px"), 
                   group = ~SchoolType,
                   color = ~school_pal(SchoolType),
                   fillColor = ~school_pal(SchoolType),
                   weight = 4, fillOpacity = 1
            ) %>% 
            addLegend(data = council.subset(),
                    pal= council_pal,
                    values = ~Council_Member,
                    title = "Council Member",
                    position = "bottomleft", opacity = 1
            ) %>% 
            addLegend(data = council_schools.subset(),
                   pal= school_pal,
                   values = ~SchoolType,
                   title = "School Type",
                   position = "bottomleft", opacity = 1
            )
    })
    
    #####
    #Stephanies
    #####
    
    #Filters main data set to show only selected facility_Types and districts
    council_fac.subset <- reactive({
      filter(facilitiesOverDist, Council_Me %in% input$council_members) %>%
        filter(POPL_TYPE %in% input$facility_type) %>% arrange(desc(Num))#filter(POPL_TYPE %in% input$POPL_TYPE)
    })
    
    #Filters helper function so only selected districts are shown
    helper.subset <- reactive({
      filter(helper, members %in% input$council_members)
    })
    
    #Filters other helper function so only selected facility types are shown
    facilityType.subset <- reactive({
      filter(facilitiesHelper, labels %in% input$facility_type)
    })
    
    #Creates leaflet map
    output$facMap <- renderLeaflet({
      leaflet() %>%
        setView(lng=-86.250, lat=41.676, zoom=12 )  %>% 
        addProviderTiles(providers$Stamen.Watercolor, group = "Art") %>%
        addPolygons(data = districts[helper.subset()$num, ], color = "#444444", weight = 4, smoothFactor = .5, fillOpacity = .5, opacity = 1, 
                    fillColor = ~colorFactor(palette = helper.subset()$colors, domain = Num)(Num)) %>%
        
        addAwesomeMarkers(data = filter(council_fac.subset(), POPL_TYPE == "FIRE STATION"), popup = ~POPL_NAME, 
                          icon = fireIcon, group = "FIRE STATION") %>%
        addAwesomeMarkers(data = filter(council_fac.subset(), POPL_TYPE == "POLICE STATION"), popup = ~POPL_NAME, 
                          icon = policeIcon, group = "POLICE STATION") %>%
        addAwesomeMarkers(data = filter(council_fac.subset(), POPL_TYPE =="LIBRARY"), popup = ~POPL_NAME, 
                          icon = libraryIcon, group = "LIBRARY") %>%
        
        addLegend(colors = facilityType.subset()$colors, 
                  labels =  facilityType.subset()$labels, opacity = 1, position = "bottomleft") %>%
        addLegend(colors = helper.subset()$colors, 
                  labels = helper.subset()$labels1, opacity = 1, position = "bottomleft")
    })
    
    #Creates histogram plot 
    output$facHistPlot <- renderPlot({
      ggplot(data = council_fac.subset()) + 
        geom_bar(aes(x =  reorder(Council_Me, -as.integer(Num)), fill = POPL_TYPE), position = position_dodge()) + 
        theme_minimal(base_size = 16) +
        scale_fill_manual(values = facilityType.subset()$colors) + 
        scale_x_discrete(labels = rev(helper.subset()$labels2)) + 
        labs(x = "District\nNumber", y = "Number\nof Facilities", fill = "POPL_TYPE") +
        coord_flip() + 
        theme(axis.title.y = element_text(angle = 0, vjust = .5), 
              axis.text.y = element_text(color = rev(helper.subset()$colors)))
    })
    
    #####
    #Ryans
    #####
    #Filters main data set to show only selected park_Types and districts
    council_parks.subset <- reactive({
      filter(parksOverDist, Council_Me %in% input$council_members) %>%
        filter(Park_Type %in% input$park_type) %>% arrange(desc(Num))
    })
    
    #Filters helper function so only selected districts are shown
    helper.subset <- reactive({
      filter(helper, members %in% input$council_members)
    })
    
    #Filters other helper function so only selected park types are shown
    parkType.subset <- reactive({
      filter(parkHelper, labels %in% input$park_type)
    })
    
    #Creates leaflet map
    output$parksMap <- renderLeaflet({
      leaflet() %>%
        setView(lng=-86.250, lat=41.676, zoom=12 )  %>% 
        addProviderTiles(providers$Stamen.Watercolor, group = "Art") %>%
        addPolygons(data = districts[helper.subset()$num, ], color = "#444444", weight = 4, smoothFactor = .5, fillOpacity = .5, opacity = 1, 
                    fillColor = ~colorFactor(palette = helper.subset()$colors, domain = Num)(Num)) %>%
        
        addAwesomeMarkers(data = filter(council_parks.subset(), Park_Type == "Block Park"), popup = ~Park_Name, 
                          icon = blockIcon, group = "Block Parks") %>%
        addAwesomeMarkers(data = filter(council_parks.subset(), Park_Type == "Community Park"), popup = ~Park_Name, 
                          icon = communityIcon, group = "Community Parks") %>%
        addAwesomeMarkers(data = filter(council_parks.subset(), Park_Type =="Neighborhood Park"), popup = ~Park_Name, 
                          icon = neighborhoodIcon, group = "Neighborhood Parks") %>%
        
        addLegend(colors = parkType.subset()$colors, 
                  labels =  parkType.subset()$labels, opacity = 1, position = "bottomleft") %>%
        addLegend(colors = helper.subset()$colors, 
                  labels = helper.subset()$labels1, opacity = 1, position = "bottomleft")
    })
    
    #Creates histogram plot
    output$parksHistPlot <- renderPlot({
      ggplot(data = council_parks.subset()) + 
        geom_bar(aes(x =  reorder(Council_Me, -as.integer(Num)), fill = Park_Type)) + 
        theme_minimal(base_size = 16) +
        scale_fill_manual(values = parkType.subset()$colors) + 
        scale_x_discrete(labels = rev(helper.subset()$labels2)) + 
        labs(x = "District\nNumber", y = "Number\nof Parks", fill = "Park Type") +
        coord_flip() + 
        theme(axis.title.y = element_text(angle = 0, vjust = .5), 
              axis.text.y = element_text(color = rev(helper.subset()$colors)))
    })
    
    #####
    #Sarah's
    #####
    #Filters main data set to show only selected park_Types and districts
    council_codes.subset <- reactive({
      filter(codesoverdistrict, Council_Me %in% input$council_members) %>%
        filter(Case_Type_Code_Description %in% input$violation_type)
    })
    
    #Filters helper function so only selected districts are shown
    helper.subset <- reactive({
      filter(helper, members %in% input$council_members)
    })
    
    #Filters other helper function so only selected park types are shown
    codeType.subset <- reactive({
      filter(codeHelper, labels %in% input$violation_type)
    })
    
    #filters formatted code data to show selected violation types
    codes.subset <- reactive({
      filter(codes_formatted, Case_Type_Code_Description %in% input$violation_type)
    })
    
    #Create Map
    output$codesMap <-  renderLeaflet({
      leaflet() %>%
        setView(lng=-86.250, lat=41.676, zoom=12 )  %>% 
        addProviderTiles(providers$Stamen.Watercolor, group = "Art") %>%
        addPolygons(data = cc[helper.subset()$num, ], color = "#444444", weight = 4, smoothFactor = .5, fillOpacity = .5, opacity = 1, 
                    fillColor = ~colorFactor(palette = helper$colors, domain = Num)(Num)) %>%
        addCircleMarkers(data = council_codes.subset(), popup = ~Case_Type_Code_Description, 
                         color = ~pal(Case_Type_Code_Description), group = ~Case_Type_Code_Description, stroke = 0, fillOpacity = .7, radius = 4) %>%
        #addLayersControl(data = codes.subset(),
        #                 overlayGroups = ~Case_Type_Code_Description,
        #                 options = layersControlOptions(collapsed = FALSE),
        #                 position = "bottomleft") %>%
        addLegend(colors = pal(unique_codes), 
                  labels = unique_codes, opacity = 1, position="bottomleft") %>%
        addLegend(colors = helper.subset()$colors, 
                  labels = helper.subset()$labels1, opacity = 1, position = "bottomleft")
    })
    #Create Line Chart
    output$linePlot <- renderPlot({
      ggplot(data = codes.subset()) + 
        geom_line(aes(x = Case_Month, y = num_violations, color = Case_Type_Code_Description)) + 
        scale_color_manual(values = codeHelper$colors) +
        theme_minimal(base_size = 16) +
        scale_fill_manual(values = codeType.subset()$colors) + 
        scale_x_discrete(limits=1:12) + 
        labs(x = "Month", y = "Number of Violations", fill = "Violation Type") +
        scale_discrete_manual(values = c("#990000", "#663366", "#6666CC", "#0033FF", "#66fFCC", "#339900"),
                              aesthetics = c('colour','fill'),name = "Violation Type") +
        theme(axis.title.y = element_text(angle = 90, vjust = .5), 
              legend.position = c(.8,.8))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

