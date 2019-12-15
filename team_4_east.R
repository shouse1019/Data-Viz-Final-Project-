################################################################################
# Data Visualization Final Project- Team 4 East
# 
# Team Members:
#   -Kenneth Hughes
#   -Stephanie Acuna
#   -Ryan Engel
#   -Sara House  
#
################################################################################

library(shiny)
library(rgdal)
library(sf)
library(sp)
library(leaflet)
library(ggmap)
library(tidyverse)

# Load the combined data object
load("./data/final_project.RData")

#########################################################################
# Kenneth Hughes - Starting
#########################################################################

# Define UI for application
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
    # Add custom style sheet            
    includeCSS("www/style.css"),
    
    # Application title
    navbarPage(title=tags$div(
                         tags$img(id="sb_icon",src="sb_flag_embiggened-1.png",height="30px"),
                         "South Bend, IN"),
               tabPanel("Quality of Life Indicators"),
               windowTitle = "South Bend, IN - Quality of Like Indicators"
               ),
    # Create Common Filte Panel
    fluidRow(class="container-fluid",
        wellPanel(
                checkboxGroupInput("council_members", 
                                   h4("District Council Member Filter"), 
                                   choiceNames = levels(council_census_df$Council_Member),
                                   choiceValues = unique(council_census_df$Council_Me),
                                   selected = unique(council_census_df$Council_Me),
                                   inline = TRUE
                )
        )
    ),
    # Create the Tab layouts
    fluidRow(height="100%",
        mainPanel(width=12,
            tabsetPanel(type="tabs",
                        
                #Create the Census/School input and output panels (Ken Hughes)
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
                             plotOutput("studentsPlot",height="50%",width= "100%"),
                             tags$hr(class="plot_hr"),
                             plotOutput("schoolsPlot",height="50%",width = "100%"),
                      )
                    )
                ),
                
                #Create the Facilities input and output panels (Stephanie Acuna)
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
                
                
                # Create the Parks input and output panels (Ryan Engel)
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
                
                
                #Create the Code Violations input/output panels (Sarah House)
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

# Main Server Function
server <- function(input, output) {
  
    # Filter the combined council and census data on Council Member
    council.subset <- reactive({
        council_census_df %>% 
            filter(Council_Me %in% input$council_members) %>% 
            arrange(Num)
    })

    # Filter the combined council and school data on Council Member 
    # Type of school
    council_schools.subset <- reactive({
        council_schools_df %>% 
            filter(Council_Me %in% input$council_members) %>% 
            filter(SchoolType %in% input$school_types) %>% 
            arrange(desc(Num))
    })

    # Summarize the census data to get totals    
    council.summary <- reactive({
        council.subset() %>% 
            summarise_at(vars(col_rename$target_name),sum)
        
    })

    # Filter the combined council and school data on School Type
    schools.subset <- reactive({
      council_schools.subset() %>% 
        filter(SchoolType %in% input$school_types) 
    })
    
    # Helper function to set colors on the students chart
    school_color.filter <- reactive({
      if(length(input$school_types) == 0) {
        c("#FFCF01","#0C2340")
      }
      else {
        if("Private" %in% input$school_types) {
          c("#0C2340","#FFCF01") 
        }
        else {
          c("#FFCF01","#0C2340")
        }
      }
    })
    
    # Create the schools plot
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
              theme_minimal(base_size = 12) +
              theme(axis.title.y = element_blank(),
                    legend.position = "top",
                    legend.title = element_text(size=16,face="bold"))
       }
    })
    
    # Create the student plot
    output$studentsPlot <- renderPlot({
       if(nrow(council.subset()) > 0) {
          st_drop_geometry(council.subset()) %>% 
             select(Council_Member,population,pop_lt_5:pop_15_to_17) %>% 
             gather(key="pop",value="students",-Council_Member,-population) %>% 
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
             scale_size_continuous(range = c(3,6), 
                                   limits = c(14000,19000)
                                   ) +
             guides(color = guide_legend(override.aes = list(size=2,shape=15))) +
             theme_minimal(base_size = 12) +
             theme(
                   axis.title.y = element_blank(),
                   legend.position = "left",
                   plot.title = element_text(size=16,face="bold"),
                   legend.title = element_text(face="bold"),
                   legend.box.margin=margin(c(5,5,5,5,5,"px")))
        }
    })
    
    # Create the schools map
    output$schoolsMap <- renderLeaflet({
        leaflet() %>% 
            setView(lng=-86.250, lat=41.676, zoom = 12 )  %>% 
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
                   color = "#0c2340",
                   fillColor = ~school_pal(SchoolType),
                   weight = 4, fillOpacity = 1, opacity=1
            ) %>% 
            addLegend(data = council.subset(),
                    pal= council_pal,
                    values = ~Council_Member,
                    title = "Council Member",
                    position = "bottomleft", opacity = .6
            ) %>% 
            addLegend(data = council_schools.subset(),
                   pal= school_pal,
                   values = ~SchoolType,
                   title = "School Type",
                   position = "bottomleft", opacity = 1
            )
    })
    
    #########################################################################
    # Kenneth Hughes - Ending
    #########################################################################
    
    #########################################################################
    #Stephanie Acuna - Starting
    #########################################################################
    
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
    
    #########################################################################
    # Stephanie Acuna - Ending
    #########################################################################
    
    #########################################################################
    # Ryan Engel - Starting
    #########################################################################
    
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
    
    #########################################################################
    # Ryan Engel - Ending
    #########################################################################
    
    #########################################################################
    # Sarah House - Starting
    #########################################################################
    
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
    
    #########################################################################
    # Sarah House - Ending
    #########################################################################
}

# Run the application 
shinyApp(ui = ui, server = server)


################################################################################
# Data Object Creation Code
#
################################################################################
# Kenneth Hughes Census and School Objects
################################################################################
#
# m2_to_sqmi <- 0.00000038610
# 
# proj_string <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# city_council <- st_read("./data/City_Council_Districts",
#                         stringsAsFactors = F)
# 
# city_council$council_st_area <- city_council %>% st_area()
# city_council <- city_council %>%
#   mutate(council_area_sqmi=round(as.numeric(council_st_area * m2_to_sqmi),2),
#          Council_Member=paste0(Num," (",Council_Me,")"))
# 
# city_council$Council_Member = as.factor(city_council$Council_Member)
# 
# school_boundaries <- st_read("./data/School_Boundaries", stringsAsFactors = F)
# school_boundaries$school_st_area <- school_boundaries %>% st_area()
# school_boundaries <- school_boundaries %>% mutate(school_area_sqmi=as.numeric(school_st_area * m2_to_sqmi))
# 
# census_data <- st_read("./data/2010_CensusData",stringsAsFactors = F)
# census_data <- st_transform(census_data, proj_string)
# census_data$census_st_area <- census_data %>%  st_area()
# census_data <- census_data %>% mutate(census_area_sqmi=as.numeric(census_st_area * m2_to_sqmi))
# 
# isect_council_census <- st_intersection(city_council,census_data)
# 
# isect_council_census$isect_area <- isect_council_census %>% st_area()
# 
# isect_council_census <- isect_council_census %>%
#   mutate(isect_percentage = round(as.numeric(isect_area/census_st_area),2),
#          isect_area_sqmi = round(as.numeric(isect_area * m2_to_sqmi),2)) %>%
#   filter(isect_percentage > 0.00)  %>%
#   mutate_at(vars(contains("SE_T")),~(ceiling(. * isect_percentage)))
# 
# col_rename <- read_csv("census_rename_cols.csv")
# col_rename <- col_rename %>%
#   mutate(mapping=paste0('"',target_name,'"="',source_name,'"'))
# 
# rname_col_vals <- paste(col_rename$mapping,collapse = ",")
# 
# dist_census_summary <- isect_council_census %>% group_by(Dist) %>%  summarise_at(vars(contains("SE_T"),isect_area_sqmi), sum)
# 
# dist_census_summary <- dist_census_summary %>%  rename_at(vars(starts_with("SE_T")),
#                                                           function(x) { paste0(col_rename$target_name[which(col_rename$source_name == x)])})
# 
# 
# # Join the City Council data with the census data
# council_census_df <- inner_join(city_council,
#                                 as.data.frame(dist_census_summary) %>% select(-geometry),by="Dist") %>%
#   mutate(pop_density = ceiling(population/council_area_sqmi)) %>% 
#   arrange(Dist)
# 
# council_schools_df <-  st_intersection(city_council,school_boundaries)
# council_schools_df$isect_area <- council_schools_df %>% st_area()
# 
# 
# uniq_council_members <- unique(council_census_df$Council_Me)
# uniq_schools  <- unique(council_schools_df$SchoolType)
# 
# 
# council_pal  <- colorFactor(palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#b4b400"),
#                             domain = council_census_df$Council_Members,
#                             reverse=FALSE)
# school_pal <- colorFactor(palette = c("#0C2340","#FFCF01"),
#                           domain = factor(council_schools_df$SchoolType),
#                           reverse = FALSE)
# schools.popup <- paste("School Name: <b>",council_schools_df$School,"</b><br>",
#                        "School Type: <b>",council_schools_df$SchoolType,"</b>",
#                        "<hr class='plot_hr'>Council District: <b>",council_schools_df$Num,"</b><br>",
#                        "Council Member: <b>",council_schools_df$Council_Me,"</b>",
#                        sep ="")
# 
# districts.popup <- paste("Council District: <b>",council_census_df$Num,"</b><br>",
#                          "Council Member: <b>",council_census_df$Council_Me,"</b><br>",
#                          "Total Area (Sq. Miles): <b>",council_census_df$isect_area_sqmi,"</b>",
#                          "<hr class='plot_hr'>Total Population: <b>",council_census_df$population,"</b><br>",
#                          "Pop Density: <b>",council_census_df$pop_density,"</b>",
#                          "<hr class='plot_hr'>Households: <b>",council_census_df$households,"</b><br>",
#                          "Total Housing Units: <b>",council_census_df$housing_units,"</b><br>",
#                          "Occupied Housing Units: <b>",council_census_df$housing_units_occupied,"</b><br>",
#                          sep ="")
# 
# save(schools.popup, districts.popup,uniq_schools,council_pal,school_pal,uniq_council_members,council_schools_df, council_census_df,col_rename,file="./data/kwh_final.rdata")
# load("./data/kwh_final.rdata")
#
################################################################################
# Stephanie Acuna Facilities Object Creation  
################################################################################
#
# # data
# districts <- readOGR(dsn = "./Data/City_Council_Districts",
#                      layer = "City_Council_Districts", stringsAsFactors = FALSE)  
# facilities <- read.csv("./Data/Public_Facilities.csv")
# 
# #Define helper data frames to show/hide elements to match team dashboard style
# helper <- tibble(members = c("Tim Scott", "Regina Williams", "Sharon McBride", "Jo M. Broden", "Dr. David Varner", "Oliver Davis"),
#                  num = 1:6, 
#                  colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#b4b400"),
#                  labels1 = c( "1 (Tim Scott)", "2 (Regina Williams)", "3 (Sharon McBride)", "4 (Jo M. Broden)", "5 (Dr. David Varner)", " 6 (Oliver Davis)"),
#                  labels2 = c( "1\nTim Scott", "2\nRegina Williams", "3\nSharon McBride", "4\nJo M. Broden", "5\nDr. David Varner", " 6\nOliver Davis"))
# 
# facilitiesHelper <- tibble(colors = c("red", "darkgreen", "black"), 
#                      labels =  c("FIRE STATION", "LIBRARY", "POLICE STATION"))
# 
# 
# #Create icon images for different types of facility
# #<script src="https://kit.fontawesome.com/1d678e299c.js" crossorigin="anonymous"></script> 
# 
# fireIcon <- makeAwesomeIcon(icon = 'fa-fire-extinguisher', library = 'fa', markerColor = 'red')
# policeIcon <- makeAwesomeIcon(icon = 'fa-certificate', library = 'fa', markerColor = 'black')
# libraryIcon <- makeAwesomeIcon(icon = 'fa-book', library = 'fa', markerColor = 'darkgreen')
# 
# 
# #Create a spatialPointsDataFrame object from the parks data using latitude and longitude given
# facilityPts <- facilities
# coordinates(facilityPts) <- ~ Lon + Lat
# proj4string(facilityPts) <- proj4string(districts)
# 
# #use over() function to map each facility to the district it is in, then get just the two relevent columns from that set
# #Next combine it with relevent columns from facilities data set
# facilitiesOverDist <- cbind(dplyr::select(over(facilityPts, districts), Num, Council_Me),
#                        dplyr::select(facilities, POPL_NAME, POPL_TYPE, POPL_ADDR1, Lat, Lon))
# 
# #Convert council_Members variable to a factor to get bar graph to display in order of district #
# facilitiesOverDist$Council_Me <- factor(facilitiesOverDist$Council_Me, 
#                                    levels = c("Tim Scott", "Regina Williams", "Sharon McBride", 
#                                               "Jo M. Broden", "Dr. David Varner", "Oliver Davis"))
# 
# #remove facilities that aren't in one of the given districts
# facilitiesOverDist <- na.omit(facilitiesOverDist)
# 
# #Get list of options
# unique_council_members <- levels(facilitiesOverDist$Council_Me)
# unique_facilities <- unique(facilitiesOverDist$POPL_TYPE)
# 
# 
# save(facilitiesHelper, 
#      fireIcon, 
#      policeIcon, 
#      libraryIcon, 
#      facilityPts, 
#      facilitiesOverDist, 
#      unique_facilities, file="./Data/ska_final.rdata")
# load("./data/ska_final.rdata")
#
################################################################################
# Ryan Engel Park Object Creation 
################################################################################
# #Read in data
# districts <- readOGR(dsn = "E:/Libraries/My Documents/School/grad/Sem 4/visualizations/git/Data-Viz-2019-Fall/FinalProject/City_Council_Districts",
#                      layer = "City_Council_Districts", stringsAsFactors = FALSE)  
# parks <- read.csv("E:/Libraries/My Documents/School/grad/Sem 4/visualizations/git/Data-Viz-2019-Fall/FinalProject/Parks_Locations_and_Features.csv")
# 
# 
# #Define helper data frames to show/hide elements
# helper <- tibble(members = c("Tim Scott", "Regina Williams", "Sharon McBride", "Jo M. Broden", "Dr. David Varner", "Oliver Davis"),
#                  num = 1:6, 
#                  colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#b4b400"),
#                  labels1 = c( "1 (Tim Scott)", "2 (Regina Williams)", "3 (Sharon McBride)", "4 (Jo M. Broden)", "5 (Dr. David Varner)", " 6 (Oliver Davis)"),
#                  labels2 = c( "1\nTim Scott", "2\nRegina Williams", "3\nSharon McBride", "4\nJo M. Broden", "5\nDr. David Varner", " 6\nOliver Davis"))
# 
# parkHelper <- tibble(colors = c("#3f5666", "#a9eb59", "#536131"), 
#                      labels =  c("Block Park", "Community Park", "Neighborhood Park"))
# 
# 
# #Create icon images with tree icons in different colors to represent different park types
# blockIcon <- makeAwesomeIcon(icon = 'tree', library = 'fa', markerColor = 'cadetblue')
# communityIcon <- makeAwesomeIcon(icon = 'tree', library = 'fa', markerColor = 'lightgreen')
# neighborhoodIcon <- makeAwesomeIcon(icon = 'tree', library = 'fa', markerColor = 'darkgreen')
# 
# 
# #Filter to what most ppl consider "parks" (example: remove cemetaries from dataset)
# parks <- filter(parks, (Park_Type == "Block Park") | (Park_Type == "Community Park") | (Park_Type == "Neighborhood Park"))
# 
# 
# #Create a spatialPointsDataFrame object from the parks data using latitude and longitude given
# parkPts <- parks
# coordinates(parkPts) <- ~ Lon + Lat
# proj4string(parkPts) <- proj4string(districts)
# 
# 
# #use over() function to map each park to the district it is in, then get just the two relevent columns from that set
# #Next combine it with relevent columns from parks data set
# parksOverDist <- cbind(dplyr::select(over(parkPts, districts), Num, Council_Me),
#                        dplyr::select(parks, Park_Name, Park_Type, Address, Lat, Lon))
# 
# 
# #Convert council_Members variable to a factor to get bar graph to display in order
# parksOverDist$Council_Me <- factor(parksOverDist$Council_Me, 
#                                    levels = c("Tim Scott", "Regina Williams", "Sharon McBride", 
#                                               "Jo M. Broden", "Dr. David Varner", "Oliver Davis"))
# 
# 
# #remove parks that aren't in one of the given districts
# parksOverDist <- na.omit(parksOverDist)
# 
# 
# #Get list of options
# unique_council_members <- levels(parksOverDist$Council_Me)
# unique_parks <- unique(parksOverDist$Park_Type)
#load("./data/final_project.RData")
#
#
################################################################################
# Sarah House Code Enforcement Dashboard
################################################################################
#
#Read in Data
#setwd('~/Desktop/City_Council_Districts')
# cc <- readOGR(dsn = ".", layer = 'City_Council_Districts', stringsAsFactors = FALSE)
# codes <- read.csv('Code Enforcment Cases.csv')
# codes <- sample_n(codes, 1000)
# #format smaller codes dataframe for line graph
# codes_formatted <- codes %>%
#   group_by(Case_Month, Case_Type_Code_Description) %>%
#   summarize(num_violations = n())
# 
# #Format non spatial data to become spatial data from joining.
# codes_geo <- codes
# coordinates(codes_geo) <- ~Lon + Lat
# proj4string(codes_geo) <- proj4string(cc)
# 
# #Join datasets to get the city council members and districts with the code violations. 
# codesoverdistrict <- cbind(dplyr::select(over(codes_geo, cc), Num, Council_Me),
#                            dplyr::select(codes, Case_Month, Case_Number, Case_Type_Code_Description, Street_Address, State_Code, Zip_Code, City, Lon, Lat))
# 
# codesoverdistrict$Council_Me <- factor(codesoverdistrict$Council_Me, 
#                                        levels = c("Tim Scott", "Regina Williams", "Sharon McBride", 
#                                                   "Jo M. Broden", "Dr. David Varner", "Oliver Davis"))
# 
# #define unique council members and codes for filters
# unique_council_members <- unique(codesoverdistrict$Council_Me)
# 
# codesoverdistrict <- codesoverdistrict %>% arrange(Case_Type_Code_Description)
# unique_codes <- unique(codesoverdistrict$Case_Type_Code_Description)
# 
# #define helper functions
# pal <- colorFactor(palette = c("#990000", "#663366", "#6666CC", "#0033FF", "#66fFCC", "#339900"), domain = unique_codes)
# helper <- tibble(members = c("Tim Scott", "Regina Williams", "Sharon McBride", "Jo M. Broden", "Dr. David Varner", "Oliver Davis"),
#                  num = 1:6, 
#                  colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#b4b400"),
#                  labels1 = c( "1 (Tim Scott)", "2 (Regina Williams)", "3 (Sharon McBride)", "4 (Jo M. Broden)", "5 (Dr. David Varner)", " 6 (Oliver Davis)"),
#                  labels2 = c( "1\nTim Scott", "2\nRegina Williams", "3\nSharon McBride", "4\nJo M. Broden", "5\nDr. David Varner", " 6\nOliver Davis"))
# 
# codeHelper <- tibble(colors = c("#990000", "#663366", "#6666CC", "#0033FF", "#66fFCC", "#339900"), 
#                      labels =  c("ZONING VIOLATIONS", "VEHICLE-PRIVATE", "ENVIRONMENTAL MOWING", "HOUSING REPAIR", "VEHICLE-PUBLIC", "ENVIRONMENTAL CLEANUP"))
# 
################################################################################
#  Combining and creating a new RDATA file.
################################################################################
#
#library(rgdal)
#library(sf)
#library(leaflet)
#
#load("./data/sh_final.rdata")
#load("./data/kwh_final.rdata")
#load("./data/RyansPart.RData")
#load("./data/ska_final.rdata")
#
#pal <- colorFactor(palette = c("#990000", "#663366", "#6666CC", "#0033FF", "#66fFCC", "#339900"), domain = unique_codes)
#save(schools.popup,districts.popup,uniq_schools,unique_council_members_parks, unique_parks, blockIcon, communityIcon, neighborhoodIcon,pal,cc,codeHelper,codes,codes_formatted,codes_geo,codesoverdistrict,col_rename,council_census_df,council_pal,council_schools_df,districts,facilitiesHelper,facilitiesOverDist,facilityPts,fireIcon,helper,libraryIcon,parkHelper,parksOverDist,policeIcon,uniq_council_members,unique_codes,unique_council_members,unique_facilities,council_pal,school_pal,file="./data/final_project.RData")
#load("./data/final_project.RData")

