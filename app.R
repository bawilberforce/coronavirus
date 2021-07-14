
# Load Packages & Functions -----------------------------------------------------------

source("functions.R")



# Create Shiny App --------------------------------------------------------

ui<-fluidPage(mainPanel(
    titlePanel("UK Coronavirus Data"),
    sidebarPanel(
    dateRangeInput("date_range", "Please select a date range:",
                              "2020-03-01", Sys.Date(),
                              max=Sys.Date(),min="2020-02-01"),
    selectInput("area", "Plot data by:",
                           choices = c("nation","region")), 
    uiOutput("focus_area"),
    width=3),
    mainPanel(tabsetPanel(
        tabPanel("New Cases", plotOutput("Plot1")),
                 tabPanel("Long Covid Cases",""),
                 tabPanel("Deaths",""))),width = 1000, height=500))

server <- function(input, output){
    output$focus_area <- renderUI({selectInput("focus_area", 
                                                    "Show data for:", 
                                                   list_poss_focus_areas(input$area))})
    output$Plot1<- renderPlot({validate(need(input$focus_area!="" &
                                        input$focus_area %in% sapply(list_poss_focus_areas(input$area), 
                                                                     function(level){as.character(level)}) == TRUE, 
                                             ""))
        get_cases_data(area=input$area)%>%
        convert_to_json()%>%
        calculate_rolling_avg()%>%
        create_graph(.,focus_area=paste0(input$focus_area), 
                     start_date=min(input$date_range),
                    end_date=max(input$date_range))})}


# Run App -----------------------------------------------------------------


shiny::shinyApp(ui, server)
