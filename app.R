
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
    width=4),
    mainPanel(tabsetPanel(
        tabPanel("New Cases", plotOutput("Plot1")),
        tabPanel("Deaths", plotOutput("Plot2")),
        tabPanel("Hospital Admissions", plotOutput("Plot3"))))))

server <- function(input, output){
    output$focus_area <- renderUI({selectInput("focus_area", 
                                                    "Show data for:", 
                                                   list_poss_focus_areas(input$area))})
    output$Plot1<- renderPlot(output_time_series(var="New_Cases", 
                                                 var_label = "New Cases",
                                                 xlabel="Specimen Date",
                                                 focus_area=input$focus_area,
                                                 area=input$area,
                                                 date_range=input$date_range))
    output$Plot2<- renderPlot(output_time_series(var="New_Deaths", 
                                             var_label = "New Deaths",
                                             xlabel="Death Date",
                                             focus_area=input$focus_area,
                                             area=input$area,
                                             date_range=input$date_range))
    output$Plot3<- renderPlot(output_time_series(var="New_Admissions", 
                                                 var_label = "New Hospital Admissions",
                                                 xlabel="Date",
                                                 focus_area=input$focus_area,
                                                 area=input$area,
                                                 date_range=input$date_range))}



# Run App -----------------------------------------------------------------


shiny::shinyApp(ui, server)
